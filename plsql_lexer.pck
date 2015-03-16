create or replace package plsql_lexer is
/*
## Purpose ##

Tokenize a SQL or PL/SQL statement.  This package is intended for use with
Statement Classifier.

Tokens may be one of these:
	whitespace               - 0,9,10,11,12,13,32,and \3000 (ideographic space)
	comment                  - Single and multiline, does not include newline at end of the single lin comment.
	text                     - Includes quotation marks, alternative quote delimiters, and "N"
	numeric                  - Everything but initial + or -: ^([0-9]+\.[0-9]+|\.[0-9]+|[0-9]+)((e|E)(\+|-)?[0-9]+)?(f|F|d|D)?
	symbol                   - May be an identifier, operator, or condition - the parser must distinguish them
	~!@%^*()-+=[]|:;<,>./    - All SQL and PL/SQL special characters
	unexpected               - Everything else.  For example "&", a SQL*Plus characters.  (Should this be included above?)

## Requirements ##

- EBCDIC is not supported.
- Only 11gR2 and above are supported.


## Example ##

begin
	dbms_output.put_line(plsql_lexer.print_tokens(plsql_lexer.tokenize(
		'select * from dual;'
	)));
end;

*/

function tokenize(p_source nclob) return token_table;
function print_tokens(p_tokens token_table) return nclob;

end;
/
create or replace package body plsql_lexer is

--Globals

--A logical character may contain 2 physical characters (or code points).
--For example, the 4-byte unicode character unistr('\D841\DF79') is counted as
--2 characters when it's in a NCLOB.
type char_table is table of nvarchar2(2 char);
g_chars char_table := char_table();

g_last_char nvarchar2(2 char);
g_last_char_position number;
g_token_text nclob;


--------------------------------------------------------------------------------
--Create a nested table of characters.
--This extra step takes care of non-trivial unicode processing up front.
--This cannot be simplified with SUBSTRC, that will not work for large CLOBs.
--TODO: Is there an easier way to do this?
procedure set_g_chars(p_source in nclob) is
	v_position number := 1;
	v_clob_length number := nvl(dbms_lob.getlength(p_source), 0);
	v_char nvarchar2(2 char);

	v_amount_of_lob_data_0 exception;
	pragma exception_init(v_amount_of_lob_data_0, -22923);

	v_offset_not_on_char_boundary exception;
	pragma exception_init(v_offset_not_on_char_boundary, -22831);
begin
	--Remove any old characters.
	g_chars.delete;

	--Loop through each code point, which is not necessarily one character.
	loop
		exit when v_position > v_clob_length;

		--When starting from the beginning (offset = 1), substr will warn if 0 characters are read.
		--This means only 2 of the 4 bytes were retrieved and we need to increase the amount.
		if v_position = 1 then
			begin
				v_char := dbms_lob.substr(lob_loc => p_source, amount => 1, offset => v_position);
				v_position := v_position + 1;
			exception when v_amount_of_lob_data_0 then
				v_char := dbms_lob.substr(lob_loc => p_source, amount => 2, offset => v_position);
				v_position := v_position + 2;
			end;
		--When starting anywhere else in the CLOB, Oracle will only throw an error if you *start*
		--from a bad position and not if you *stop* in a bad position.
		--To detect if the next character is 1 or 2 code points, first read starting one character
		--ahead.  If it works, get one code point, if it fails, get two.
		else
			begin
				--Jump ahead to next position just to see if it throws an error.
				v_char := dbms_lob.substr(lob_loc => p_source, amount => 1, offset => v_position + 1);
				--If it didn't, then grab one code point.
				v_char := dbms_lob.substr(lob_loc => p_source, amount => 1, offset => v_position);
				v_position := v_position + 1;
			--If there's an exception, grab 2 code points.
			exception when v_offset_not_on_char_boundary then
				v_char := dbms_lob.substr(lob_loc => p_source, amount => 2, offset => v_position);
				v_position := v_position + 2;
			end;
		end if;

		g_chars.extend();
		g_chars(g_chars.count) := v_char;

	end loop;
end set_g_chars;


--------------------------------------------------------------------------------
--Get and consume one character.
function get_char return nvarchar2 is
begin
	g_last_char_position := g_last_char_position + 1;

	if g_last_char_position > g_chars.count then
		return null;
	else
		return g_chars(g_last_char_position);
	end if;
end;


--------------------------------------------------------------------------------
--Get but do not consume next character.
function look_ahead(p_offset number) return nvarchar2 is
begin
	if g_last_char_position + p_offset > g_chars.count then
		return null;
	else
		return g_chars(g_last_char_position + p_offset);
	end if;
end look_ahead;


--------------------------------------------------------------------------------
--From the current position, return a string that contains all possibly numeric
--characters.  The real parsing will be done by a regular expression, but we
--can at least filter out anything that's not one of 0-9,+,-,.,e,E,f,F,d,D
--'^([0-9]+\.[0-9]+|\.[0-9]+|[0-9]+)((e|E)(\+|-)?[0-9]+)?(f|F|d|D)?');
function get_potential_numeric_string return nvarchar2 is
	v_string nvarchar2(32767);
	v_numeric_position number := g_last_char_position;
begin
	loop
		exit when v_numeric_position > g_chars.count;
		exit when g_chars(v_numeric_position) not in
			(
				'0','1','2','3','4','5','6','7','8','9',
				'+','-','.','e','E','f','F','d','D'
			);

		v_string := v_string || g_chars(v_numeric_position);
		v_numeric_position := v_numeric_position + 1;
	end loop;

	return v_string;
end get_potential_numeric_string;


--------------------------------------------------------------------------------
--Is the character white space.
function is_whitespace(p_char nvarchar2) return boolean is
begin
	/*
	--Find single-byte whitespaces.
	--ASSUMPTION: There are no 3 or 4 byte white space characters.
	declare
		c1 varchar2(1); c2 varchar2(1); c3 varchar2(1); c4 varchar2(1);
		v_string varchar2(10);
		v_throwaway number;
	begin
		for n1 in 0..15 loop c1 := trim(to_char(n1, 'XX'));
		for n2 in 0..15 loop c2 := trim(to_char(n2, 'XX'));
		for n3 in 0..15 loop c3 := trim(to_char(n3, 'XX'));
		for n4 in 0..15 loop c4 := trim(to_char(n4, 'XX'));
			v_string := unistr('\'||c1||c2||c3||c4);
			begin
				execute immediate 'select 1 a '||v_string||' from dual' into v_throwaway;
				dbms_output.put_line('Whitespace character: \'||c1||c2||c3||c4);
			exception when others then null;
			end;
		end loop; end loop; end loop; end loop;
	end;
	*/

	--These results are not the same as the regular expression "\s".
	--There are dozens of Unicode white space characters, but only these
	--are considered whitespace in PL/SQL or SQL.
	if p_char in
	(
		unistr('\0000'),unistr('\0009'),unistr('\000A'),unistr('\000B'),
		unistr('\000C'),unistr('\000D'),unistr('\0020'),unistr('\3000')
	) then
		return true;
	else
		return false;
	end if;
end is_whitespace;


--------------------------------------------------------------------------------
--Is the character alphabetic, in any language.
function is_alpha(p_char nvarchar2) return boolean is
begin
	return regexp_like(p_char, '[[:alpha:]]');
end is_alpha;


--------------------------------------------------------------------------------
--Is the character alphabetic (in any language), numeric, or one of "_", "#", or "$".
function is_alpha_numeric_or__#$(p_char nvarchar2) return boolean is
begin
	return regexp_like(p_char, '[[:alpha:]]|[0-9]|\_|#|\$');
end is_alpha_numeric_or__#$;


--------------------------------------------------------------------------------
--Create a symbol token and potentially throw one of these two errors:
--ORA-00972: identifier is too long
--ORA-01741: illegal zero-length identifier
function create_symbol_check_length(p_token_text nclob) return token is
	v_token_string varchar2(4000);
begin
	--Don't bother converting NCLOB if it's obviously too long.
	if dbms_lob.getlength(p_token_text) >= 200 then
		return token('symbol', p_token_text, -972, 'identifier is too long');
	--Else convert to VARCHAR2, return error if it's > 30 bytes (excluding quotation marks).
	else
		--Grab much more than necessary to ensure we don't land in the middle of a 2-code point character.
		v_token_string := dbms_lob.substr(lob_loc => p_token_text, amount => 1000);

		--Look for empty quoted identifier.
		if v_token_string = '""' then
			return token('symbol', p_token_text, -1741, 'illegal zero-length identifier');
		--Look for identifer is too long.
		elsif lengthb(replace(v_token_string, '"', null)) > 30 then
			return token('symbol', p_token_text, -972, 'identifier is too long');
		else
			return token('symbol', p_token_text, null, null);
		end if;
	end if;
end create_symbol_check_length;


--------------------------------------------------------------------------------
--Return the next token from a string.
--Type is one of: EOF, whitespace, comment, text, numeric, symbol, or special charcters.
--See the package specification for some information on the lexer.
function get_token return token is
	v_quote_delimiter nvarchar2(1 char);
begin
	--Out of characters.
	if g_last_char_position > g_chars.count or g_chars.count = 0 then
		return token('EOF', null, null, null);
	end if;

	--Load first character.
	if g_last_char_position = 0 then
		g_last_char := get_char;
	end if;

	--Whitespace - don't throw it out, it may contain a hint or help with pretty printing.
	if is_whitespace(g_last_char) then
		g_token_text := g_last_char;
		loop
			g_last_char := get_char;
			exit when not is_whitespace(g_last_char);
			g_token_text := g_token_text || g_last_char;
		end loop;
		return token('whitespace', g_token_text, null, null);
	end if;

	--Single line comment.
	if g_last_char = '-' and look_ahead(1) = '-' then
		g_token_text := g_last_char || get_char;
		loop
			g_last_char := get_char;
			--chr(13) by itself does not count.
			exit when g_last_char = chr(10) or g_last_char is null;
			g_token_text := g_token_text || g_last_char;
		end loop;
		return token('comment', g_token_text, null, null);
	end if;

	--Multi-line comment.
	if g_last_char = '/' and look_ahead(1) = '*' then
		g_token_text := g_last_char || get_char;
		loop
			g_last_char := get_char;
			if g_last_char = '*' and look_ahead(1) = '/' then
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				exit;
			end if;
			if look_ahead(1) is null then
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				return token('comment', g_token_text, -01742, 'comment not terminated properly');
			end if;
			g_token_text := g_token_text || g_last_char;
		end loop;
		return token('comment', g_token_text, null, null);
	end if;

	--Text.
	if g_last_char = '''' then
		g_token_text := g_last_char;
		loop
			g_last_char := get_char;
			--Ignore escaped strings.
			if g_last_char = '''' and look_ahead(1) = '''' then
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
			elsif g_last_char = '''' and (look_ahead(1) is null or look_ahead(1) <> '''') then
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				exit;
			elsif look_ahead(1) is null then
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				return token('text', g_token_text, -01756, 'quoted string not properly terminated');
			end if;
			g_token_text := g_token_text || g_last_char;
		end loop;
		return token('text', g_token_text, null, null);
	end if;

	--Nvarchar text.
	if lower(g_last_char) = 'n' and look_ahead(1) = '''' then
		--Consume 2 characters: n and the quote.
		g_token_text := g_last_char;
		g_last_char := get_char;
		g_token_text := g_token_text||g_last_char;
		loop
			g_last_char := get_char;
			--Ignore escaped strings.
			if g_last_char = '''' and look_ahead(1) = '''' then
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
			elsif g_last_char = '''' and (look_ahead(1) is null or look_ahead(1) <> '''') then
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				exit;
			elsif look_ahead(1) is null then
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				return token('text', g_token_text, -01756, 'quoted string not properly terminated');
			end if;
			g_token_text := g_token_text || g_last_char;
		end loop;
		return token('text', g_token_text, null, null);
	end if;

	--Alternative quoting mechanism.
	if lower(g_last_char) = 'q' and look_ahead(1) = '''' then
		--Consume 3 characters: q, quote, and the quote delimiter.
		g_token_text := g_last_char;
		g_last_char := get_char;
		g_token_text := g_token_text||g_last_char;
		g_last_char := get_char;
		g_token_text := g_token_text||g_last_char;
		--The ending delimiter is different in a few cases.
		v_quote_delimiter := case g_last_char
			when '[' then ']'
			when '{' then '}'
			when '<' then '>'
			when '(' then ')'
			else g_last_char
		end;

		loop
			g_last_char := get_char;
			if g_last_char = v_quote_delimiter and look_ahead(1) = '''' then
				--"Alternative quotes (q'#...#') cannot use spaces, tabs, or carriage returns as delimiters".
				--(The error says carriage return, but testing indicates they really mean newlines)
				if g_last_char in (chr(9), chr(10), chr(32)) then
					g_token_text := g_token_text || g_last_char;
					g_last_char := get_char;
					g_token_text := g_token_text || g_last_char;
					g_last_char := get_char;
					return token('text', g_token_text, -00911, 'invalid character');
				end if;

				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				exit;
			end if;
			if look_ahead(1) is null then
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				return token('text', g_token_text, -01756, 'quoted string not properly terminated');
			end if;
			g_token_text := g_token_text || g_last_char;
		end loop;
		return token('text', g_token_text, null, null);
	end if;

	--Nvarchar alternative quoting mechanism.
	if lower(g_last_char) = 'n' and lower(look_ahead(1)) = 'q' and look_ahead(2) = '''' then
		--Consume 4 characters: n, q, quote, and the quote delimiter.
		g_token_text := g_last_char;
		g_last_char := get_char;
		g_token_text := g_token_text||g_last_char;
		g_last_char := get_char;
		g_token_text := g_token_text||g_last_char;
		g_last_char := get_char;
		g_token_text := g_token_text||g_last_char;
		--The ending delimiter is different in a few cases.
		v_quote_delimiter := case g_last_char
			when '[' then ']'
			when '{' then '}'
			when '<' then '>'
			when '(' then ')'
			else g_last_char
		end;

		loop
			g_last_char := get_char;
			if g_last_char = v_quote_delimiter and look_ahead(1) = '''' then
				--"Alternative quotes (q'#...#') cannot use spaces, tabs, or carriage returns as delimiters".
				--(The error says carriage return, but also includes newlines)
				if g_last_char in (chr(9), chr(10), chr(32)) then
					g_token_text := g_token_text || g_last_char;
					g_last_char := get_char;
					g_token_text := g_token_text || g_last_char;
					g_last_char := get_char;
					return token('text', g_token_text, -00911, 'invalid character');
				end if;

				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				exit;
			end if;
			if look_ahead(1) is null then
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				return token('text', g_token_text, -01756, 'quoted string not properly terminated');
			end if;
			g_token_text := g_token_text || g_last_char;
		end loop;
		return token('text', g_token_text, null, null);
	end if;

	--Numeric.
	--This follows the BNF diagram, except it doesn't include leading + or -.
	--And note that the diagram incorrectly implies '3+3' is a number,
	--the (E|e)?(+|-)? is incorrect.
	--http://docs.oracle.com/database/121/SQLRF/img/number.gif
	if g_last_char between '0' and '9' or (g_last_char = '.' and look_ahead(1) between '0' and '9') then
		declare
			v_substring nvarchar2(32767) := get_potential_numeric_string();
		begin
			--Note: Combining classes, anchors, and regexp_substr positions other than 1 do not always work.
			--Note: This won't work with numbers larger than 1K characters,
			--a ridiculous number that would cause a runtime error, but is theoretically valid.
			g_token_text := regexp_substr(
				v_substring,
				'^([0-9]+\.[0-9]+|\.[0-9]+|[0-9]+)((e|E)(\+|-)?[0-9]+)?(f|F|d|D)?');
		end;

		--Advance the characters.
		--Regular "length" is fine here since numerics cannot be more than one code point.
		g_last_char_position := g_last_char_position + length(g_token_text) - 1;
		g_last_char := get_char;
		return token('numeric', g_token_text, null, null);
	end if;

	--Symbol - quoted identifier.  Note that quoted identifers are not escaped.
	if g_last_char = '"' then
		g_token_text := g_last_char;
		loop
			g_last_char := get_char;
			if g_last_char = '"'then
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				exit;
			end if;
			if look_ahead(1) is null then
				g_token_text := g_token_text || g_last_char;
				g_last_char := get_char;
				return token('symbol', g_token_text, -01740, 'missing double quote in identifier');
			end if;
			g_token_text := g_token_text || g_last_char;
		end loop;
		return create_symbol_check_length(g_token_text);
	end if;

	--Symbol.
	--Starts with alpha (in any language!), may contain number, "_", "$", and "#".
	if is_alpha(g_last_char) then
		g_token_text := g_last_char;
		loop
			g_last_char := get_char;
			if g_last_char is null or not is_alpha_numeric_or__#$(g_last_char) then
				exit;
			end if;
			g_token_text := g_token_text || g_last_char;
		end loop;
		return create_symbol_check_length(g_token_text);
	end if;

	--Special characters used for operators. starting from top-left of keyboard.
	--Igore the IBM "not" character - it's in the manual but is only supported
	--on obsolete platforms: http://stackoverflow.com/q/9305925/409172
	if g_last_char in ('~','!','@','%','^','*','(',')','-','+','=','[',']','|',':',';','<',',','>','.','/') then
		g_token_text := g_last_char;
		g_last_char := get_char;
		return token(g_token_text, g_token_text, null, null);
	end if;

	--Unexpected - everything else.
	g_token_text := g_last_char;
	g_last_char := get_char;
	return token('unexpected', g_token_text, null, null);
end get_token;


--------------------------------------------------------------------------------
--Convert a string into a VARRAY of tokens.
function tokenize(p_source nclob) return token_table is
	v_token token;
	v_tokens token_table := token_table();
begin
	--Set some globals.
	set_g_chars(p_source);
	g_last_char_position := 0;

	--Get all the tokens.
	loop
		v_token := get_token;
		v_tokens.extend;
		v_tokens(v_tokens.count) := v_token;
		exit when v_token.type = 'EOF';
	end loop;

	--Return them.
	return v_tokens;
end tokenize;


--------------------------------------------------------------------------------
--Print tokens for debugging.
function print_tokens(p_tokens token_table) return nclob is
	v_output nclob;
begin
	for i in 1 .. p_tokens.count loop
		v_output := v_output||' '||p_tokens(i).type;
	end loop;

	return substr(v_output, 2);
end print_tokens;


end;
/
