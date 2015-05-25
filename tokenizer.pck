create or replace package tokenizer is
--Copyright (C) 2015 Jon Heller.  This program is licensed under the LGPLv3.
C_VERSION constant varchar2(10) := '0.2.0';


/*
## Purpose ##

Tokenize a SQL or PL/SQL statement.

Tokens may be one of these types:
    whitespace
        Characters 0,9,10,11,12,13,32,and unistr('\3000') (ideographic space)
    comment
        Single and multiline.  Does not include newline at end of the single line comment
    text
        Includes quotation marks, alternative quote delimiters, "Q", and "N"
    numeric
        Everything but initial + or -: ^([0-9]+\.[0-9]+|\.[0-9]+|[0-9]+)((e|E)(\+|-)?[0-9]+)?(f|F|d|D)?
    word
        May be a keyword, identifier, or (alphabetic) operator.
        The parser must distinguish between them because keywords are frequently not reserved.
    inquiry_directive
        PL/SQL preprocessor (conditional compilation) feature that is like: $$name
    preprocessor_control_token
		PL/SQL preprocessor (conditional compilation) feature that is like: $plsql_identifier
    ,}?
        3-character punctuation operators (Row Pattern Quantifier).
    ~= != ^= <> := => >= <= ** || << >> {- -} *? +? ?? ,} }? {,
        2-character punctuation operators.
    @ $ % ^ * ( ) - + = [ ] { } | : ; < , > . / ?
        1-character punctuation operators.
    EOF
        End of File.
    unexpected
        Everything else.  For example "&", a SQL*Plus characters.


## Output ##

The most important output is a Token type:
	type     varchar2(4000),
	value    nclob,
	sqlcode  number,
	sqlerrm  varchar2(4000)

- type: One of the names listed in the Purpose section above.
- value: The exact string value.
- sqlcode: The SQLCODE for a serious error that prevents effective lexing.
	For example, this could be "ORA-1756: quoted string not properly terminated".
- sqlerrm: The SQLERRM that goes with the SQLCODE above.


## Requirements ##

- Only 11gR2 and above are supported.  But this will likely work well in lower versions.
- EBCDIC character set is not supported.


## Example ##

begin
	dbms_output.put_line(tokenizer.print_tokens(tokenizer.tokenize(
		'select * from dual;'
	)));
end;

Results:  word whitespace * whitespace word whitespace word ; EOF

*/

--Main functions.
function tokenize(p_source nclob) return token_table;
function print_tokens(p_tokens token_table) return nclob;

--Helper functions useful for some tools.
function is_lexical_whitespace(p_char nvarchar2) return boolean;
function get_nvarchar2_table_from_nclob(p_nclob nclob) return nvarchar2_table;

end;
/
create or replace package body tokenizer is

--Globals

--A logical character may contain 2 physical characters (or code points).
--For example, the 4-byte unicode character unistr('\D841\DF79') is counted as
--2 characters when it's in a NCLOB.
g_chars nvarchar2_table := nvarchar2_table();

g_last_char nvarchar2(2 char);
g_last_char_position number;
g_token_text nclob;

--Last non-whitespace, non-comment token.
g_last_concrete_token token;
--Track when we're inside a MATCH_RECOGNIZE and a PATTERN to disambiguate "$".
--"$" is a pattern row when inside, else it could be for conditional compilation
--or an identifier name.  
g_match_recognize_paren_count number;
g_pattern_paren_count number;


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
--Is the character alphabetic (in any language), numeric, or one of "_", or "#".
function is_alpha_numeric_or__#(p_char nvarchar2) return boolean is
begin
	return regexp_like(p_char, '[[:alpha:]]|[0-9]|\_|#');
end is_alpha_numeric_or__#;


--------------------------------------------------------------------------------
--Track tokens to detect if inside a row pattern matching.
--Row pattern matching introduces some ambiguity because the regular-expression
--syntax conflicts with "$", "**", and "||".
procedure track_row_pattern_matching(p_token token) is
begin
	--Start counters.
	if p_token.type = '('
	and g_last_concrete_token.type = 'word'
	and lower(g_last_concrete_token.value) = 'pattern'
	and g_match_recognize_paren_count > 0
	and g_pattern_paren_count = 0 then
		g_pattern_paren_count := 1;
	elsif p_token.type = '('
	and g_last_concrete_token.type = 'word'
	and lower(g_last_concrete_token.value) = 'match_recognize'
	and g_match_recognize_paren_count = 0 then
		g_match_recognize_paren_count := 1;
	--Increment or decrement parentheses counters.
	elsif g_pattern_paren_count > 0 and p_token.type = '(' then
		g_pattern_paren_count := g_pattern_paren_count + 1;
	elsif g_pattern_paren_count > 0 and p_token.type = ')' then
		g_pattern_paren_count := g_pattern_paren_count - 1;
	elsif g_match_recognize_paren_count > 0 and p_token.type = '(' then
		g_match_recognize_paren_count := g_match_recognize_paren_count + 1;
	elsif g_match_recognize_paren_count > 0 and p_token.type = ')' then
		g_match_recognize_paren_count := g_match_recognize_paren_count - 1;
	end if;
end track_row_pattern_matching;


--------------------------------------------------------------------------------
--Return the next token from a string.
--Type is one of: EOF, whitespace, comment, text, numeric, word, or special charcters.
--See the package specification for some information on the tokenizer.
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
	if is_lexical_whitespace(g_last_char) then
		g_token_text := g_last_char;
		loop
			g_last_char := get_char;
			exit when not is_lexical_whitespace(g_last_char);
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

	--Word - quoted identifier.  Note that quoted identifers are not escaped.
	--Do *not* check for these errors in words:
	--"ORA-00972: identifier is too long" or "ORA-01741: illegal zero-length identifier".
	--Database links have different rules, like 128 bytes instead of 30, and we
	--won't know if it's a database link name until parse time.
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
				return token('word', g_token_text, -01740, 'missing double quote in identifier');
			end if;
			g_token_text := g_token_text || g_last_char;
		end loop;
		return token('word', g_token_text, null, null);
	end if;

	--Word.
	--Starts with alpha (in any language!), may contain number, "_", "$", and "#".
	if is_alpha(g_last_char) then
		g_token_text := g_last_char;
		loop
			g_last_char := get_char;

			--"$" does not count as part of the word when inside a row pattern match.
			if g_pattern_paren_count > 0 then
				if g_last_char is null or not is_alpha_numeric_or__#(g_last_char) then
					exit;
				end if;
			else
				if g_last_char is null or not is_alpha_numeric_or__#$(g_last_char) then
					exit;
				end if;
			end if;

			g_token_text := g_token_text || g_last_char;
		end loop;
		return token('word', g_token_text, null, null);
	end if;

	--Inquiry Directive.
	--Starts with $$ alpha (in any language!), may contain number, "_", "$", and "#".
	if g_last_char = '$' and look_ahead(1) = '$' and is_alpha(look_ahead(2)) then
		g_token_text := g_last_char || get_char;
		g_token_text := g_token_text || get_char;
		loop
			g_last_char := get_char;
			if g_last_char is null or not is_alpha_numeric_or__#$(g_last_char) then
				exit;
			end if;
			g_token_text := g_token_text || g_last_char;
		end loop;
		return token('inquiry_directive', g_token_text, null, null);
	end if;

	--Inquiry Directive.
	--Starts with $ alpha (in any language!), may contain number, "_", "$", and "#".
	if g_last_char = '$' and is_alpha(look_ahead(1)) then
		g_token_text := g_last_char || get_char;
		loop
			g_last_char := get_char;
			if g_last_char is null or not is_alpha_numeric_or__#$(g_last_char) then
				exit;
			end if;
			g_token_text := g_token_text || g_last_char;
		end loop;
		return token('preprocessor_control_token', g_token_text, null, null);
	end if;

	--3-character punctuation operators.
	--12c Row Pattern Quantifiers introduced a lot of regular-expression operators.
	if g_last_char||look_ahead(1)||look_ahead(2) in (',}?') then
		g_token_text := g_last_char || get_char || get_char;
		g_last_char := get_char;
		return token(g_token_text, g_token_text, null, null);
	end if;

	--2-character punctuation operators.
	--Igore the IBM "not" character - it's in the manual but is only supported
	--on obsolete platforms: http://stackoverflow.com/q/9305925/409172
	if g_last_char||look_ahead(1) in ('~=','!=','^=','<>',':=','=>','>=','<=','<<','>>','{-','-}','*?','+?','??',',}','}?','{,') then
		g_token_text := g_last_char || get_char;
		g_last_char := get_char;
		return token(g_token_text, g_token_text, null, null);
	end if;

	if g_last_char||look_ahead(1) in ('**','||') and g_pattern_paren_count = 0 then
		g_token_text := g_last_char || get_char;
		g_last_char := get_char;
		return token(g_token_text, g_token_text, null, null);
	end if;

	--1-character punctuation operators.
	if g_last_char in ('@','%','^','*','(',')','-','+','=','[',']','{','}','|',':',';','<',',','>','.','/','?') then
		g_token_text := g_last_char;
		g_last_char := get_char;
		return token(g_token_text, g_token_text, null, null);
	end if;

	--"$" only counts as "$" inside row pattern matching.
	if g_last_char = '$' and g_pattern_paren_count > 0 then
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
	--Initialize globals.
	g_chars := get_nvarchar2_table_from_nclob(p_source);
	--set_g_chars(p_source);
	g_last_char_position := 0;
	g_last_concrete_token := token(null, null, null, null);
	g_match_recognize_paren_count := 0;
	g_pattern_paren_count := 0;

	--Get all the tokens.
	loop
		v_token := get_token;
		v_tokens.extend;
		v_tokens(v_tokens.count) := v_token;
		track_row_pattern_matching(v_token);
		if v_token.type not in ('whitespace', 'comment', 'EOF') then
			g_last_concrete_token := v_token;
		end if;
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


--------------------------------------------------------------------------------
--Is the character white space.
function is_lexical_whitespace(p_char nvarchar2) return boolean is
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
end is_lexical_whitespace;


--------------------------------------------------------------------------------
--Create a nested table of characters.
--This extra step takes care of non-trivial unicode processing up front.
--This cannot be simplified with SUBSTRC, that will not work for large CLOBs.
--TODO: Is there an easier way to do this?
function get_nvarchar2_table_from_nclob(p_nclob nclob) return nvarchar2_table
is
	v_chars nvarchar2_table := nvarchar2_table();

	v_position number := 1;
	v_clob_length number := nvl(dbms_lob.getlength(p_nclob), 0);
	v_char nvarchar2(2 char);

	v_amount_of_lob_data_0 exception;
	pragma exception_init(v_amount_of_lob_data_0, -22923);

	v_offset_not_on_char_boundary exception;
	pragma exception_init(v_offset_not_on_char_boundary, -22831);
begin
	--Loop through each code point, which is not necessarily one character.
	loop
		exit when v_position > v_clob_length;

		--When starting from the beginning (offset = 1), substr will warn if 0 characters are read.
		--This means only 2 of the 4 bytes were retrieved and we need to increase the amount.
		if v_position = 1 then
			begin
				v_char := dbms_lob.substr(lob_loc => p_nclob, amount => 1, offset => v_position);
				v_position := v_position + 1;
			exception when v_amount_of_lob_data_0 then
				v_char := dbms_lob.substr(lob_loc => p_nclob, amount => 2, offset => v_position);
				v_position := v_position + 2;
			end;
		--When starting anywhere else in the CLOB, Oracle will only throw an error if you *start*
		--from a bad position and not if you *stop* in a bad position.
		--To detect if the next character is 1 or 2 code points, first read starting one character
		--ahead.  If it works, get one code point, if it fails, get two.
		else
			begin
				--Jump ahead to next position just to see if it throws an error.
				v_char := dbms_lob.substr(lob_loc => p_nclob, amount => 1, offset => v_position + 1);
				--If it didn't, then grab one code point.
				v_char := dbms_lob.substr(lob_loc => p_nclob, amount => 1, offset => v_position);
				v_position := v_position + 1;
			--If there's an exception, grab 2 code points.
			exception when v_offset_not_on_char_boundary then
				v_char := dbms_lob.substr(lob_loc => p_nclob, amount => 2, offset => v_position);
				v_position := v_position + 2;
			end;
		end if;

		v_chars.extend();
		v_chars(v_chars.count) := v_char;

	end loop;

	return v_chars;

end get_nvarchar2_table_from_nclob;


end;
/
