create or replace package statement_splitter is
--Copyright (C) 2015 Jon Heller.  This program is licensed under the LGPLv3.


/*
## Purpose ##

Split a string of separate SQL and PL/SQL statements terminated by ";".

Unlike SQL*Plus, even PL/SQL-like statements can be terminiated solely with a ";".
This is helpful because it's difficult to use a "/" in strings in most IDEs.

If you want to run in a more SQL*Plus-like mode, set p_optional_sqlplus_delimiter
to "/".  Then a "/" on a line by itself is also a terminator.  This optional
delimiter is configurable, does not override the ";" terminator, and is removed
from the split strings.


## Output ##

TODO

## Requirements ##

TODO

## Example ##

TODO

*/

function split(p_statements in nclob, p_optional_sqlplus_delimiter in nvarchar2 default null) return nclob_table;

end;
/
create or replace package body statement_splitter is

C_TERMINATOR_SEMI              constant number := 1;
C_TERMINATOR_PLSQL_DECLARE_END constant number := 2;
C_TERMINATOR_PLSQL_END         constant number := 3;

type token_table_table is table of token_table;


--------------------------------------------------------------------------------
/*
Purpose: Detect PLSQL_DECLARATION, a new 12c feature that allows PL/SQL in SQL.

Description:
A PL/SQL Declaration must have this pattern before the first ";":

	(null or not "START") "WITH" ("FUNCTION"|"PROCEDURE") (not "(" or "AS")

This was discovered by analyzing all "with" strings in the Oracle documentation
text descriptions.  That is, download the library and run a command like this:

	C:\E11882_01\E11882_01\server.112\e26088\img_text>findstr /s /i "with" *.*

There are a lot of potential ambiguities as SQL does not have many fully
reserved words.  And the pattern "with" "function" can be found in 2 cases:the following:

	1. Hierarchical queries.  Exclude them by looking for "start" before "with".
	select *
	from
	(
		select 1 function from dual
	)
	connect by function = 1
	start with function = 1;

	Note: "start" cannot be the name of a table, no need to worry about DML
	statements like `insert into start with ...`.

	2. Subquery factoring that uses "function" as a name.  Stupid, but possible.

	with function as (select 1 a from dual) select * from function;
	with function(a) as (select 1 a from dual) select * from function;
*/
function has_plsql_declaration(p_tokens token_table, p_token_start_index in number) return boolean is
	v_previous_concrete_token_1 token := token(null, null, null, null);
	v_previous_concrete_token_2 token := token(null, null, null, null);
	v_previous_concrete_token_3 token := token(null, null, null, null);
begin
	for i in p_token_start_index .. p_tokens.count loop
		--Return true if PL/SQL Declaration found.
		if
		--For performance, check types first, instead of potentially large values.
		(
			p_tokens(i).type = 'word' and
			v_previous_concrete_token_1.type = 'word' and
			v_previous_concrete_token_2.type = 'word' and
			(v_previous_concrete_token_3.type = 'word' or v_previous_concrete_token_3.type is null)
		)
		and
		(
			lower(p_tokens(i).value) <> 'as' and
			lower(v_previous_concrete_token_1.value) in ('function', 'procedure') and
			lower(v_previous_concrete_token_2.value) = 'with' and
			(lower(v_previous_concrete_token_3.value) <> 'start' or v_previous_concrete_token_3.value is null)
		) then
			return true;
		--Return false if ';' is found.
		elsif p_tokens(i).type = ';' then
			return false;
		--Shift tokens if it is not a whitespace or comment.
		elsif p_tokens(i).type not in ('whitespace', 'comment') then
			v_previous_concrete_token_3 := v_previous_concrete_token_2;
			v_previous_concrete_token_2 := v_previous_concrete_token_1;
			v_previous_concrete_token_1 := p_tokens(i);
		end if;
	end loop;

	--Return false is nothing found.
	return false;
end has_plsql_declaration;





--------------------------------------------------------------------------------
/*
Purpose: Detect if there is another PLSQL_DECLARATION.  This is only valid if
called immediately at the end of another PLSQL_DECLARATION.

An additional PL/SQL Declaration must start with this pattern:

	("function"|"procedure") word [anything other than "(", "is", or "as"]

This is complicated because there may be a regular common table expression with
the name "function" or "procedure".  Thanks, Oracle, for not reserving keywords.

See the function has_plsql_declaration for some more information.
*/
function has_another_plsql_declaration(p_tokens token_table, p_token_start_index in number) return boolean is
	v_next_concrete_token_1 token := token(null, null, null, null);
	v_next_concrete_token_2 token := token(null, null, null, null);
begin
	--Loop through the tokens and find concrete tokens.
	for i in p_token_start_index .. p_tokens.count loop
		--If it's concrete, decide which one it is.
		if p_tokens(i).type not in ('whitespace', 'comment', 'EOF') then
			--Record the first one.
			if v_next_concrete_token_1.type is null then
				v_next_concrete_token_1 := p_tokens(i);
			--Record the second one and exit the loop.
			else
				v_next_concrete_token_2 := p_tokens(i);
				exit;
			end if;
		end if;
	end loop;

	--Determine if there is another PL/SQL Declaration.
	if
	lower(v_next_concrete_token_1.value) in ('function', 'procedure') and
	lower(v_next_concrete_token_2.value) not in ('(', 'is', 'as') then
		return true;
	else
		return false;
	end if;
end has_another_plsql_declaration;


--------------------------------------------------------------------------------
function only_ws_comments_eof_remain(p_tokens in out nocopy token_table, p_token_index in number)  return boolean is
begin
	for i in p_token_index .. p_tokens.count loop
		if p_tokens(i).type not in ('whitespace', 'comment', 'EOF') then
			return false;
		end if;
	end loop;
	return true;
end only_ws_comments_eof_remain;


--------------------------------------------------------------------------------
procedure add_statement_consume_tokens(
	p_split_statements in out nocopy nclob_table,
	p_tokens in out nocopy token_table,
	p_terminator number,
	p_new_statement in out nclob,
	p_token_index in out number
) is
	v_new_tokens token_table := token_table();

	--Return the next concrete token, or NULL if there are no more.
	function get_next_concrete_value_n(p_n in number) return nvarchar2 is
		v_concrete_token_counter number := 0;
	begin
		--Loop through the tokens.
		for i in p_token_index + 1 .. p_tokens.count loop
			--Process if it's concrte.
			if p_tokens(i).type not in ('whitespace', 'comment', 'EOF') then
				--Increment concrete counter.
				v_concrete_token_counter := v_concrete_token_counter + 1;

				--Return the value if we've reached the Nth concrete token.
				if v_concrete_token_counter = p_n then
					return p_tokens(i).value;
				end if;
			end if;
		end loop;

		--Return NULL if nothing was found.
		return null;
	end;
begin
	--Look for a ';' anywhere.
	if p_terminator = C_TERMINATOR_SEMI then
		--Build new statement and count tokens.
		loop
			--Increment.
			exit when p_token_index >= p_tokens.count;
			p_new_statement := p_new_statement || p_tokens(p_token_index).value;

			--Detect end of statement.
			if p_tokens(p_token_index).type = ';' or p_tokens(p_token_index).type = 'EOF' then
				--Stop if no more tokens.
				if p_token_index = p_tokens.count then
					exit;
				--Consume all tokens if only whitespace, comments, and EOF remain.
				elsif only_ws_comments_eof_remain(p_tokens, p_token_index+1) then
					--Consume all tokens.
					loop
						p_token_index := p_token_index + 1;
						p_new_statement := p_new_statement || p_tokens(p_token_index).value;
						exit when p_token_index = p_tokens.count;
					end loop;
				--Otherwise stop at this spot.
				else
					exit;
				end if;
			end if;

			p_token_index := p_token_index + 1;
		end loop;

	--Look for a '/' on a line by itself, separated only by whitespace.
	--TODO: Remove this?
	--elsif p_terminator = '/' then
	--	null;

	/*
	Match BEGIN and END for a PLSQL_DECLARATION.
		They are not reserved words so they must only be counted when they are in the right spot.
	BEGIN must come after "begin", "as", "is", ";", or ">>", or the beginning of the string. 
		- "as" could be a column name, but it cannot be referenced as a column name:
			select as from (select 1 as from dual);
				   *
			ERROR at line 1:
			ORA-00936: missing expression
		- Some forms of "begin begin" do not count, such as select begin begin from (select 1 begin from dual);
		- Exclude "as begin" if it's used as a column alias.  Exclude where the next concrete
		  token(s) are ",", "from", "into", or "bulk collect".
	END must come after ";"
		It cannot come after ">>", labels can't go there without compilation error.
		end could be an object, but the object will be invalid so things won't compile
	*/
	elsif p_terminator = C_TERMINATOR_PLSQL_DECLARE_END then
		declare
			v_previous_concrete_token_1 token := token(null, null, null, null);
			v_previous_concrete_token_2 token := token(null, null, null, null);
			v_previous_concrete_token_3 token := token(null, null, null, null);
			v_has_entered_block boolean := false;
			v_block_counter number := 0;
			v_prev_conc_tok_was_real_begin boolean := false;
		begin
			--Build new statement and count tokens.
			loop
				--Increment
				exit when p_token_index >= p_tokens.count;
				p_new_statement := p_new_statement || p_tokens(p_token_index).value;

				--Detect BEGIN
				if
				lower(p_tokens(p_token_index).value) = 'begin'
				and
				(
					(
						lower(v_previous_concrete_token_1.value) in ('as', 'is', ';', '>>')
						or
						v_previous_concrete_token_1.type is null
					)
					or
					(
						--Ignore some "begin begin", such as select begin begin from (select 1 begin from dual);
						lower(v_previous_concrete_token_1.value) = 'begin'
						and
						v_prev_conc_tok_was_real_begin
					)
				)
				--Ignore "as begin" if it's used as a column alias.
				and not
				(
					lower(v_previous_concrete_token_1.value) = 'as'
					and
					get_next_concrete_value_n(1) in (',', 'from', 'into')
				)
				and not
				(
					lower(v_previous_concrete_token_1.value) = 'as'
					and
					lower(get_next_concrete_value_n(1)) in ('bulk')
					and
					lower(get_next_concrete_value_n(2)) in ('collect')
				)
				then
					v_has_entered_block := true;
					v_block_counter := v_block_counter + 1;
					v_prev_conc_tok_was_real_begin := true;
				--If token is concrete, reset the flag.
				elsif p_tokens(p_token_index).type not in ('whitespace', 'comment', 'EOF') then
					v_prev_conc_tok_was_real_begin := false;
				end if;

				--Detect END
				if
				p_tokens(p_token_index).type = ';'
				and
				(
					(
						lower(v_previous_concrete_token_1.value) = 'end'
						and
						lower(v_previous_concrete_token_2.type) = ';'
					)
					or
					--Optional block name.
					(
						lower(v_previous_concrete_token_1.type) = 'word'
						and
						lower(v_previous_concrete_token_2.value) = 'end'
						and
						lower(v_previous_concrete_token_3.type) = ';'
					)
				) then
					v_block_counter := v_block_counter - 1;
				end if;

				--Detect end of statement.
				if (v_has_entered_block and v_block_counter = 0) or p_tokens(p_token_index).type = 'EOF' then
					--Stop if no more tokens.
					if p_token_index = p_tokens.count then
						exit;
					--Consume all tokens if only whitespace, comments, and EOF remain.
					elsif only_ws_comments_eof_remain(p_tokens, p_token_index+1) then
						--Consume all tokens.
						loop
							p_token_index := p_token_index + 1;
							p_new_statement := p_new_statement || p_tokens(p_token_index).value;
							exit when p_token_index = p_tokens.count;
						end loop;
					--There could be more than one function.
					elsif has_another_plsql_declaration(p_tokens, p_token_index + 1) then
						p_token_index := p_token_index + 1;
						add_statement_consume_tokens(p_split_statements, p_tokens, C_TERMINATOR_PLSQL_DECLARE_END, p_new_statement, p_token_index);
						return;
					--Otherwise look for the next ';'.
					else
						p_token_index := p_token_index + 1;
						add_statement_consume_tokens(p_split_statements, p_tokens, C_TERMINATOR_SEMI, p_new_statement, p_token_index);
						return;
					end if;
				end if;

				--Shift tokens if it is not a whitespace or comment.
				if p_tokens(p_token_index).type not in ('whitespace', 'comment') then
					v_previous_concrete_token_3 := v_previous_concrete_token_2;
					v_previous_concrete_token_2 := v_previous_concrete_token_1;
					v_previous_concrete_token_1 := p_tokens(p_token_index);
				end if;

				--Increment
				p_token_index := p_token_index + 1;
			end loop;
		end;

	/*
	Match BEGIN and END for a common PL/SQL block.
		They are not reserved words so they must only be counted when they are in the right spot.
	BEGIN must come after "begin", "as", "is", ";", or ">>", or the beginning of the string. 
		- "as" could be a column name, but it cannot be referenced as a column name:
			select as from (select 1 as from dual);
				   *
			ERROR at line 1:
			ORA-00936: missing expression
		- Some forms of "begin begin" do not count, such as select begin begin from (select 1 begin from dual);
		- Exclude "as begin" if it's used as a column alias.  Exclude where the next concrete
		  token(s) are ",", "from", "into", or "bulk collect".
	END must come after ";"
		It cannot come after ">>", labels can't go there without compilation error.
		end could be an object, but the object will be invalid so things won't compile
	*/
	elsif p_terminator = C_TERMINATOR_PLSQL_END then
		declare
			v_previous_concrete_token_1 token := token(null, null, null, null);
			v_previous_concrete_token_2 token := token(null, null, null, null);
			v_previous_concrete_token_3 token := token(null, null, null, null);
			v_has_entered_block boolean := false;
			v_block_counter number := 0;
			v_prev_conc_tok_was_real_begin boolean := false;
		begin
			--Build new statement and count tokens.
			loop
				--Increment
				exit when p_token_index >= p_tokens.count;
				p_new_statement := p_new_statement || p_tokens(p_token_index).value;

				--Detect BEGIN
				--
				--Detecting BEGIN after 'as', 'is', ';', and '>>' is simple.
				if
				lower(p_tokens(p_token_index).value) = 'begin'
				and
				(
					(
						lower(v_previous_concrete_token_1.value) in ('as', 'is', ';', '>>')
						or
						v_previous_concrete_token_1.type is null
					)
					or
					(
						--Ignore some "begin begin", such as select begin begin from (select 1 begin from dual);
						lower(v_previous_concrete_token_1.value) = 'begin'
						and
						v_prev_conc_tok_was_real_begin
					)
				)
				--Ignore "as begin" if it's used as a column alias.
				and not
				(
					lower(v_previous_concrete_token_1.value) = 'as'
					and
					get_next_concrete_value_n(1) in (',', 'from', 'into')
				)
				and not
				(
					lower(v_previous_concrete_token_1.value) = 'as'
					and
					lower(get_next_concrete_value_n(1)) in ('bulk')
					and
					lower(get_next_concrete_value_n(2)) in ('collect')
				) then
					v_has_entered_block := true;
					v_block_counter := v_block_counter + 1;
					v_prev_conc_tok_was_real_begin := true;
				--If token is concrete, reset the flag.
				elsif p_tokens(p_token_index).type not in ('whitespace', 'comment', 'EOF') then
					v_prev_conc_tok_was_real_begin := false;
				end if;

				--Detect END
				if
				p_tokens(p_token_index).type = ';'
				and
				(
					(
						lower(v_previous_concrete_token_1.value) = 'end'
						and
						lower(v_previous_concrete_token_2.type) = ';'
					)
					or
					--Optional block name.
					(
						lower(v_previous_concrete_token_1.type) = 'word'
						and
						lower(v_previous_concrete_token_2.value) = 'end'
						and
						lower(v_previous_concrete_token_3.type) = ';'
					)
				) then
					v_block_counter := v_block_counter - 1;
				end if;

				--Detect end of statement.
				if (v_has_entered_block and v_block_counter = 0) or p_tokens(p_token_index).type = 'EOF' then
					--Consume all tokens if only whitespace, comments, and EOF remain.
					if only_ws_comments_eof_remain(p_tokens, p_token_index+1) then
						--Consume all tokens.
						loop
							p_token_index := p_token_index + 1;
							p_new_statement := p_new_statement || p_tokens(p_token_index).value;
							exit when p_token_index = p_tokens.count;
						end loop;
					--Else stop here.
					else
						exit;
					end if;
				end if;

				--Shift tokens if it is not a whitespace or comment.
				if p_tokens(p_token_index).type not in ('whitespace', 'comment') then
					v_previous_concrete_token_3 := v_previous_concrete_token_2;
					v_previous_concrete_token_2 := v_previous_concrete_token_1;
					v_previous_concrete_token_1 := p_tokens(p_token_index);
				end if;

				--Increment
				p_token_index := p_token_index + 1;
			end loop;
		end;
	end if;

	--Remove the first character if it's a newline.
	if substr(p_new_statement, 1, 1) = chr(10) and dbms_lob.getLength(p_new_statement) > 1 then
		dbms_lob.copy(
			dest_lob => p_new_statement,
			src_lob => p_new_statement,
			amount => dbms_lob.getLength(p_new_statement)-1,
			src_offset => 2);
		dbms_lob.trim(lob_loc => p_new_statement, newlen => dbms_lob.getLength(p_new_statement)-1);
	elsif substr(p_new_statement, 1, 2) = chr(13)||chr(10) and dbms_lob.getLength(p_new_statement) > 2 then
		dbms_lob.copy(
			dest_lob => p_new_statement,
			src_lob => p_new_statement,
			amount => dbms_lob.getLength(p_new_statement)-2,
			src_offset => 3);
		dbms_lob.trim(lob_loc => p_new_statement, newlen => dbms_lob.getLength(p_new_statement)-2);
	end if;

	--Add new statement to array
	p_split_statements.extend;
	p_split_statements(p_split_statements.count) := p_new_statement;

	--TODO: Make sure every statement ends with an EOF?

	--Create new tokens table excluding the tokens used for the new statement.
	for i in p_token_index+1 .. p_tokens.count loop
		v_new_tokens.extend;
		v_new_tokens(v_new_tokens.count) := p_tokens(i);
	end loop;
	p_tokens := v_new_tokens;

end add_statement_consume_tokens;


--------------------------------------------------------------------------------
--Split a string into separate strings by an optional delmiter, usually "/".
--This follows the SQL*Plus rules - the delimiter must be on a line by itself,
--although the line may contain whitespace before and after the delimiter.
function split_string_by_optional_delim(p_statements in nclob, p_optional_sqlplus_delimiter in nvarchar2)
return nclob_table is
	v_chars nvarchar2_table := tokenizer.get_nvarchar2_table_from_nclob(p_statements);
	v_delimiter_size number := lengthc(p_optional_sqlplus_delimiter);
	v_char_index number := 0;
	v_string nclob;
	v_is_empty_line boolean := true;

	v_strings nclob_table := nclob_table();

	--Get N chars for comparing with multi-character delimiter.
	function get_next_n_chars(p_n number) return nvarchar2 is
		v_next_n_chars nvarchar2(32767);
	begin
		for i in v_char_index .. least(v_char_index + p_n - 1, v_chars.count) loop
			v_next_n_chars := v_next_n_chars || v_chars(i);
		end loop;

		return v_next_n_chars;
	end get_next_n_chars;
begin
	--Return whole string if the delimiter is NULL.
	if p_optional_sqlplus_delimiter is null then
		v_strings.extend;
		v_strings(v_strings.count) := p_statements;
		return v_strings;
	--Throw an error if the delimiter is whitespace.
	elsif tokenizer.is_lexical_whitespace(p_optional_sqlplus_delimiter) then
		raise_application_error(-20000, 'The optional delimiter cannot be set to whitespace.');
	end if;

	--Loop through characters and build strings.
	loop
		v_char_index := v_char_index + 1;

		--Look for delimiter if it's on an empty line.
		if v_is_empty_line then
			--Push, increment counter for multi-char delimiters, and exit if last characters are delimiter.
			if v_char_index = v_chars.count and get_next_n_chars(v_delimiter_size) = p_optional_sqlplus_delimiter then
				v_strings.extend;
				v_strings(v_strings.count) := v_string;
				v_char_index := v_char_index + v_delimiter_size - 1;
				exit;
			--Add char, push, and exit if it's the last character.
			elsif v_char_index = v_chars.count then
				v_string := v_string || v_chars(v_char_index);
				v_strings.extend;
				v_strings(v_strings.count) := v_string;
				exit;
			--Continue if it's still whitespace. 
			elsif tokenizer.is_lexical_whitespace(v_chars(v_char_index)) then
				v_string := v_string || v_chars(v_char_index);
			--Split string if delimiter is found
			elsif get_next_n_chars(v_delimiter_size) = p_optional_sqlplus_delimiter /*+ TODO */ /*and rest of line is only whitespace*/ then
				--TODO: Exclude N characters.
				v_strings.extend;
				v_strings(v_strings.count) := v_string;
				v_string := null;
				v_char_index := v_char_index + v_delimiter_size - 1;
			--It's no longer an empty line otherwise.
			else
				v_string := v_string || v_chars(v_char_index);
				v_is_empty_line := false;
			end if;
		--Look for newlines.
		elsif v_chars(v_char_index) = chr(10) then
			v_string := v_string || v_chars(v_char_index);
			v_is_empty_line := true;
		--Add the string after the last character.
		elsif v_char_index >= v_chars.count then
			v_string := v_string || v_chars(v_char_index);
			v_strings.extend;
			v_strings(v_strings.count) := v_string;
			exit;
		--Else just add the character.
		else
			v_string := v_string || v_chars(v_char_index);
		end if;
	end loop;

	return v_strings;
end split_string_by_optional_delim;


--------------------------------------------------------------------------------
--Split a token stream into statements by ";".
function split_tokens_by_primary_term(p_tokens in out token_table) return nclob_table is
	v_split_statements nclob_table := nclob_table();
	v_command_name varchar2(4000);
	v_temp_new_statement nclob;
	v_temp_token_index number;
begin
	--Split into statements.
	loop
		v_temp_new_statement := null;
		v_temp_token_index := 1;

		--Classify.
		declare
			v_throwaway_number number;
			v_throwaway_string varchar2(32767);
		begin
			statement_classifier.classify(
				p_abstract_tokens => p_tokens,
				p_category => v_throwaway_string,
				p_statement_type => v_throwaway_string,
				p_command_name => v_command_name,
				p_command_type => v_throwaway_number,
				p_lex_sqlcode => v_throwaway_number,
				p_lex_sqlerrm => v_throwaway_string
			);
		end;

		--Find a terminating token based on the classification.
		--
		--#1: Throw error if statement could not be classified:
		if v_command_name is null then
			raise_application_error(-20000, 'Cannot classify and split statement(s).  Check the syntax.');

		--#2: Match "}" for Java code.
		/*
			'CREATE JAVA', if "{" is found before first ";"
			Note: Single-line comments are different, "//".  Exclude any "", "", or "" after a 
				Create java_partial_tokenizer to lex Java statements (Based on: https://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html), just need:
					- multi-line comment
					- single-line comment - Note Lines are terminated by the ASCII characters CR, or LF, or CR LF.
					- character literal - don't count \'
					- string literal - don't count \"
					- {
					- }
					- other
					- Must all files end with }?  What about packages only, or annotation only file?

				CREATE JAVA CLASS USING BFILE (java_dir, 'Agent.class')
				CREATE JAVA SOURCE NAMED "Welcome" AS public class Welcome { public static String welcome() { return "Welcome World";   } }
				CREATE JAVA RESOURCE NAMED "appText" USING BFILE (java_dir, 'textBundle.dat')

				TODO: More examples using lexical structures.
		*/
		elsif v_command_name in ('CREATE JAVA') then
			--TODO
			raise_application_error(-20000, 'CREATE JAVA is not yet supported.');

		--#3: Match PLSQL_DECLARATION BEGIN and END.
		elsif
		v_command_name in ('CREATE MATERIALIZED VIEW ', 'CREATE SCHEMA', 'CREATE TABLE', 'CREATE VIEW', 'DELETE', 'EXPLAIN', 'INSERT', 'SELECT', 'UPDATE', 'UPSERT')
		and
		has_plsql_declaration(p_tokens, 1)
		then
			add_statement_consume_tokens(v_split_statements, p_tokens, C_TERMINATOR_PLSQL_DECLARE_END, v_temp_new_statement, v_temp_token_index);


		--#4: Match PL/SQL BEGIN and END.
		elsif v_command_name in ('CREATE FUNCTION','CREATE PROCEDURE','CREATE TRIGGER','CREATE TYPE BODY', 'PL/SQL EXECUTE') then
			add_statement_consume_tokens(v_split_statements, p_tokens, C_TERMINATOR_PLSQL_END, v_temp_new_statement, v_temp_token_index);

		--#5: Stop at possibly unbalanced BEGIN/END;
		/*
		4a
		create or replace package test_package is
		end;

		4b
		create or replace package body test_package is
		begin
			null;
		end;

		4c
		create or replace package body test_package is
			procedure test1 is begin null; end;
		end;

		4d
		create or replace package body test_package is
			procedure test1 is begin null; end;
		begin
			null;
		end;

		4e
		create or replace package body test_package is
			cursor my_cursor is with function test_function return number is begin return 1; end; select test_function from dual;
			procedure test1 is begin null; end;
		begin
			null;
		end;
		*/
		elsif v_command_name in ('CREATE PACKAGE BODY') then
			--TODO
			null;
			/*
			if CREATE PACKAGE BODY then
				--Nested BEGIN/ENDs in the declare section.
				if is_plsql_declaration or is_procedure_declaration or is_function_declaration then
					loop through begin ends
				--Nested BEGIN/ENDs in initialize section.
				elsif is_begin
					loop through begin ends
				--End of package.
				elsif is_end
					end of package
				if is_begin
			end if;
			*/
		--#6: Stop at first END.
		--(TODO: Can declaration have unbalanced begin and end for cursors?)
		elsif v_command_name in ('CREATE PACKAGE') then
			--TODO
			null;

		--#7: Stop at first ";" for everything else.
		else
			add_statement_consume_tokens(v_split_statements, p_tokens, C_TERMINATOR_SEMI, v_temp_new_statement, v_temp_token_index);
		end if;

		--Quit when there are no more tokens.
		exit when p_tokens.count = 0;
	end loop;

	return v_split_statements;
end split_tokens_by_primary_term;


--------------------------------------------------------------------------------
--Split a string of separate SQL and PL/SQL statements terminated by ";" and
--some secondary terminator, usually "/".
function split(p_statements in nclob, p_optional_sqlplus_delimiter in nvarchar2 default null) return nclob_table is
	v_split_statements nclob_table := nclob_table();
	v_split_tokens token_table_table := token_table_table();
begin
	--Split the string by the optional delimiter, usually "/".
	v_split_statements := split_string_by_optional_delim(p_statements, p_optional_sqlplus_delimiter);

	--Tokenize the strings.
	for i in 1 .. v_split_statements.count loop
		v_split_tokens.extend;
		v_split_tokens(v_split_tokens.count) := tokenizer.tokenize(v_split_statements(i));
	end loop;

	--Split each set of tokens by the primary terminator, ";".
	v_split_statements := nclob_table();
	for i in 1 .. v_split_tokens.count loop
		v_split_statements := v_split_statements multiset union split_tokens_by_primary_term(v_split_tokens(i));
	end loop;

	--Return the statements.
	return v_split_statements;
end split;

end;
/
