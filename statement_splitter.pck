create or replace package statement_splitter is
--Copyright (C) 2015 Jon Heller.  This program is licensed under the LGPLv3.


/*
## Purpose ##

Split a string into separate SQL and PL/SQL statements based on standard terminators.

Terminators work according to SQL*Plus rules, even though SQL*Plus-only commands do not work:
	1. statements can terminate with ";" or "/".
	2. A "/" terminator must be on a line by itself, excluding whitespace.
	3. If the statement includes a PL/SQL block it must be terminated by a "/".
	These statements always require a "/" at the end:
		"CREATE ASSEMBLY|FUNCTION|JAVA|LIBRARY|PACKAGE|PACKAGE BODY|PROCEDURE|TYPE|TYPE BODY|TRIGGER"
	These statements require a terminating "/" if they use the 12c plsql_declaration (WITH) feature:
		CREATE MATERIALIZED VIEW, CREATE SCHEMA, DELETE, EXPLAIN, INSERT, SELECT, UPDATE, MERGE
	4. As a convenience, terminators may be excluded from the last statement.


## Output ##

TODO

## Requirements ##

TODO

## Example ##

TODO

*/

function split(p_statements in nclob) return nclob_table;

end;
/
create or replace package body statement_splitter is


--------------------------------------------------------------------------------
/*
Purpose: Detect PLSQL_DECLARATION, a new 12c feature that allows PL/SQL in SQL.

Description:
A PL/SQL Declaration must have this pattern before the first ";":

	(null or not "START") "WITH" "FUNCTION" (not "(" or "AS")

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
			lower(v_previous_concrete_token_1.value) = 'function' and
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
	p_terminator varchar2,
	p_new_statement in out nclob,
	p_token_index in out number
) is
	v_new_tokens token_table := token_table();
begin
	--Look for a ';' anywhere.
	if p_terminator = ';' then
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
	elsif p_terminator = '/' then
		null;

	--TODO:
	/*
	Match BEGIN and ENDs
		They are not reserved words so they must only be counted when they are in the right spot.
	BEGIN must come after "as", "is", ";", or ">>", or the beginning of the string. 
		"as" could be a column name, but it cannot be referenced as a column name:
		select as from (select 1 as from dual);
			   *
		ERROR at line 1:
		ORA-00936: missing expression
	END must come after ";"
		It cannot come after ">>", labels can't go there without compilation error.
		end could be an object, but the object will be invalid so things won't compile
	*/
	--TODO: Find OTHER plsql_declarations and last ';'
	elsif p_terminator = 'END' then
		declare
			v_previous_concrete_token_1 token := token(null, null, null, null);
			v_previous_concrete_token_2 token := token(null, null, null, null);
			v_previous_concrete_token_3 token := token(null, null, null, null);
			v_has_entered_block boolean := false;
			v_block_counter number := 0;
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
					lower(v_previous_concrete_token_1.value) in ('as', 'is', ';', '>>')
					or
					v_previous_concrete_token_1.type is null
				) then
					v_has_entered_block := true;
					v_block_counter := v_block_counter + 1;
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
					--TODO: Need new function, "is_next_plsql_declaration".
					elsif has_plsql_declaration(p_tokens, p_token_index) then
						p_token_index := p_token_index + 1;
						add_statement_consume_tokens(p_split_statements, p_tokens, 'BEGIN', p_new_statement, p_token_index);
						return;
					--Otherwise look for the next ';'.
					else
						p_token_index := p_token_index + 1;
						add_statement_consume_tokens(p_split_statements, p_tokens, ';', p_new_statement, p_token_index);
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
--Print tokens for debugging.
function split(p_statements in nclob) return nclob_table is
	v_split_statements nclob_table := nclob_table();
	v_tokens token_table;
	v_command_name varchar2(4000);
	v_temp_new_statement nclob;
	v_temp_token_index number;
begin

	--Tokenize.
	v_tokens := tokenizer.tokenize(p_statements);
	--TODO: Remove
	dbms_output.put_line(tokenizer.print_tokens(v_tokens));

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
				p_statement => p_statements,
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

		--#3: Match BEGIN and END
		elsif
		v_command_name in ('CREATE FUNCTION','CREATE PROCEDURE','CREATE TRIGGER','CREATE TYPE BODY')
		OR
		(
			v_command_name in ('CREATE MATERIALIZED VIEW ', 'CREATE SCHEMA', 'CREATE TABLE', 'CREATE VIEW', 'DELETE', 'EXPLAIN', 'INSERT', 'SELECT', 'UPDATE', 'UPSERT')
			AND
			has_plsql_declaration(v_tokens, 1)
		)
		then
			dbms_output.put_line('HAS PLSQL_DECLARATION');
			add_statement_consume_tokens(v_split_statements, v_tokens, 'END', v_temp_new_statement, v_temp_token_index);


		--#4: Stop at possibly unbalanced BEGIN/END;
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
		--#5: Stop at first END.
		--(TODO: Can declaration have unbalanced begin and end for cursors?)
		elsif v_command_name in ('CREATE PACKAGE') then
			--TODO
			null;

		--#6: Stop at first ";" for everything else.
		else
			add_statement_consume_tokens(v_split_statements, v_tokens, ';', v_temp_new_statement, v_temp_token_index);
		end if;

		--TODO:
		--Stop whenever "/" on a line by itself with only comments or whitespace




/*
			--Commands that always require a "/":
			v_command_name in ('CREATE ASSEMBLY','CREATE FUNCTION','CREATE JAVA','CREATE LIBRARY','CREATE PACKAGE',
				'CREATE PACKAGE BODY','CREATE PROCEDURE','CREATE TRIGGER','CREATE TYPE','CREATE TYPE BODY')
			or
			--Commands that sometimes require a "/":
			(
				v_command_name in ('CREATE MATERIALIZED VIEW ', 'CREATE SCHEMA', 'DELETE', 'EXPLAIN', 'INSERT', 'SELECT', 'UPDATE', 'UPSERT')
				and has_plsql_declaration(v_tokens)
			) then
			add_statement_consume_tokens(v_split_statements, v_tokens, '/');
		--All other commands stop at first ";" or EOF:
		else
			add_statement_consume_tokens(v_split_statements, v_tokens, ';');
		end if;
*/

		--Quit when there are no more tokens.
		exit when v_tokens.count = 0;
	end loop;

	return v_split_statements;
end split;

end;
/
