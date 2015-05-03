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
function has_plsql_declaration(p_tokens token_table) return boolean is
	v_previous_concrete_token_1 token := token(null, null, null, null);
	v_previous_concrete_token_2 token := token(null, null, null, null);
	v_previous_concrete_token_3 token := token(null, null, null, null);
begin
	--TODO: Test this.

	for i in 1 .. p_tokens.count loop
		--Detect PL/SQL Declaration and return true
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
		--Shift tokens if it is not a whitespace or comment.
		elsif p_tokens(i).type not in ('whitespace', 'comment') then
			v_previous_concrete_token_1 := p_tokens(i);
			v_previous_concrete_token_2 := v_previous_concrete_token_1;
			v_previous_concrete_token_3 := v_previous_concrete_token_2;
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
	p_terminator varchar2
) is
	v_token_index number := 0;
	v_new_statement nclob;
	v_new_tokens token_table := token_table();
begin
	--Look for a ';' anywhere.
	if p_terminator = ';' then
		--Build new statement and count tokens.
		loop
			exit when v_token_index = p_tokens.count;
			v_token_index := v_token_index + 1;
			v_new_statement := v_new_statement || p_tokens(v_token_index).value;

			if p_tokens(v_token_index).type = ';' or p_tokens(v_token_index).type = 'EOF' then
				--Stop if no more tokens.
				if v_token_index = p_tokens.count then
					exit;
				--Consume all tokens if only whitespace, comments, and EOF remain.
				elsif only_ws_comments_eof_remain(p_tokens, v_token_index+1) then
					--Consume all tokens.
					loop
						v_token_index := v_token_index + 1;
						v_new_statement := v_new_statement || p_tokens(v_token_index).value;
						exit when v_token_index = p_tokens.count;
					end loop;
				--Otherwise stop at this spot.
				else
					exit;
				end if;
			end if;
		end loop;

		--Remove the first character if it's a newline.
		if substr(v_new_statement, 1, 1) = chr(10) and dbms_lob.getLength(v_new_statement) > 1 then
			dbms_lob.copy(
				dest_lob => v_new_statement,
				src_lob => v_new_statement,
				amount => dbms_lob.getLength(v_new_statement)-1,
				src_offset => 2);
			dbms_lob.trim(lob_loc => v_new_statement, newlen => dbms_lob.getLength(v_new_statement)-1);
		elsif substr(v_new_statement, 1, 2) = chr(13)||chr(10) and dbms_lob.getLength(v_new_statement) > 2 then
			dbms_lob.copy(
				dest_lob => v_new_statement,
				src_lob => v_new_statement,
				amount => dbms_lob.getLength(v_new_statement)-2,
				src_offset => 3);
			dbms_lob.trim(lob_loc => v_new_statement, newlen => dbms_lob.getLength(v_new_statement)-2);
		end if;

		--Add new statement to array
		p_split_statements.extend;
		p_split_statements(p_split_statements.count) := v_new_statement;
	--Look for a '/' on a line by itself, separated only by whitespace.
	elsif p_terminator = '/' then
		null;
	end if;

	--Create new tokens table excluding the tokens used for the new statement.
	for i in v_token_index+1 .. p_tokens.count loop
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
begin

	--Tokenize.
	v_tokens := tokenizer.tokenize(p_statements);
	--TODO: Remove
	dbms_output.put_line(tokenizer.print_tokens(v_tokens));

	--Split into statements.
	loop
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
			has_plsql_declaration(v_tokens)
		)
		then
			--TODO
			null;
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
			add_statement_consume_tokens(v_split_statements, v_tokens, ';');
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
