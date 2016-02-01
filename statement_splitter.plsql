create or replace package statement_splitter is
--Copyright (C) 2015 Jon Heller.  This program is licensed under the LGPLv3.

function split_by_sqlplus_delimiter(p_statements in nclob, p_sqlplus_delimiter in nvarchar2 default '/') return nclob_table;

--DO NOT USE THE BELOW FUNCTION YET, IT HAS SOME SERIOUS PROBLEMS.
function split_by_semicolon(p_tokens in token_table) return token_table_table;

--DO NOT USE THE BELOW FUNCTION YET, IT HAS SOME SERIOUS PROBLEMS.
function split_by_sqlplus_del_and_semi(p_statements in nclob, p_sqlplus_delimiter in nvarchar2 default '/') return token_table_table;

/*

== Purpose ==

Split a string of separate SQL and PL/SQL statements terminated by ";".

Unlike SQL*Plus, even PL/SQL-like statements can be terminiated solely with a ";".
This is helpful because it's difficult to use a "/" in strings in most IDEs.

If you want to run in a more SQL*Plus-like mode, set p_optional_sqlplus_delimiter
to "/".  Then a "/" on a line by itself is also a terminator.  This optional
delimiter is configurable, does not override the ";" terminator, and is removed
from the split strings.


== Output ==

TODO

== Requirements ==

TODO

== Example ==

TODO

*/

end;
/
create or replace package body statement_splitter is

C_TERMINATOR_SEMI              constant number := 1;
C_TERMINATOR_PLSQL_DECLARE_END constant number := 2;
C_TERMINATOR_PLSQL_MATCHED_END constant number := 3;
C_TERMINATOR_PLSQL_EXTRA_END   constant number := 4;
C_TERMINATOR_EOF               constant number := 5;
C_TERMINATOR_PACKAGE_BODY      constant number := 6;

C_REGULAR_TRIGGER              constant number := 1;
C_COMPOUND_TRIGGER             constant number := 2;
C_CALL_TRIGGER                 constant number := 3;





--------------------------------------------------------------------------------
/*
Purpose: Get the trigger type and the token index for the beginning of the trigger_body.
	The trigger_body token index helps identify when to start counting BEGINs and ENDs.
	Before that point, it's easier to exclude than to include because this is the only
	PL/SQL BEGIN that can start after many different keywords.

For lexing and parsing there are 3 important different types of triggers:
regular triggers, compound triggers, and CALL triggers.

Trigger type is determined by which keywords are found first:
	1. Regular - DECLARE, <<, or BEGIN (e.g. something that begins a PL/SQL body.)
	2. Compound - COMPOUND TRIGGER
	3. Call - CALL

The tricky part with 1 and 3 is that DECLARE, BEGIN, or CALL can be used as
names for other objects.  Based on the trigger syntax diagrams, the "real"
keywords are found when these conditions are true:
	1. It is not found after ('trigger', '.', 'of', ',', 'on', 'as', 'follows', 'precedes', 'table')
	2. It is not inside 'when ( condition )'
*/
procedure get_trigger_type_body_index (
	p_tokens in token_table,
	p_trigger_type out number,
	p_trigger_body_start_index out number
) is
	v_previous_concrete_token_1 token := token(null, null, null, null, null, null, null, null);
	v_previous_concrete_token_2 token := token(null, null, null, null, null, null, null, null);
	v_when_condition_paren_counter number := 0;
begin
	--Loop through all the tokens until a type is found.
	for i in 1 .. p_tokens.count loop
		--Check for new WHEN ( condition ).
		if
		(
			v_when_condition_paren_counter = 0
			and
			p_tokens(i).type = '('
			and
			lower(v_previous_concrete_token_1.value) = 'when'
		) then
			v_when_condition_paren_counter := 1;
		--Only count parenthese if inside WHEN (condition).
		elsif v_when_condition_paren_counter >= 1 then
			if p_tokens(i).type = '(' then
				v_when_condition_paren_counter := v_when_condition_paren_counter + 1;
			elsif p_tokens(i).type = ')' then
				v_when_condition_paren_counter := v_when_condition_paren_counter - 1;
			end if;
		--Compound Trigger check.
		elsif
		(
			lower(p_tokens(i).value) = 'trigger'
			and
			lower(v_previous_concrete_token_1.value) = 'compound'
		) then
			p_trigger_type := C_COMPOUND_TRIGGER;
			p_trigger_body_start_index := i;
			return;
		end if;

		--Ignore some "regular" tokens if they are found in the wrong context.
		if
		(
			lower(v_previous_concrete_token_1.value) in ('trigger', '.', 'of', ',', 'on', 'as', 'follows', 'precedes', 'table')
			or
			v_when_condition_paren_counter >= 1
		) then
			null;
		--Regular token found.
		elsif p_tokens(i).type = 'word' and lower(p_tokens(i).value) in ('declare', '<<', 'begin') then
			p_trigger_type := C_REGULAR_TRIGGER;
			p_trigger_body_start_index := i;
			return;
		--CALL token found.
		elsif lower(p_tokens(i).value) = 'call' then
			p_trigger_type := C_CALL_TRIGGER;
			p_trigger_body_start_index := null;
			return;
		end if;

		--Shift tokens if it is not a whitespace, comment, or EOF.
		if p_tokens(i).type not in ('whitespace', 'comment', 'EOF') then
			v_previous_concrete_token_2 := v_previous_concrete_token_1;
			v_previous_concrete_token_1 := p_tokens(i);
		end if;
	end loop;

	--Return regular type if none was found.
	p_trigger_type := C_REGULAR_TRIGGER;
	p_trigger_body_start_index := null;

end get_trigger_type_body_index;


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
	v_previous_concrete_token_1 token := token(null, null, null, null, null, null, null, null);
	v_previous_concrete_token_2 token := token(null, null, null, null, null, null, null, null);
	v_previous_concrete_token_3 token := token(null, null, null, null, null, null, null, null);
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
		--Shift tokens if it is not a whitespace, comment, or EOF.
		elsif p_tokens(i).type not in ('whitespace', 'comment', 'EOF') then
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
	v_next_concrete_token_1 token := token(null, null, null, null, null, null, null, null);
	v_next_concrete_token_2 token := token(null, null, null, null, null, null, null, null);
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
--Purpose: Determine if a "CREATE PROCEDURE" or "CREATE FUNCTION" is EXTERNAL.
--	That is, does the syntax use either "call_spec" or "EXTERNAL".
--
--This is true if (is|as) (external|langauge java|language c|language dotnet)
--is found before the first semicolon.  I'm not sure if "language dotnet" is valid.
--It appears in some error messages  and in the Oracle Database Lite SQL Reference.
--It shouldn't hurt to include it.
--
--Assumption: This is only called for "CREATE PROCEDURE" or "CREATE FUNCTION".
--
--Example: create procedure test_procedure as external language c name "c_test" library somelib;
function is_external_method(p_tokens in token_table, p_temp_token_index in number) return boolean is
	v_previous_concrete_token_1 token := token(null, null, null, null, null, null, null, null);
	v_previous_concrete_token_2 token := token(null, null, null, null, null, null, null, null);
begin
	for i in p_temp_token_index .. p_tokens.count loop
		--Look for semicolon or a sequence of tokens that implies it's external.
		if p_tokens(i).type = ';' then
			return false;
		elsif
		(
			(
				lower(v_previous_concrete_token_1.value) in ('is', 'as')
				and
				lower(p_tokens(i).value) = 'external'
			)
			or
			(
				lower(v_previous_concrete_token_2.value) in ('is', 'as')
				and
				lower(v_previous_concrete_token_1.value) in ('language')
				and
				lower(p_tokens(i).value) in ('java', 'c', 'dotnet')
			)
		) then
			return true;
		end if;

		--Shift tokens if it is not a whitespace or comment.
		if p_tokens(i).type not in ('whitespace', 'comment') then
			v_previous_concrete_token_2 := v_previous_concrete_token_1;
			v_previous_concrete_token_1 := p_tokens(i);
		end if;
	end loop;

	--Default to not an external method.
	return false;
end is_external_method;


--------------------------------------------------------------------------------
function only_ws_comments_eof_remain(p_tokens in token_table, p_token_index in number)  return boolean is
begin
	for i in p_token_index .. p_tokens.count loop
		if p_tokens(i).type not in ('whitespace', 'comment', 'EOF') then
			return false;
		end if;
	end loop;
	return true;
end only_ws_comments_eof_remain;


--------------------------------------------------------------------------------
--Return the next concrete token, or NULL if there are no more.
function get_next_concrete_value_n(p_tokens in token_table, p_token_index in number, p_n in number) return nvarchar2 is
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
end get_next_concrete_value_n;


--------------------------------------------------------------------------------
/*
BEGIN must come after "begin", "as", "is", ";", or ">>", "then", "else", "loop",
	the beginning of the string, or in the special case of a trigger the first
	BEGIN can happen anywhere after	all the excluded BEGINs are thrown out
	(based on the trigger type).
	- "as" could be a column name, but it cannot be referenced as a column name:
		select as from (select 1 as from dual);
			   *
		ERROR at line 1:
		ORA-00936: missing expression
	- Some forms of "begin begin" do not count, such as select begin begin from (select 1 begin from dual);
	- Exclude "as begin", "then begin", "else begin", and "loop begin" if it's used as an alias.
		Exclude where next concrete token is ",", "from", "into", or "bulk collect".  For column aliases.
		Exclude where next concrete token is "," or ")".  For CLUSTER_ID USING, model columns, PIVOT_IN_CLAUSE, XMLATTRIBUTES, XMLCOLATTVAL, XMLELEMENT, XMLFOREST, XMLnamespaces_clause.
		Exclude where next concrete token is "," or ")" or "columns".  For XMLTABLE_options.
			ASTRONOMICALLY UNLIKELY LEXER BUG: It is possible to have an object named "COLUMNS", although it would be invalid.
		Exclude when "in pivot (xml)" and previous1 = "as" and previous2 = ")" and next1 in (",", "for").  For PIVOT clause.
			create or replace procedure test(a number) as begin for i in 1 .. 2 loop null; end loop; end;

			select *
			from (select 1 deptno, 'A' job, 100 sal from dual)
			pivot
			(
				sum(sal) as begin1, sum(sal) as begin
				for deptno
				in  (1,2)
			);
		Exclude when command_name in ('ALTER TABLE', 'CREATE TABLE') and previous1 = "as" and previous2 = "store".  For nested_table_col_properties.
			create type type1 is table of number;
			create table test1
			(
				a type1
			)
			nested table a store as begin;

			create or replace procedure store as begin null end;

		Documentation bug: For XMLnamespaces_clause (in XMLELEMENT) there must be a comma between "string AS identifier"
			and "DEFAULT string".  Although the documentation implies " 'A' as begin default 'B' " is valid it is NOT.
			It must be " 'A' as begin, default 'B' ", which is handled by above rules.
	- Note: These rules were determined by downloading and searching the BNF descriptions like this: findstr /i /s "as" *.*
		and findstr /i /s "statement" *.*
*/
procedure detect_begin(
	p_tokens in token_table,
	p_token_index in number,
	p_command_name in varchar2,
	v_previous_concrete_token_1 in out nocopy token,
	v_previous_concrete_token_2 in out nocopy token,
	v_has_entered_block in out boolean,
	v_block_counter in out number,
	v_pivot_paren_counter in number,
	v_prev_conc_tok_was_real_begin in out boolean,
	v_has_nested_table in boolean,
	p_trigger_body_start_index in out number
) is
begin
	if
	p_tokens(p_token_index).type = 'word'
	and
	lower(p_tokens(p_token_index).value) = 'begin'
	and
	(
		--Normal rules.
		(
			(
				p_command_name <> 'CREATE TRIGGER'
				or
				p_trigger_body_start_index is null
			)
			and
			(
				(
					lower(v_previous_concrete_token_1.value) in ('as', 'is', ';', '>>', 'then', 'else', 'loop')
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
			--Ignore "as begin" if it's used as an alias.
			and not
			(
				lower(v_previous_concrete_token_1.value) = 'as'
				and
				get_next_concrete_value_n(p_tokens, p_token_index, 1) in (',', 'from', 'into', ')', 'columns')
			)
			--Ignore "as begin" if it's used in bulk collect.
			and not
			(
				lower(v_previous_concrete_token_1.value) = 'as'
				and
				lower(get_next_concrete_value_n(p_tokens, p_token_index, 1)) in ('bulk')
				and
				lower(get_next_concrete_value_n(p_tokens, p_token_index, 2)) in ('collect')
			)
			--Ignore "as begin" if it's used in a PIVOT
			and not
			(
				v_pivot_paren_counter > 0
				and
				lower(v_previous_concrete_token_1.value) = 'as'
				and
				lower(v_previous_concrete_token_2.value) = ')'
				and
				lower(get_next_concrete_value_n(p_tokens, p_token_index, 1)) in (',', 'for')
			)
			--Ignore "as begin" if it's used in a nested table "... store as begin".
			and not
			(
				v_has_nested_table
				and
				lower(v_previous_concrete_token_2.value) = 'store'
				and
				lower(v_previous_concrete_token_1.value) = 'as'
			)
		)
		or
		--Trigger rules.
		(
			--Count BEGIN if it's after the starting index.
			p_command_name = 'CREATE TRIGGER'
			and
			p_token_index >= p_trigger_body_start_index
		)
	)
	then
		v_has_entered_block := true;
		v_block_counter := v_block_counter + 1;
		v_prev_conc_tok_was_real_begin := true;
		--Reset this, only the first BEGIN is treated differently.
		p_trigger_body_start_index := null;
	--If token is concrete, reset the flag.
	elsif p_tokens(p_token_index).type not in ('whitespace', 'comment', 'EOF') then
		v_prev_conc_tok_was_real_begin := false;
	end if;

end detect_begin;


--------------------------------------------------------------------------------
/*
END must come after ";", or as part of a trigger timing point.
	It cannot come after ">>", labels can't go there without compilation error.
	end could be an object, but the object will be invalid so things won't compile
Note: Special case of "END" in empty package is handled elsewhere.
*/
procedure detect_end(
	p_tokens in token_table,
	p_token_index in number,
	v_previous_concrete_token_1 in out nocopy token,
	v_previous_concrete_token_2 in out nocopy token,
	v_previous_concrete_token_3 in out nocopy token,
	v_previous_concrete_token_4 in out nocopy token,
	v_previous_concrete_token_5 in out nocopy token,
	v_block_counter in out number
) is
begin
	if
	p_tokens(p_token_index).type = ';'
	and
	(
		--Regular "end;".
		(
			lower(v_previous_concrete_token_1.value) = 'end'
			and
			lower(v_previous_concrete_token_2.type) = ';'
		)
		or
		--End with optional name: "end object_name;".
		--But "object_name" cannot be "if", "loop", or "case".
		(
			v_previous_concrete_token_1.type = 'word'
			and
			lower(v_previous_concrete_token_2.value) = 'end'
			and
			lower(v_previous_concrete_token_3.type) = ';'
			and
			lower(v_previous_concrete_token_1.value) not in ('if', 'loop', 'case')
		)
		or
		--Trigger timing points end.
		--One of: before statement, before each row, after statement, after each row, instead of each row
		(
			(
				lower(v_previous_concrete_token_3.value) = 'end'
				and
				lower(v_previous_concrete_token_2.value) = 'before'
				and
				lower(v_previous_concrete_token_1.value) = 'statement'
			)
			or
			(
				lower(v_previous_concrete_token_4.value) = 'end'
				and
				lower(v_previous_concrete_token_3.value) = 'before'
				and
				lower(v_previous_concrete_token_2.value) = 'each'
				and
				lower(v_previous_concrete_token_1.value) = 'row'
			)
			or
			(
				lower(v_previous_concrete_token_3.value) = 'end'
				and
				lower(v_previous_concrete_token_2.value) = 'after'
				and
				lower(v_previous_concrete_token_1.value) = 'statement'
			)
			or
			(
				lower(v_previous_concrete_token_4.value) = 'end'
				and
				lower(v_previous_concrete_token_3.value) = 'after'
				and
				lower(v_previous_concrete_token_2.value) = 'each'
				and
				lower(v_previous_concrete_token_1.value) = 'row'
			)
			or
			(
				lower(v_previous_concrete_token_5.value) = 'end'
				and
				lower(v_previous_concrete_token_4.value) = 'instead'
				and
				lower(v_previous_concrete_token_3.value) = 'of'
				and
				lower(v_previous_concrete_token_2.value) = 'each'
				and
				lower(v_previous_concrete_token_1.value) = 'row'
			)
		)
	) then
		v_block_counter := v_block_counter - 1;
	end if;
end detect_end;


--------------------------------------------------------------------------------
procedure add_statement_consume_tokens(
	p_split_tokens in out nocopy token_table_table,
	p_old_tokens in token_table,
	p_terminator number,
	p_new_tokens in out nocopy token_table,
	p_token_index in out number,
	p_command_name in varchar2,
	p_trigger_body_start_index in number
) is
	v_previous_concrete_token_1 token := token(null, null, null, null, null, null, null, null);
	v_previous_concrete_token_2 token := token(null, null, null, null, null, null, null, null);
	v_previous_concrete_token_3 token := token(null, null, null, null, null, null, null, null);
	v_previous_concrete_token_4 token := token(null, null, null, null, null, null, null, null);
	v_previous_concrete_token_5 token := token(null, null, null, null, null, null, null, null);


	---------------------------------------
	--Shift tokens if the new token is not whitespace.
	procedure shift_tokens_if_not_ws(
		p_new_token in token,
		p_previous_concrete_token_1 in out nocopy token,
		p_previous_concrete_token_2 in out nocopy token,
		p_previous_concrete_token_3 in out nocopy token,
		p_previous_concrete_token_4 in out nocopy token,
		p_previous_concrete_token_5 in out nocopy token
	) is
	begin
		if p_new_token.type not in ('whitespace', 'comment') then
			p_previous_concrete_token_5 := p_previous_concrete_token_4;
			p_previous_concrete_token_4 := p_previous_concrete_token_3;
			p_previous_concrete_token_3 := p_previous_concrete_token_2;
			p_previous_concrete_token_2 := p_previous_concrete_token_1;
			p_previous_concrete_token_1 := p_new_token;
		end if;
	end shift_tokens_if_not_ws;

	---------------------------------------
	--Count pivot parentheses.
	procedure set_pivot_paren_counter(
		v_pivot_paren_counter in out number,
		v_previous_concrete_token_1 in token,
		v_previous_concrete_token_2 in token,
		v_previous_concrete_token_3 in token
	) is
	begin
		--Initialize, if it's not already initialized and it's in a "pivot xml? (".
		if
		(
			v_pivot_paren_counter = 0
			and
			(
				(
					v_previous_concrete_token_1.value = '('
					and
					lower(v_previous_concrete_token_2.value) = 'pivot'
				)
				or
				(
					v_previous_concrete_token_1.value = '('
					and
					lower(v_previous_concrete_token_2.value) = 'xml'
					and
					lower(v_previous_concrete_token_3.value) = 'pivot'
				)
			)
		) then
			v_pivot_paren_counter := 1;
		--Increment, if it's in a PIVOT and a "(" is found.
		elsif
		(
			v_pivot_paren_counter > 0
			and
			p_old_tokens(p_token_index).value = '('
		) then
			v_pivot_paren_counter := v_pivot_paren_counter + 1;
		--Decrement, if it's in a PIVOT and a ")" is found.
		elsif
		(
			v_pivot_paren_counter > 0
			and
			p_old_tokens(p_token_index).value = ')'
		) then
			v_pivot_paren_counter := v_pivot_paren_counter - 1;
		end if;
	end set_pivot_paren_counter;

	---------------------------------------
	--Track the first "IS" or "AS" and handle an empty package or empty package body.
	procedure track_first_isas_and_empty_pkg(
		p_is_past_first_is_or_as in out boolean,
		p_exit_loop out boolean
	) is
	begin
		--Do not exit the outer loop by default.
		p_exit_loop := false;

		--Detect the first IS or AS, and possibly an empty package.
		if not p_is_past_first_is_or_as and lower(p_old_tokens(p_token_index).value) in ('is', 'as') then
			p_is_past_first_is_or_as := true;

			--Consume first "is" or "as" and shift tokens.
			p_token_index := p_token_index + 1;
			p_new_tokens.extend;
			p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);
			shift_tokens_if_not_ws(p_old_tokens(p_token_index), v_previous_concrete_token_1, v_previous_concrete_token_2, v_previous_concrete_token_3, v_previous_concrete_token_4, v_previous_concrete_token_5);

			--Return when empty package.
			--Special case where the next concrete token is END.
			declare
				v_next_concrete_value1 nvarchar2(32767);
				v_next_concrete_value2 nvarchar2(32767);
				v_next_concrete_value3 nvarchar2(32767);
			begin
				v_next_concrete_value1 := get_next_concrete_value_n(p_old_tokens, p_token_index, 1);
				v_next_concrete_value2 := get_next_concrete_value_n(p_old_tokens, p_token_index, 2);
				v_next_concrete_value3 := get_next_concrete_value_n(p_old_tokens, p_token_index, 3);

				--Consume tokens and exit if an END is found.
				if
				(
					--Regular "END;".
					(
						lower(v_next_concrete_value1) = 'end'
						and
						lower(v_next_concrete_value2) = ';'
					)
					or
					(
						lower(v_next_concrete_value1) = 'end'
						and
						lower(v_next_concrete_value3) = ';'
					)
				) then
					--Consume all tokens until the final ";".
					loop
						p_token_index := p_token_index + 1;
						p_new_tokens.extend;
						p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);
						exit when p_old_tokens(p_token_index).type = ';';
					end loop;
					p_token_index := p_token_index + 1;
					p_exit_loop := true;
				end if;
			end;
		end if;
	end track_first_isas_and_empty_pkg;

begin
	--Consume everything
	if p_terminator = C_TERMINATOR_EOF then
		--Consume all tokens.
		loop
			exit when p_token_index > p_old_tokens.count;
			p_new_tokens.extend;
			p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);
			p_token_index := p_token_index + 1;
		end loop;
	--Look for a ';' anywhere.
	elsif p_terminator = C_TERMINATOR_SEMI then
		--Build new statement and count tokens.
		loop
			--Increment.
			exit when p_token_index > p_old_tokens.count;
			p_new_tokens.extend;
			p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);

			--Detect end of statement.
			if p_old_tokens(p_token_index).type = ';' or p_old_tokens(p_token_index).type = 'EOF' then
				--Stop if no more tokens.
				if p_token_index = p_old_tokens.count then
					p_token_index := p_token_index + 1;
					exit;
				--Consume all tokens if only whitespace, comments, and EOF remain.
				elsif only_ws_comments_eof_remain(p_old_tokens, p_token_index+1) then
					--Consume all tokens.
					loop
						p_token_index := p_token_index + 1;
						exit when p_token_index > p_old_tokens.count;
						p_new_tokens.extend;
						p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);
					end loop;
					exit;
				--Otherwise stop at this spot.
				else
					p_token_index := p_token_index + 1;
					exit;
				end if;
			end if;

			p_token_index := p_token_index + 1;
		end loop;

	--Match BEGIN and END for a PLSQL_DECLARATION.
	elsif p_terminator = C_TERMINATOR_PLSQL_DECLARE_END then
		declare
			v_has_entered_block boolean := false;
			v_block_counter number := 0;
			v_pivot_paren_counter number := 0;
			v_prev_conc_tok_was_real_begin boolean := false;
			v_has_nested_table boolean := false;
			v_trigger_body_start_index number := p_trigger_body_start_index;
		begin
			--Build new statement and count tokens.
			loop
				--Increment
				exit when p_token_index > p_old_tokens.count;
				p_new_tokens.extend;
				p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);

				--Set the PIVOT parentheses counter.
				set_pivot_paren_counter(v_pivot_paren_counter, v_previous_concrete_token_1, v_previous_concrete_token_2, v_previous_concrete_token_3);

				--Set v_has_nested_table.
				if
					p_command_name in ('CREATE TABLE', 'ALTER TABLE')
					and
					lower(v_previous_concrete_token_2.value) = 'nested'
					and lower(v_previous_concrete_token_1.value) = 'table'
				then
					v_has_nested_table := true;
				end if;

				--Detect BEGIN and END.
				detect_begin(p_old_tokens, p_token_index, p_command_name, v_previous_concrete_token_1, v_previous_concrete_token_2, v_has_entered_block, v_block_counter, v_pivot_paren_counter, v_prev_conc_tok_was_real_begin, v_has_nested_table, v_trigger_body_start_index);
				detect_end(p_old_tokens, p_token_index, v_previous_concrete_token_1, v_previous_concrete_token_2, v_previous_concrete_token_3, v_previous_concrete_token_4, v_previous_concrete_token_5, v_block_counter);

				--Detect end of statement.
				if (v_has_entered_block and v_block_counter = 0) or p_old_tokens(p_token_index).type = 'EOF' then
					--Stop if no more tokens.
					if p_token_index = p_old_tokens.count then
						exit;
					--Consume all tokens if only whitespace, comments, and EOF remain.
					elsif only_ws_comments_eof_remain(p_old_tokens, p_token_index+1) then
						--Consume all tokens.
						loop
							p_token_index := p_token_index + 1;
							exit when p_token_index > p_old_tokens.count;
							p_new_tokens.extend;
							p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);
						end loop;
					--There could be more than one function.
					elsif has_another_plsql_declaration(p_old_tokens, p_token_index + 1) then
						p_token_index := p_token_index + 1;
						add_statement_consume_tokens(p_split_tokens, p_old_tokens, C_TERMINATOR_PLSQL_DECLARE_END, p_new_tokens, p_token_index, p_command_name, null);
						return;
					--Otherwise look for the next ';'.
					else
						p_token_index := p_token_index + 1;
						add_statement_consume_tokens(p_split_tokens, p_old_tokens, C_TERMINATOR_SEMI, p_new_tokens, p_token_index, p_command_name, null);
						return;
					end if;
				end if;

				--Shift tokens.
				shift_tokens_if_not_ws(p_old_tokens(p_token_index), v_previous_concrete_token_1, v_previous_concrete_token_2,
					v_previous_concrete_token_3, v_previous_concrete_token_4, v_previous_concrete_token_5);

				--Increment
				p_token_index := p_token_index + 1;
			end loop;
		end;

	--Match BEGIN and END for a common PL/SQL block.
	elsif p_terminator = C_TERMINATOR_PLSQL_MATCHED_END then
		declare
			v_has_entered_block boolean := false;
			v_block_counter number := 0;
			v_pivot_paren_counter number := 0;
			v_prev_conc_tok_was_real_begin boolean := false;
			v_has_nested_table boolean := false;
			v_trigger_body_start_index number := p_trigger_body_start_index;
		begin
			--Build new statement and count tokens.
			loop
				--Increment
				exit when p_token_index > p_old_tokens.count;
				p_new_tokens.extend;
				p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);

				--Set the PIVOT parentheses counter.
				set_pivot_paren_counter(v_pivot_paren_counter, v_previous_concrete_token_1, v_previous_concrete_token_2, v_previous_concrete_token_3);

				--Set v_has_nested_table.
				if
					p_command_name in ('CREATE TABLE', 'ALTER TABLE')
					and
					lower(v_previous_concrete_token_2.value) = 'nested'
					and
					lower(v_previous_concrete_token_1.value) = 'table'
				then
					v_has_nested_table := true;
				end if;

				--Detect BEGIN and END.
				detect_begin(p_old_tokens, p_token_index, p_command_name, v_previous_concrete_token_1, v_previous_concrete_token_2, v_has_entered_block, v_block_counter, v_pivot_paren_counter, v_prev_conc_tok_was_real_begin, v_has_nested_table, v_trigger_body_start_index);
				detect_end(p_old_tokens, p_token_index, v_previous_concrete_token_1, v_previous_concrete_token_2, v_previous_concrete_token_3, v_previous_concrete_token_4, v_previous_concrete_token_5, v_block_counter);

				--Detect end of statement.
				if (v_has_entered_block and v_block_counter = 0) or p_old_tokens(p_token_index).type = 'EOF' then
					--Consume all tokens if only whitespace, comments, and EOF remain.
					if only_ws_comments_eof_remain(p_old_tokens, p_token_index+1) then
						--Consume all tokens.
						loop
							p_token_index := p_token_index + 1;
							exit when p_token_index > p_old_tokens.count;
							p_new_tokens.extend;
							p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);
						end loop;
						exit;
					--Else stop here.
					else
						p_token_index := p_token_index + 1;
						exit;
					end if;
				end if;

				--Shift tokens.
				shift_tokens_if_not_ws(p_old_tokens(p_token_index), v_previous_concrete_token_1, v_previous_concrete_token_2,
					v_previous_concrete_token_3, v_previous_concrete_token_4, v_previous_concrete_token_5);

				--Increment
				p_token_index := p_token_index + 1;
			end loop;
		end;

	--Match BEGIN and END for a PL/SQL statement that has an extra END.
	elsif p_terminator = C_TERMINATOR_PLSQL_EXTRA_END then
		--This is almost identical to C_TERMINATOR_PLSQL_MATCHED_END.
		--The only difference is this code expects an extra block.
		--TODO: Refactor for DRY.
		declare
			v_has_entered_block boolean := false;
			v_block_counter number := 1;  --Start at 1, an extra END is required.
			v_pivot_paren_counter number := 0;
			v_prev_conc_tok_was_real_begin boolean := false;
			v_has_nested_table boolean := false;
			v_trigger_body_start_index number := p_trigger_body_start_index;

			v_is_past_first_is_or_as boolean := false;
			v_exit_loop boolean := false;
		begin
			--Build new statement and count tokens.
			loop
				--Increment
				exit when p_token_index > p_old_tokens.count;
				p_new_tokens.extend;
				p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);

				--Detect the first IS or AS, and possibly an empty package.
				track_first_isas_and_empty_pkg(v_is_past_first_is_or_as, v_exit_loop);
				if v_exit_loop then
					exit;
				end if;

				--Set the PIVOT parentheses counter.
				set_pivot_paren_counter(v_pivot_paren_counter, v_previous_concrete_token_1, v_previous_concrete_token_2, v_previous_concrete_token_3);

				--Set v_has_nested_table.
				if
					p_command_name in ('CREATE TABLE', 'ALTER TABLE')
					and
					lower(v_previous_concrete_token_2.value) = 'nested'
					and
					lower(v_previous_concrete_token_1.value) = 'table'
				then
					v_has_nested_table := true;
				end if;

				--Detect BEGIN and END.
				detect_begin(p_old_tokens, p_token_index, p_command_name, v_previous_concrete_token_1, v_previous_concrete_token_2, v_has_entered_block, v_block_counter, v_pivot_paren_counter, v_prev_conc_tok_was_real_begin, v_has_nested_table, v_trigger_body_start_index);
				detect_end(p_old_tokens, p_token_index, v_previous_concrete_token_1, v_previous_concrete_token_2, v_previous_concrete_token_3, v_previous_concrete_token_4, v_previous_concrete_token_5, v_block_counter);

				--Detect end of statement.
				if v_block_counter = 0 or p_old_tokens(p_token_index).type = 'EOF' then
					--Consume all tokens if only whitespace, comments, and EOF remain.
					if only_ws_comments_eof_remain(p_old_tokens, p_token_index+1) then
						--Consume all tokens.
						loop
							p_token_index := p_token_index + 1;
							exit when p_token_index > p_old_tokens.count;
							p_new_tokens.extend;
							p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);
						end loop;
						exit;
					--Else stop here.
					else
						p_token_index := p_token_index + 1;
						exit;
					end if;
				end if;

				--Shift tokens.
				shift_tokens_if_not_ws(p_old_tokens(p_token_index), v_previous_concrete_token_1, v_previous_concrete_token_2,
					v_previous_concrete_token_3, v_previous_concrete_token_4, v_previous_concrete_token_5);

				--Increment
				p_token_index := p_token_index + 1;
			end loop;
		end;

	--Match BEGIN and END for a PL/SQL statement that *may* have an extra END.
	elsif p_terminator = C_TERMINATOR_PACKAGE_BODY then
		--See the unit tests for a good set of examples of the 5 different types of package bodies.

		--This is similar to C_TERMINATOR_PLSQL_MATCHED_END.
		--TODO: Refactor for DRY.
		declare
			v_has_entered_block boolean := false;
			v_block_counter number := 1;  --Start at 1, an extra END is required.
			v_pivot_paren_counter number := 0;
			v_prev_conc_tok_was_real_begin boolean := false;

			v_is_past_first_is_or_as boolean := false;
			v_exit_loop boolean := false;

			--Not needed, only used because it's required by detect_begin.
			v_has_nested_table boolean := false;
			v_trigger_body_start_index number := p_trigger_body_start_index;
		begin
			--Build new statement and count tokens.
			loop
				--Increment
				exit when p_token_index > p_old_tokens.count;
				p_new_tokens.extend;
				p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);

				--Detect the first IS or AS, and possibly an empty package.
				track_first_isas_and_empty_pkg(v_is_past_first_is_or_as, v_exit_loop);
				if v_exit_loop then
					exit;
				end if;

				--Start looking for procedure|function|cursor|begin|end after the first IS|AS was found
				if v_is_past_first_is_or_as then

					--Loop until matching END; then continue.
					--For non-external procedures or functions, or cursor with plsql_declaration.
					if
					(
						(
							(
								lower(p_old_tokens(p_token_index).value) in ('procedure', 'function')
								and
								--Cursor with multiple CTEs could have one named "function as (select ...",
								--which is not a real function.
								lower(get_next_concrete_value_n(p_old_tokens, p_token_index, 1)) <> 'as'
							)
							and
							not is_external_method(p_old_tokens, p_token_index)
						)
						or
						(
							--TODO: What about multiple functions/procedures?
							--What about a second CTE named "function as (select 1 a from dual)"?
							lower(p_old_tokens(p_token_index).value) in ('cursor')
							and
							has_plsql_declaration(p_old_tokens, p_token_index)
						)
					)
					then
						--Consume until matching END.
						declare
							v_has_entered_sub_block boolean := false;
							v_sub_block_counter number := 0;
							v_prev_conc_tok_real_begin_sub boolean := false;
						begin
							loop
								--Consume.
								p_token_index := p_token_index + 1;
								p_new_tokens.extend;
								p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);

								--Detect begin and end.
								detect_begin(p_old_tokens, p_token_index, p_command_name, v_previous_concrete_token_1, v_previous_concrete_token_2, v_has_entered_sub_block, v_sub_block_counter, v_pivot_paren_counter, v_prev_conc_tok_real_begin_sub, v_has_nested_table, v_trigger_body_start_index);
								detect_end(p_old_tokens, p_token_index, v_previous_concrete_token_1, v_previous_concrete_token_2, v_previous_concrete_token_3, v_previous_concrete_token_4, v_previous_concrete_token_5, v_sub_block_counter);

								--Stop looking when block is over or out of tokens.
								exit when
								(
									p_token_index = p_old_tokens.count
									or
									(
										v_has_entered_sub_block
										and
										v_sub_block_counter = 0
									)
								);

								--Shift.
								shift_tokens_if_not_ws(p_old_tokens(p_token_index), v_previous_concrete_token_1, v_previous_concrete_token_2, v_previous_concrete_token_3, v_previous_concrete_token_4, v_previous_concrete_token_5);
							end loop;
						end;

					--Ignore non-concrete tokens.
					elsif p_old_tokens(p_token_index).type in ('whitespace', 'comment', 'EOF') then
						null;

					--Ignore labels.
					--Putting a label between procedures is illegal but parsable.
					elsif p_old_tokens(p_token_index).type = '<<' then
							--Consume all tokens until the final ";".
							loop
								p_token_index := p_token_index + 1;
								p_new_tokens.extend;
								p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);
								shift_tokens_if_not_ws(p_old_tokens(p_token_index), v_previous_concrete_token_1, v_previous_concrete_token_2, v_previous_concrete_token_3, v_previous_concrete_token_4, v_previous_concrete_token_5);
								exit when p_old_tokens(p_token_index).type = '>>';
							end loop;

					--Loop until matching END; and return.
					--For BEGIN.
					elsif lower(p_old_tokens(p_token_index).value) = 'begin' then
						--Consume until matching END.
						declare
							v_has_entered_sub_block boolean := true;
							v_sub_block_counter number := 1;
							v_prev_conc_tok_real_begin_sub boolean := true;
						begin
							loop
								--Consume.
								p_token_index := p_token_index + 1;
								p_new_tokens.extend;
								p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);

								--Detect begin and end.
								detect_begin(p_old_tokens, p_token_index, p_command_name, v_previous_concrete_token_1, v_previous_concrete_token_2, v_has_entered_sub_block, v_sub_block_counter, v_pivot_paren_counter, v_prev_conc_tok_real_begin_sub, v_has_nested_table, v_trigger_body_start_index);
								detect_end(p_old_tokens, p_token_index, v_previous_concrete_token_1, v_previous_concrete_token_2, v_previous_concrete_token_3, v_previous_concrete_token_4, v_previous_concrete_token_5, v_sub_block_counter);

								--Stop looking when block is over or out of tokens.
								exit when
								(
									p_token_index = p_old_tokens.count
									or
									(
										v_has_entered_sub_block
										and
										v_sub_block_counter = 0
									)
								);

								--Shift.
								shift_tokens_if_not_ws(p_old_tokens(p_token_index), v_previous_concrete_token_1, v_previous_concrete_token_2, v_previous_concrete_token_3, v_previous_concrete_token_4, v_previous_concrete_token_5);
							end loop;
						end;
						p_token_index := p_token_index + 1;
						exit;

					--Process END and return.
					elsif lower(p_old_tokens(p_token_index).value) in ('end') then
						loop
							p_token_index := p_token_index + 1;
							p_new_tokens.extend;
							p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);
							exit when p_token_index = p_old_tokens.count or p_old_tokens(p_token_index).type = ';';
						end loop;
						p_token_index := p_token_index + 1;
						exit;

					--Loop until next semicolon and continue.
					--For external functions and procedures, types, items, pragmas, end, and cursors without plsql_declarations.
					else
						loop
							p_token_index := p_token_index + 1;
							p_new_tokens.extend;
							p_new_tokens(p_new_tokens.count) := p_old_tokens(p_token_index);
							exit when p_token_index = p_old_tokens.count or p_old_tokens(p_token_index).type = ';';
						end loop;
					end if;

				end if;

				--Shift tokens.
				shift_tokens_if_not_ws(p_old_tokens(p_token_index), v_previous_concrete_token_1, v_previous_concrete_token_2, v_previous_concrete_token_3, v_previous_concrete_token_4, v_previous_concrete_token_5);

				--Increment
				p_token_index := p_token_index + 1;
			end loop;
		end;

	end if;

	p_split_tokens.extend;
	p_split_tokens(p_split_tokens.count) := p_new_tokens;
end add_statement_consume_tokens;


--------------------------------------------------------------------------------
--Split a string into separate strings by an optional delmiter, usually "/".
--This follows the SQL*Plus rules - the delimiter must be on a line by itself,
--although the line may contain whitespace before and after the delimiter.
--The delimiter and whitespace on the same line are included with the first statement.
function split_by_sqlplus_delimiter(p_statements in nclob, p_sqlplus_delimiter in nvarchar2 default '/') return nclob_table is
	v_chars nvarchar2_table := tokenizer.get_nvarchar2_table_from_nclob(p_statements);
	v_delimiter_size number := nvl(lengthc(p_sqlplus_delimiter), 0);
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

	--Check if there are only whitespace characters before the next newline
	function only_ws_before_next_newline return boolean is
	begin
		--Loop through the characters.
		for i in v_char_index + v_delimiter_size .. v_chars.count loop
			--TRUE if a newline is found.
			if v_chars(i) = chr(10) then
				return true;
			--False if non-whitespace is found.
			elsif not tokenizer.is_lexical_whitespace(v_chars(i)) then
				return false;
			end if;
		end loop;

		--True if neither a newline or a non-whitespace was found.
		return true;
	end only_ws_before_next_newline;
begin
	--Special cases.
	--
	--Throw an error if the delimiter is null.
	if p_sqlplus_delimiter is null then
		raise_application_error(-20000, 'The SQL*Plus delimiter cannot be NULL.');
	end if;
	--Throw an error if the delimiter contains whitespace.
	for i in 1 .. lengthc(p_sqlplus_delimiter) loop
		if tokenizer.is_lexical_whitespace(substrc(p_sqlplus_delimiter, i, 1)) then
			raise_application_error(-20001, 'The SQL*Plus delimiter cannot contain whitespace.');
		end if;
	end loop;
	--Return an empty string if the string is NULL.
	if p_statements is null then
		v_strings.extend;
		v_strings(v_strings.count) := p_statements;
		return v_strings;
	end if;

	--Loop through characters and build strings.
	loop
		v_char_index := v_char_index + 1;

		--Look for delimiter if it's on an empty line.
		if v_is_empty_line then
			--Add char, push, and exit if it's the last character.
			if v_char_index = v_chars.count then
				v_string := v_string || v_chars(v_char_index);
				v_strings.extend;
				v_strings(v_strings.count) := v_string;
				exit;
			--Continue if it's still whitespace.
			elsif tokenizer.is_lexical_whitespace(v_chars(v_char_index)) then
				v_string := v_string || v_chars(v_char_index);
			--Split string if delimiter is found.
			elsif get_next_n_chars(v_delimiter_size) = p_sqlplus_delimiter and only_ws_before_next_newline then
				--Consume delimiter.
				for i in 1 .. v_delimiter_size loop
					v_string := v_string || v_chars(v_char_index);
					v_char_index := v_char_index + 1;
				end loop;

				--Consume all tokens until either end of string or next character is non-whitespace.
				loop
					v_string := v_string || v_chars(v_char_index);
					v_char_index := v_char_index + 1;
					exit when v_char_index = v_chars.count or not tokenizer.is_lexical_whitespace(v_chars(v_char_index));
				end loop;

				--Remove extra increment.
				v_char_index := v_char_index - 1;

				--Add string and start over.
				v_strings.extend;
				v_strings(v_strings.count) := v_string;
				v_string := null;
				v_is_empty_line := false;
			--It's no longer an empty line otherwise.
			else
				v_string := v_string || v_chars(v_char_index);
				v_is_empty_line := false;
			end if;
		--Add the string after the last character.
		elsif v_char_index >= v_chars.count then
			v_string := v_string || v_chars(v_char_index);
			v_strings.extend;
			v_strings(v_strings.count) := v_string;
			exit;
		--Look for newlines.
		elsif v_chars(v_char_index) = chr(10) then
			v_string := v_string || v_chars(v_char_index);
			v_is_empty_line := true;
		--Else just add the character.
		else
			v_string := v_string || v_chars(v_char_index);
		end if;
	end loop;

	return v_strings;
end split_by_sqlplus_delimiter;


--------------------------------------------------------------------------------
--Split a token stream into statements by ";".
function split_by_semicolon(p_tokens in token_table)
return token_table_table is
	v_split_tokens token_table_table := token_table_table();
	v_command_name varchar2(4000);
	v_temp_new_tokens token_table := token_table();
	v_token_index number := 1;
	v_trigger_type number;
	v_trigger_body_start_index number;
begin
	--Split into statements.
	loop
		v_temp_new_tokens := token_table();

		--Classify.
		declare
			v_throwaway_number number;
			v_throwaway_string varchar2(32767);
		begin
			statement_classifier.classify(
				p_tokens => p_tokens,
				p_category => v_throwaway_string,
				p_statement_type => v_throwaway_string,
				p_command_name => v_command_name,
				p_command_type => v_throwaway_number,
				p_lex_sqlcode => v_throwaway_number,
				p_lex_sqlerrm => v_throwaway_string,
				p_start_index => v_token_index
			);
		end;

		--Find a terminating token based on the classification.
		--
		--#1: Return everything with no splitting if the statement is Invalid or Nothing.
		--    These are probably errors but the application must decide how to handle them.
		if v_command_name in ('Invalid', 'Nothing') then
			add_statement_consume_tokens(v_split_tokens, p_tokens, C_TERMINATOR_EOF, v_temp_new_tokens, v_token_index, v_command_name, null);

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
		(
			v_command_name in ('CREATE MATERIALIZED VIEW ', 'CREATE SCHEMA', 'CREATE TABLE', 'CREATE VIEW', 'DELETE', 'EXPLAIN', 'INSERT', 'SELECT', 'UPDATE', 'UPSERT')
			and
			has_plsql_declaration(p_tokens, v_token_index)
		)
		then
			add_statement_consume_tokens(v_split_tokens, p_tokens, C_TERMINATOR_PLSQL_DECLARE_END, v_temp_new_tokens, v_token_index, v_command_name, null);

		--#4: Match PL/SQL BEGIN and END.
		elsif
		(
			v_command_name in ('PL/SQL EXECUTE')
			or
			(
				v_command_name in ('CREATE FUNCTION','CREATE PROCEDURE')
				and
				not is_external_method(p_tokens, v_token_index)
			)
		)
		 then
			add_statement_consume_tokens(v_split_tokens, p_tokens, C_TERMINATOR_PLSQL_MATCHED_END, v_temp_new_tokens, v_token_index, v_command_name, null);

		--#5: Match possibly unbalanced BEGIN and END.  Package bodies sometimes have an
		--extra END and sometimes they have multiple balanced BEGIN/ENDs.
		--
		--Ignore BEGIN/END pairs inside CURSOR/FUNCTION/PROCEDURE, and then exit
		--whenever end_count >= begin_count
		elsif v_command_name in ('CREATE PACKAGE BODY') then
			add_statement_consume_tokens(v_split_tokens, p_tokens, C_TERMINATOR_PACKAGE_BODY, v_temp_new_tokens, v_token_index, v_command_name, null);

		--#6: Stop when there is one "extra" END.
		elsif v_command_name in ('CREATE PACKAGE', 'CREATE TYPE BODY') then
			add_statement_consume_tokens(v_split_tokens, p_tokens, C_TERMINATOR_PLSQL_EXTRA_END, v_temp_new_tokens, v_token_index, v_command_name, null);

		--#7: Triggers may terminate with a matching END, an extra END, or a semicolon.
		elsif v_command_name in ('CREATE TRIGGER') then
			get_trigger_type_body_index(p_tokens, v_trigger_type, v_trigger_body_start_index);

			if v_trigger_type = C_REGULAR_TRIGGER then
				add_statement_consume_tokens(v_split_tokens, p_tokens, C_TERMINATOR_PLSQL_MATCHED_END, v_temp_new_tokens, v_token_index, v_command_name, v_trigger_body_start_index);
			elsif v_trigger_type = C_COMPOUND_TRIGGER then
				add_statement_consume_tokens(v_split_tokens, p_tokens, C_TERMINATOR_PLSQL_EXTRA_END, v_temp_new_tokens, v_token_index, v_command_name, v_trigger_body_start_index);
			elsif v_trigger_type = C_CALL_TRIGGER then
				add_statement_consume_tokens(v_split_tokens, p_tokens, C_TERMINATOR_SEMI, v_temp_new_tokens, v_token_index, v_command_name, v_trigger_body_start_index);
			end if;

		--#8: Stop at first ";" for everything else.
		else
			add_statement_consume_tokens(v_split_tokens, p_tokens, C_TERMINATOR_SEMI, v_temp_new_tokens, v_token_index, v_command_name, null);
		end if;

		--Quit when there are no more tokens.
		exit when v_token_index > p_tokens.count;
	end loop;

	--TODO: Fix line_number, column_number, first_char_position and last_char_position.

	return v_split_tokens;
end split_by_semicolon;


--------------------------------------------------------------------------------
--Split a string of separate SQL and PL/SQL statements terminated by ";" and
--some secondary terminator, usually "/".
function split_by_sqlplus_del_and_semi(p_statements in nclob, p_sqlplus_delimiter in nvarchar2 default '/')
return token_table_table is
	v_split_statements nclob_table := nclob_table();
	v_split_token_tables token_table_table := token_table_table();
begin
	--First split by SQL*Plus delimiter.
	v_split_statements := split_by_sqlplus_delimiter(p_statements, p_sqlplus_delimiter);

	--Split each string further by the primary terminator, ";".
	for i in 1 .. v_split_statements.count loop
		v_split_token_tables :=
			v_split_token_tables
			multiset union
			split_by_semicolon(tokenizer.tokenize(v_split_statements(i)));
	end loop;

	--Return the statements.
	return v_split_token_tables;
end split_by_sqlplus_del_and_semi;


end;
/
