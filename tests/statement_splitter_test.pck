create or replace package statement_splitter_test authid current_user is
/*
## Purpose ##

Unit tests for statement_splitter.


## Example ##

begin
	statement_splitter_test.run;
	statement_splitter_test.run(statement_splitter_test.c_dynamic_tests);
end;

*/
pragma serially_reusable;

--Globals to select which test suites to run.
c_errors             constant number := power(2, 1);
c_simple             constant number := power(2, 2);
c_optional_delimiter constant number := power(2, 3);
c_plsql_declaration  constant number := power(2, 4);
c_plsql_block        constant number := power(2, 5);

c_static_tests  constant number := c_errors+c_simple+c_optional_delimiter+c_plsql_declaration+c_plsql_block;

c_dynamic_tests constant number := power(2, 30);

c_all_tests constant number := c_static_tests+c_dynamic_tests;

--Run the unit tests and display the results in dbms output.
procedure run(p_tests number default c_static_tests);

end;
/
create or replace package body statement_splitter_test is
pragma serially_reusable;

--Global counters.
g_test_count number := 0;
g_passed_count number := 0;
g_failed_count number := 0;

--Global types
type output_rec is record
(
	category varchar2(100),
	statement_type varchar2(100),
	command_name varchar2(64),
	command_type number,
	lex_sqlcode number,
	lex_sqlerrm varchar2(4000),
	fatal_error varchar2(4000)
);


-- =============================================================================
-- Helper procedures.
-- =============================================================================

--------------------------------------------------------------------------------
procedure assert_equals(p_test nvarchar2, p_expected nvarchar2, p_actual nvarchar2) is
begin
	g_test_count := g_test_count + 1;

	if p_expected = p_actual or p_expected is null and p_actual is null then
		g_passed_count := g_passed_count + 1;
	else
		g_failed_count := g_failed_count + 1;
		dbms_output.put_line('Failure with '||p_test);
		dbms_output.put_line('Expected: '||p_expected);
		dbms_output.put_line('Actual  : '||p_actual);
	end if;
end assert_equals;


-- =============================================================================
-- Test Suites
-- =============================================================================

--------------------------------------------------------------------------------
procedure test_errors is
begin
	--TODO
	null;
end test_errors;


--------------------------------------------------------------------------------
procedure test_simple is
	v_statements nclob;
	v_split_statements nclob_table := nclob_table();
begin
	v_statements:='select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('No split 1', v_statements, v_split_statements(1));
	v_statements:='select * from dual';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('No split 2', v_statements, v_split_statements(1));

	v_statements:='select * from dual a;select * from dual b;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Simple split 1a', 'select * from dual a;', v_split_statements(1));
	assert_equals('Simple split 1b', 'select * from dual b;', v_split_statements(2));

	v_statements:='select * from dual a; select * from dual b; ';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Simple split 2a', 'select * from dual a;', v_split_statements(1));
	assert_equals('Simple split 2b', ' select * from dual b; ', v_split_statements(2));
	assert_equals('Simple split 2c', 2, v_split_statements.count);

	--TODO
	null;
end test_simple;


--------------------------------------------------------------------------------
procedure test_optional_delimiter is
	v_statements nclob;
	v_split_statements nclob_table := nclob_table();
	c_slash constant varchar2(1) := '/';
begin
	--Invalid SQL, but should only be one line.
	v_statements:='select * from dual/';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Slash 1a', '1', v_split_statements.count);
	assert_equals('Slash 1b', v_statements, v_split_statements(1));

	--Valid SQL, not split.
	v_statements:='select * from dual'||chr(10)||'/';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Slash 2a', '1', v_split_statements.count);
	assert_equals('Slash 2b', 'select * from dual'||chr(10), v_split_statements(1));

	--Valid SQL, split in two.
	v_statements:='select * from dual a'||chr(10)||' 	/	 '||chr(10)||'select * from dual b';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Slash 3a', '2', v_split_statements.count);
	assert_equals('Slash 3b', 'select * from dual a'||chr(10)||' 	', v_split_statements(1));
	assert_equals('Slash 3c', '	 '||chr(10)||'select * from dual b', v_split_statements(2));

	--Valid SQL, split in three.
	v_statements:='select * from dual a'||chr(10)||' 	/	 '||chr(10)||'select * from dual b; select * from dual c';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Slash 4a', '3', v_split_statements.count);
	assert_equals('Slash 4b', 'select * from dual a'||chr(10)||' 	', v_split_statements(1));
	assert_equals('Slash 4c', '	 '||chr(10)||'select * from dual b;', v_split_statements(2));
	assert_equals('Slash 4c', ' select * from dual c', v_split_statements(3));

	--Valid SQL, split in two with a custom delimiter.
	v_statements:='select * from dual a'||chr(10)||' 	#	 '||chr(10)||'select * from dual b';v_split_statements:=statement_splitter.split(v_statements, '#');
	assert_equals('Slash 5a', '2', v_split_statements.count);
	assert_equals('Slash 5b', 'select * from dual a'||chr(10)||' 	', v_split_statements(1));
	assert_equals('Slash 5c', '	 '||chr(10)||'select * from dual b', v_split_statements(2));

	--Valid SQL, split in two with a custom multi-character delimiter.
	--TODO: This shows up as two different tokens - we must collapse them somehow.
	v_statements:='select * from dual a'||chr(10)||' 	$$	 '||chr(10)||'select * from dual b';v_split_statements:=statement_splitter.split(v_statements, '$$');
	assert_equals('Slash 6a', '2', v_split_statements.count);
	assert_equals('Slash 6b', 'select * from dual a'||chr(10)||' 	', v_split_statements(1));
--	assert_equals('Slash 6c', '	 '||chr(10)||'select * from dual b', v_split_statements(2));




/*
	--Split into two.  Slash on line with just whitespace - spaces, tabs, newlines.
	select * from dual a

	  /  

	select * from dual b
	--Not split, newline is not on a line by itself
	select * from dual a
	/ --bad comment
	select * from dual b
	--Not split, newline is not on a line by itself
	select * from dual a
	/* bad comment *SLASH /
	select * from dual b
*/
end test_optional_delimiter;


--------------------------------------------------------------------------------
procedure test_plsql_declaration is
	v_statements nclob;
	v_split_statements nclob_table := nclob_table();
begin
	v_statements:='with function f return number is begin return 1; end; function g return number is begin return 2; end; select f from dual;select 1 from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 1a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 1b', 'with function f return number is begin return 1; end; function g return number is begin return 2; end; select f from dual;', v_split_statements(1));
	assert_equals('plsql_declaration 1c', 'select 1 from dual;', v_split_statements(2));

	v_statements:='with function f return number is begin return 1; end; function g return number is begin return 2; end; h as (select 1 a from dual) select f from dual;select 1 from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 2a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 2b', 'with function f return number is begin return 1; end; function g return number is begin return 2; end; h as (select 1 a from dual) select f from dual;', v_split_statements(1));
	assert_equals('plsql_declaration 2c', 'select 1 from dual;', v_split_statements(2));

	v_statements:='with function f return number is begin return 1; end; function g return number is begin return 2; end; h(a) as (select 1 a from dual) select f from dual;select 1 from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 3a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 3b', 'with function f return number is begin return 1; end; function g return number is begin return 2; end; h(a) as (select 1 a from dual) select f from dual;', v_split_statements(1));
	assert_equals('plsql_declaration 3c', 'select 1 from dual;', v_split_statements(2));

	v_statements:='with function f return number is begin return 1; end; function g return number is begin return 2; end; h as (select 1 a from dual), i as (select 1 a from dual) select f from dual;select 1 from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 4a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 4b', 'with function f return number is begin return 1; end; function g return number is begin return 2; end; h as (select 1 a from dual), i as (select 1 a from dual) select f from dual;', v_split_statements(1));
	assert_equals('plsql_declaration 4c', 'select 1 from dual;', v_split_statements(2));

	v_statements:='with function f return number is begin return 1; end; procedure g is begin null; end; h as (select 1 a from dual), i as (select 1 a from dual) select f from dual;select 1 from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 5a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 5b', 'with function f return number is begin return 1; end; procedure g is begin null; end; h as (select 1 a from dual), i as (select 1 a from dual) select f from dual;', v_split_statements(1));
	assert_equals('plsql_declaration 5c', 'select 1 from dual;', v_split_statements(2));

	v_statements:='with function f return number is begin return 1; end; function g return number is begin return 2; end; function as (select 1 a from dual) select f from dual;select 1 from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 6a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 6b', 'with function f return number is begin return 1; end; function g return number is begin return 2; end; function as (select 1 a from dual) select f from dual;', v_split_statements(1));
	assert_equals('plsql_declaration 6c', 'select 1 from dual;', v_split_statements(2));

	v_statements:='with function f return number is begin return 1; end; function g return number is begin return 2; end; function(a) as (select 1 a from dual) select f from dual;select 1 from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 7a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 7b', 'with function f return number is begin return 1; end; function g return number is begin return 2; end; function(a) as (select 1 a from dual) select f from dual;', v_split_statements(1));
	assert_equals('plsql_declaration 7c', 'select 1 from dual;', v_split_statements(2));
end test_plsql_declaration;


--------------------------------------------------------------------------------
procedure test_plsql_block is
	v_statements nclob;
	v_split_statements nclob_table := nclob_table();
begin
	v_statements:='declare v_test number; begin select begin begin into v_test from (select 1 begin from dual); end; select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_block: begin begin does not start a block 1a', 2, v_split_statements.count);
	assert_equals('plsql_block: begin begin does not start a block 1b', 'declare v_test number; begin select begin begin into v_test from (select 1 begin from dual); end;', v_split_statements(1));
	assert_equals('plsql_block: begin begin does not start a block 1c', ' select * from dual;', v_split_statements(2));

	v_statements:='select begin begin into v_test from (select 1 begin from dual); select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_block: begin begin does not start a block 2a', 2, v_split_statements.count);
	assert_equals('plsql_block: begin begin does not start a block 2b', 'select begin begin into v_test from (select 1 begin from dual);', v_split_statements(1));
	assert_equals('plsql_block: begin begin does not start a block 2c', ' select * from dual;', v_split_statements(2));

	v_statements:='declare v_test number; begin begin begin select begin begin into v_test from (select 1 begin from dual); end; end; end; select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_block: begin begin does not start a block 3a', 2, v_split_statements.count);
	assert_equals('plsql_block: begin begin does not start a block 3b', 'declare v_test number; begin begin begin select begin begin into v_test from (select 1 begin from dual); end; end; end;', v_split_statements(1));
	assert_equals('plsql_block: begin begin does not start a block 3c', ' select * from dual;', v_split_statements(2));
end test_plsql_block;


--------------------------------------------------------------------------------
procedure dynamic_tests is
	type clob_table is table of clob;
	type string_table is table of varchar2(100);
	type number_table is table of number;
	v_sql_ids string_table;
	v_sql_fulltexts clob_table;
	v_command_types number_table;
	v_command_names string_table;
	sql_cursor sys_refcursor;
begin
	--Test everything in GV$SQL.
	open sql_cursor for
	q'<
		--Only need to select one value per SQL_ID.
		select sql_id, sql_fulltext, command_type, command_name
		from
		(
			select sql_id, sql_fulltext, command_type, command_name, row_number() over (partition by sql_id order by 1) rownumber
			from gv$sql
			join gv$sqlcommand using (command_type)
			--TEST - takes 2 seconds
			where sql_id = 'dfffkcnqfystw'
		)
		where rownumber = 1
		order by sql_id
	>';

	loop
		fetch sql_cursor bulk collect into v_sql_ids, v_sql_fulltexts, v_command_types, v_command_names limit 100;
		exit when v_sql_fulltexts.count = 0;

		--Debug if there is an infinite loop.
		--dbms_output.put_line('SQL_ID: '||statements.sql_id);

		for i in 1 .. v_sql_fulltexts.count loop

			g_test_count := g_test_count + 1;

			--TODO: Test that each statement is only split into one

			/*
			statement_classifier.classify(v_sql_fulltexts(i), v_category, v_statement_type, v_command_name, v_command_type, v_lex_sqlcode, v_lex_sqlerrm);
			if v_command_type = v_command_types(i) and v_command_name = v_command_names(i) then
				g_passed_count := g_passed_count + 1;
			else
				g_failed_count := g_failed_count + 1;
				dbms_output.put_line('Failed: '||v_sql_ids(i));
				dbms_output.put_line('Expected Command Type: '||v_command_types(i));
				dbms_output.put_line('Expected Command Name: '||v_command_names(i));
				dbms_output.put_line('Actual Command Type:   '||v_command_type);
				dbms_output.put_line('Actual Command Name:   '||v_command_name);
			end if;
			*/
		end loop;
	end loop;
end dynamic_tests;



-- =============================================================================
-- Main Procedure
-- =============================================================================

--------------------------------------------------------------------------------
procedure run(p_tests number default c_static_tests) is
begin
	--Reset counters.
	g_test_count := 0;
	g_passed_count := 0;
	g_failed_count := 0;

	--Run the chosen tests.
	if bitand(p_tests, c_errors)             > 0 then test_errors;             end if;
	if bitand(p_tests, c_simple)             > 0 then test_simple;             end if;
	if bitand(p_tests, c_optional_delimiter) > 0 then test_optional_delimiter; end if;
	if bitand(p_tests, c_plsql_declaration)  > 0 then test_plsql_declaration;  end if;
	if bitand(p_tests, c_plsql_block)        > 0 then test_plsql_block;        end if;
	if bitand(p_tests, c_dynamic_tests)      > 0 then dynamic_tests;           end if;

	--Print summary of results.
	dbms_output.put_line(null);
	dbms_output.put_line('----------------------------------------');
	dbms_output.put_line('PL/SQL Statement Classifier Test Summary');
	dbms_output.put_line('----------------------------------------');
	dbms_output.put_line('Total : '||g_test_count);
	dbms_output.put_line('Passed: '||g_passed_count);
	dbms_output.put_line('Failed: '||g_failed_count);

	--Print easy to read pass or fail message.
	if g_failed_count = 0 then
		dbms_output.put_line('
  _____         _____ _____
 |  __ \ /\    / ____/ ____|
 | |__) /  \  | (___| (___
 |  ___/ /\ \  \___ \\___ \
 | |  / ____ \ ____) |___) |
 |_| /_/    \_\_____/_____/');
	else
		dbms_output.put_line('
  ______      _____ _
 |  ____/\   |_   _| |
 | |__ /  \    | | | |
 |  __/ /\ \   | | | |
 | | / ____ \ _| |_| |____
 |_|/_/    \_\_____|______|');
	end if;
end run;

end;
/