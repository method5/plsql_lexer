create or replace package statement_classifier_test authid current_user is

/*
## Purpose ##

Unit tests for statement_classifier.


## Example ##

begin
	statement_classifier_test.run;
	statement_classifier_test.run(statement_classifier_test.c_dynamic_tests);
end;

*/

--Globals to select which test suites to run.
c_test_whitespace  constant number := power(2, 1);
c_test_other       constant number := power(2, 9);

c_dynamic_tests    constant number := power(2, 30);

--Default option is to run all static test suites.
c_all_static_tests constant number := c_test_whitespace+c_test_other;

--Run the unit tests and display the results in dbms output.
procedure run(p_tests number default c_all_static_tests);

end;
/
create or replace package body statement_classifier_test is

--Global counters.
g_test_count number := 0;
g_passed_count number := 0;
g_failed_count number := 0;


--Helper procedures.


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


--------------------------------------------------------------------------------
--Test Suites
procedure test_whitespace is
begin
	null;
end test_whitespace;


--------------------------------------------------------------------------------
procedure test_other is
begin
	null;
end test_other;


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

	v_category varchar2(30);
	v_statement_type varchar2(30);
	v_command_name varchar2(4000);
	v_command_type varchar2(4000);
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

			statement_classifier.classify(v_sql_fulltexts(i), v_category, v_statement_type, v_command_name, v_command_type);
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
		end loop;
	end loop;
end dynamic_tests;


--------------------------------------------------------------------------------
procedure run(p_tests number default c_all_static_tests) is
begin
	--Reset counters.
	g_test_count := 0;
	g_passed_count := 0;
	g_failed_count := 0;

	--Run the chosen tests.
	if bitand(p_tests, c_test_whitespace)  > 0 then test_whitespace; end if;
	if bitand(p_tests, c_test_other)       > 0 then test_other; end if;
	if bitand(p_tests, c_dynamic_tests)    > 0 then dynamic_tests; end if;


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
