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
c_type_body          constant number := power(2, 6);
c_trigger            constant number := power(2, 7);
c_proc_and_func      constant number := power(2, 8);
c_package_body       constant number := power(2, 9);

c_static_tests  constant number := c_errors+c_simple+c_optional_delimiter
	+c_plsql_declaration+c_plsql_block+c_type_body+c_trigger+c_proc_and_func+c_package_body;

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
	--TODO:
	--Return everything for Invalid or Nothing.

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

	--Small or empty strings should not crash.
	v_statements:='';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Simple split 3a', 1, v_split_statements.count);
	assert_equals('Simple split 3b', null, v_split_statements(1));

	v_statements:='a'||chr(10);v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Simple split 4a', 1, v_split_statements.count);
	assert_equals('Simple split 4b', 'a'||chr(10), v_split_statements(1));

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
	v_statements:='select * from dual/';v_split_statements:=statement_splitter.split(v_statements, '/');
	assert_equals('Slash 1a', '1', v_split_statements.count);
	assert_equals('Slash 1b', v_statements, v_split_statements(1));

	--Valid SQL, not split.
	v_statements:='select * from dual'||chr(10)||'/';v_split_statements:=statement_splitter.split(v_statements, '/');
	assert_equals('Slash 2a', '1', v_split_statements.count);
	assert_equals('Slash 2b', 'select * from dual'||chr(10), v_split_statements(1));

	--Valid SQL, split in two.
	v_statements:='select * from dual a'||chr(10)||' 	/	 '||chr(10)||'select * from dual b';v_split_statements:=statement_splitter.split(v_statements, '/');
	assert_equals('Slash 3a', '2', v_split_statements.count);
	assert_equals('Slash 3b', 'select * from dual a'||chr(10)||' 	', v_split_statements(1));
	assert_equals('Slash 3c', '	 '||chr(10)||'select * from dual b', v_split_statements(2));

	--Valid SQL, split in three.
	v_statements:='select * from dual a'||chr(10)||' 	/	 '||chr(10)||'select * from dual b; select * from dual c';v_split_statements:=statement_splitter.split(v_statements, '/');
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
	assert_equals('Slash 6c', '	 '||chr(10)||'select * from dual b', v_split_statements(2));

	--SQL split in two with a custom delimiter, in the middle of a string.
	--This is a "bug", but it's how SQL*Plus works.
	v_statements:='select '''||chr(10)||' 	/	 '||chr(10)||''' from dual';v_split_statements:=statement_splitter.split(v_statements, '/');
	assert_equals('Slash 7a', '2', v_split_statements.count);
	assert_equals('Slash 7b', 'select '''||chr(10)||' 	', v_split_statements(1));
	assert_equals('Slash 7c', '	 '||chr(10)||''' from dual', v_split_statements(2));

	--SQL *not* split in two because the terminator has a non-whitespace item on the same line.
	--This is a weird, but it's how SQL*Plus works.
	v_statements:='select * from dual a'||chr(10)||' / --bad comment'||chr(10)||'select * from dual b';v_split_statements:=statement_splitter.split(v_statements, '/');
	assert_equals('Slash 8a', '1', v_split_statements.count);
	assert_equals('Slash 8b', v_statements, v_split_statements(1));


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

	--"select 1 as begin" should not count as a "BEGIN".
	v_statements:='with function f return number is v_test number; begin select 1 as begin into v_test from dual; return 1; end; select f from dual;select 1 from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 8a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 8b', 'with function f return number is v_test number; begin select 1 as begin into v_test from dual; return 1; end; select f from dual;', v_split_statements(1));
	assert_equals('plsql_declaration 8c', 'select 1 from dual;', v_split_statements(2));

	--TODO: Test commas, FROM, into, bulk collect.

	--CLUSTER_ID "as begin" exception
	v_statements:='with function f return number is v_number number; begin select cluster_id(some_model using asdf as begin) into v_number from dual; return v_number; end; select f from dual;select * from dual b;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 9a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 9b', 'with function f return number is v_number number; begin select cluster_id(some_model using asdf as begin) into v_number from dual; return v_number; end; select f from dual;', v_split_statements(1));
	assert_equals('plsql_declaration 9c', 'select * from dual b;', v_split_statements(2));

	--PIVOT_IN_CLAUSE "as begin" exception.
	v_statements:=q'!
with function f return number is
	v_number number;
begin
	select 1
	into v_number
	from (select 1 deptno, 'A' job, 100 sal from dual)
	pivot
	(
		sum(sal)
		for deptno
		in  (1,2 as begin)
	);
	return v_number;
end;select f from dual;select * from dual b!';
	v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 10a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 10b', 'select * from dual b', v_split_statements(2));

	--XMLATTRIBUTES "as begin" exception.
	v_statements:=q'!
with function f return xmltype is
	v_test xmltype;
begin
	select xmlelement("a", xmlattributes(1 as begin)) into v_test from dual;
	return v_test;
end; select f from dual;select * from dual b!';
	v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 11a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 11b', 'select * from dual b', v_split_statements(2));

	--XMLCOLATTVAL "as begin" exception.
	v_statements:=q'!
with function f return xmltype is
	v_test xmltype;
begin
	select xmlelement("a", xmlcolattval(1 as begin))
	into v_test
	from dual;
	return v_test;
end; select f from dual;select * from dual b!';
	v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 12a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 12b', 'select * from dual b', v_split_statements(2));

	--XMLELEMENTS "as begin" exception.
	v_statements:=q'!
with function f return xmltype is
	v_test xmltype;
begin
	select xmlelement("a", sys.odcivarchar2list('b') as begin) into v_test from dual;
	return v_test;
end; select f from dual;select * from dual b!';
	v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 13a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 13b', 'select * from dual b', v_split_statements(2));

	--XMLFOREST "as begin" exception.
	v_statements:=q'!
with function f return xmltype is
	v_test xmltype;
begin
	select xmlforest(1 as begin) into v_test from dual;
	return v_test;
end; select f from dual;select * from dual b!';
	v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 14a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 14b', 'select * from dual b', v_split_statements(2));

	--XMLTABLE_options "as begin" exception.
	v_statements:=q'!
with function f return varchar2 is
	v_test varchar2(1);
begin
	select name
	into v_test
	from (select xmltype('<emp><name>A</name></emp>') the_xml from dual) emp
	cross join xmltable('/emp' passing emp.the_xml, emp.the_xml as begin columns name varchar2(100) path '/emp/name');
	return v_test;
end; select f from dual;select * from dual b!';
	v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 15a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 15b', 'select * from dual b', v_split_statements(2));

	--XMLnamespaces_clause "as begin" exception.
	v_statements:=q'!
with function f return varchar2 is
	v_test varchar2(1);
begin
	select name
	into v_test
	from (select xmltype('<emp><name>A</name></emp>') the_xml from dual) emp
	cross join xmltable(xmlnamespaces('N' as begin, default ''), '/emp' passing the_xml columns name varchar2(1) path '/emp/name');
	return v_test;
end; select f from dual;select * from dual b!';
	v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 16a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 16b', 'select * from dual b', v_split_statements(2));

	--PIVOT "as begin" exception.
	v_statements:=q'!
with function f return number is
	v_number number;
begin
	select 1
	into v_number
	from (select 1 deptno, 'A' job, 100 sal from dual)
	pivot
	(
		sum(sal) as begin
		for deptno
		in  (1,2)
	);
	return v_number;
end; select f from dual;select * from dual b!';
	v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 17a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 17b', 'select * from dual b', v_split_statements(2));

	--PIVOT XML "as begin" exception.
	v_statements:=q'!
with function f return number is
	v_number number;
begin
	select 1
	into v_number
	from (select 1 deptno, 'A' job, 100 sal from dual)
	pivot xml
	(
		sum(sal) as begin1, sum(sal) as begin
		for deptno
		in  (any)
	);
	return v_number;
end; select f from dual;select * from dual b!';
	v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 18a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 18b', 'select * from dual b', v_split_statements(2));

	--nested_table_col_properties "as begin" exception.
	v_statements:=q'!
create table test1 nested table a store as begin as
with function f return varchar2 is v_string varchar2(1); begin return 'A'; end;
select sys.dbms_debug_vc2coll('A') a from dual;select * from dual b!';
	v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_declaration 19a', 2, v_split_statements.count);
	assert_equals('plsql_declaration 19b', 'select * from dual b', v_split_statements(2));

	--TODO: SQL with PL/SQL with a SQL with PL/SQL.
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

	v_statements:='declare v_test number; begin select 1 as end into v_test from dual; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_block: "as end" does not count 1a', 2, v_split_statements.count);
	assert_equals('plsql_block: "as end" does not count 1b', 'declare v_test number; begin select 1 as end into v_test from dual; end;', v_split_statements(1));
	assert_equals('plsql_block: "as end" does not count 1c', 'select * from dual;', v_split_statements(2));

	v_statements:='declare v_test number; begin with end as (select 1 a from dual) select a into v_test from end; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('plsql_block: "as end" does not count 2a', 2, v_split_statements.count);
	assert_equals('plsql_block: "as end" does not count 2b', 'declare v_test number; begin with end as (select 1 a from dual) select a into v_test from end; end;', v_split_statements(1));
	assert_equals('plsql_block: "as end" does not count 2c', 'select * from dual;', v_split_statements(2));
end test_plsql_block;


--------------------------------------------------------------------------------
procedure test_type_body is
	v_statements nclob;
	v_split_statements nclob_table := nclob_table();
begin
	--TODO:
	--All type body member types.
	/* This is the type spec to make the next type body work.
	create or replace type type1 is object
	(
		a number,
		member procedure procedure1,
		member function function1 return number,
		order member function return_order(a type1) return number,
		final instantiable constructor function type1 return self as result
	);
	*/
/*
	v_statements:='
		create or replace type body type1 is
			member procedure procedure1 is begin null; end;
			member function function1 return number is begin return 1; end;
			order member function return_order(a type1) return number is begin return 1; end;
			final instantiable constructor function type1 return self as result is begin null; end;
		end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Type Body 1a', 2, v_split_statements.count);
	assert_equals('Type body 1b', ' select * from dual;', v_split_statements(2));
*/
	null;
end test_type_body;


--------------------------------------------------------------------------------
procedure test_trigger is
	v_statements nclob;
	v_split_statements nclob_table := nclob_table();
begin
	--Regular triggers have a matched begin/end.
	v_statements:='
		create or replace trigger test2_trigger1
		instead of insert on test2_vw
		begin null; end test2_trigger1;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 1a', 2, v_split_statements.count);
	assert_equals('Trigger 1b', 'select * from dual;', v_split_statements(2));

	--Compound triggers require an extra END.
	v_statements:='
		create or replace trigger test1_trigger2
		for update of a on test1
		compound trigger
			test_variable number;
			procedure nested_procedure is begin null; end nested_procedure;
			before statement is begin null; end before statement;
			before each row is begin null; end before each row;
			after statement is begin null; end after statement;
			after each row is begin null; end after each row;
			--This is invalid even though the manual implies it is allowed.
			--instead of each row is begin null; end instead of each row;
		end test1_trigger2;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 2a', 2, v_split_statements.count);
	assert_equals('Trigger 2b', 'select * from dual;', v_split_statements(2));

	--A CALL trigger needs a regular terminator.
	--(This behavior is slightly different than SQL*Plus and the manual.
	--Officially, the CALL version of a trigger cannot end with a semicolon.)
	v_statements:='
		create or replace trigger test1_trigger1
		before delete on test1
		for each row
		call test_procedure;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 3a', 2, v_split_statements.count);
	assert_equals('Trigger 3b', 'select * from dual;', v_split_statements(2));


	---------------------------------------
	--Regular triggers with "CALL" in different position.
	---------------------------------------

	--Name of trigger.
	v_statements:='create trigger call before update on table1 for each row begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 4a', 2, v_split_statements.count);
	assert_equals('Trigger 4b', 'select * from dual;', v_split_statements(2));

	--Name of schema and trigger.
	v_statements:='create trigger call.call before update on table1 for each row begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 5a', 2, v_split_statements.count);
	assert_equals('Trigger 5b', 'select * from dual;', v_split_statements(2));

	--Name of schema and trigger.
	v_statements:='create trigger call.call before update on table1 for each row begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 6a', 2, v_split_statements.count);
	assert_equals('Trigger 6b', 'select * from dual;', v_split_statements(2));

	--dml_event_clause - first column, schema name and table name
	v_statements:='create trigger call.call before update of call on call.call for each row begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 7a', 2, v_split_statements.count);
	assert_equals('Trigger 7b', 'select * from dual;', v_split_statements(2));

	--dml_event_clause - additional column, schema name, and tble name.
	v_statements:='create trigger call.call before update of a, call on call.call for each row begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 8a', 2, v_split_statements.count);
	assert_equals('Trigger 8b', 'select * from dual;', v_split_statements(2));

	--referencing_clause 1 - old
	v_statements:='create or replace trigger trigger1 after update on table1 referencing old as call begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 9a', 2, v_split_statements.count);
	assert_equals('Trigger 9b', 'select * from dual;', v_split_statements(2));

	--referencing_clause 2 - new
	v_statements:='create or replace trigger trigger1 after update on table1 referencing new as call begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 10a', 2, v_split_statements.count);
	assert_equals('Trigger 10b', 'select * from dual;', v_split_statements(2));

	--referencing_clause 3 - parent
	v_statements:='create or replace trigger trigger1 instead of update on nested table v_type1_nt of view1 referencing parent as call old as asdf new as qwer begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 11a', 2, v_split_statements.count);
	assert_equals('Trigger 11b', 'select * from dual;', v_split_statements(2));

	--referencing_clause 4 - combined 1
	v_statements:='create or replace trigger trigger1 instead of update on nested table v_type1_nt of view1 referencing parent as call old as asdf new as qwer begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 12a', 2, v_split_statements.count);
	assert_equals('Trigger 12b', 'select * from dual;', v_split_statements(2));

	--referencing_clause 5 - combined 2
	v_statements:='create or replace trigger trigger1 instead of update on nested table v_type1_nt of view1 referencing old as asdf parent as call new as qwer begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 13a', 2, v_split_statements.count);
	assert_equals('Trigger 13b', 'select * from dual;', v_split_statements(2));

	--referencing_clause 6 - "as begin" does not count as a real BEGIN.
	v_statements:='create or replace trigger trigger1 after update on table1 referencing old as begin begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 13.5a', 2, v_split_statements.count);
	assert_equals('Trigger 13.5b', 'select * from dual;', v_split_statements(2));

	--referencing_clause 7 - "as end" does not count as a real END.
	v_statements:='create or replace trigger trigger1 after update on table1 referencing old as end begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 13.7a', 2, v_split_statements.count);
	assert_equals('Trigger 13.7b', 'select * from dual;', v_split_statements(2));

	--trigger_ordering_clause 1 - follows 1
	v_statements:='create or replace trigger trigger2 before update on table1 for each row follows call begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 14a', 2, v_split_statements.count);
	assert_equals('Trigger 14b', 'select * from dual;', v_split_statements(2));

	--trigger_ordering_clause 1 - follows 2
	v_statements:='create or replace trigger trigger2 before update on table1 for each row follows call.call begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 15a', 2, v_split_statements.count);
	assert_equals('Trigger 15b', 'select * from dual;', v_split_statements(2));

	--trigger_ordering_clause 1 - follows 3
	v_statements:='create or replace trigger trigger2 before update on table1 for each row follows call.call, call begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 16a', 2, v_split_statements.count);
	assert_equals('Trigger 16b', 'select * from dual;', v_split_statements(2));

	--trigger_ordering_clause 1 - follows 4.  Yes, this is valid syntax!  (Except that the semicolon after the first statement would not work in SQL*Plus.)
	v_statements:='create or replace trigger trigger2 before update on table1 for each row follows call.call, call, call, call call test_procedure;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 17a', 2, v_split_statements.count);
	assert_equals('Trigger 17b', 'select * from dual;', v_split_statements(2));

	--trigger_ordering_clause 2 - precedes
	v_statements:='create or replace trigger trigger2 before update on table1 for each row precedes call begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 18a', 2, v_split_statements.count);
	assert_equals('Trigger 18b', 'select * from dual;', v_split_statements(2));

	--WHEN (condition)
	v_statements:='create or replace trigger trigger2 before update on table1 for each row when (((old.call > new.call)) or (old.call > 1)) begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 19a', 2, v_split_statements.count);
	assert_equals('Trigger 19b', 'select * from dual;', v_split_statements(2));

	--system_trigger [on schema.schema]
	v_statements:='create or replace trigger trigger_schema before comment or create on call.schema begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 20a', 2, v_split_statements.count);
	assert_equals('Trigger 20b', 'select * from dual;', v_split_statements(2));
end test_trigger;


--------------------------------------------------------------------------------
procedure test_proc_and_func is
	v_statements nclob;
	v_split_statements nclob_table := nclob_table();
begin
	--Regular procedure.
	v_statements:='create procedure test_procedure is begin null; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 1a', 2, v_split_statements.count);
	assert_equals('Trigger 1b', 'create procedure test_procedure is begin null; end;', v_split_statements(1));
	assert_equals('Trigger 1c', 'select * from dual;', v_split_statements(2));

	--External procedure.
	v_statements:='create procedure test_procedure as external language c name "c_test" library test_lib;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 1a', 2, v_split_statements.count);
	assert_equals('Trigger 1b', 'create procedure test_procedure as external language c name "c_test" library test_lib;', v_split_statements(1));
	assert_equals('Trigger 1c', 'select * from dual;', v_split_statements(2));

	--Regular function.
	v_statements:='create function test_function return number is begin return 1; end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 1a', 2, v_split_statements.count);
	assert_equals('Trigger 1b', 'create function test_function return number is begin return 1; end;', v_split_statements(1));
	assert_equals('Trigger 1c', 'select * from dual;', v_split_statements(2));

	--External function.
	v_statements:='create function test_function return number as external language c name "c_test" library test_lib;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Trigger 1a', 2, v_split_statements.count);
	assert_equals('Trigger 1b', 'create function test_function return number as external language c name "c_test" library test_lib;', v_split_statements(1));
	assert_equals('Trigger 1c', 'select * from dual;', v_split_statements(2));
end test_proc_and_func;


--------------------------------------------------------------------------------
procedure test_package_body is
	v_statements nclob;
	v_split_statements nclob_table := nclob_table();
begin
	--#1: Extra END in an emtpy package body.
	v_statements:='
		create or replace package body test_package is
		end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Package Body 1a', 2, v_split_statements.count);
	assert_equals('Packabe Body 1b', 'select * from dual;', v_split_statements(2));

	v_statements:='
		create or replace package body test_package is
		end test_package;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Package Body 1.5a', 2, v_split_statements.count);
	assert_equals('Packabe Body 1.5b', 'select * from dual;', v_split_statements(2));

	--#2: One matched BEGIN and END when there is only an initialization block.
	v_statements:='
		create or replace package body test_package is
		begin
			null;
		end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Package Body 2a', 2, v_split_statements.count);
--	assert_equals('Packabe Body 2b', 'select * from dual;', v_split_statements(2));

	--#3: Matched BEGIN and END and extra END.
	v_statements:='
		create or replace package body test_package is
			procedure test1 is begin null; end;
		end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Package Body 3a', 2, v_split_statements.count);
--	assert_equals('Packabe Body 3b', 'select * from dual;', v_split_statements(2));

	--#4: Two sets of matched BEGINs and ENDs - from methods.
	v_statements:='
		create or replace package body test_package is
			procedure test1 is begin null; end;
		begin
			null;
		end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Package Body 4a', 2, v_split_statements.count);
--	assert_equals('Packabe Body 4b', 'select * from dual;', v_split_statements(2));

	--#5: Two sets of matched BEGINs and ENDs - from CURSORS and methods.
	v_statements:='
		create or replace package body test_package is
			cursor my_cursor is with function test_function return number is begin return 1; end; select test_function from dual;
			procedure test1 is begin null; end;
		begin
			null;
		end;select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Package Body 5a', 2, v_split_statements.count);
--	assert_equals('Packabe Body 5b', 'select * from dual;', v_split_statements(2));

	/*
	--
	v_statements:='
		select * from dual;';v_split_statements:=statement_splitter.split(v_statements);
	assert_equals('Package Body a', 2, v_split_statements.count);
	assert_equals('Packabe Body b', 'select * from dual;', v_split_statements(2));
	*/
end test_package_body;


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
	if bitand(p_tests, c_type_body)          > 0 then test_type_body;          end if;
	if bitand(p_tests, c_trigger)            > 0 then test_trigger;            end if;
	if bitand(p_tests, c_proc_and_func)      > 0 then test_proc_and_func;      end if;
	if bitand(p_tests, c_package_body)       > 0 then test_package_body;       end if;

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
