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
c_errors        constant number := power(2, 1);
c_commands      constant number := power(2, 2);

c_static_tests  constant number := c_errors+c_commands;

c_dynamic_tests constant number := power(2, 30);

c_all_tests constant number := c_static_tests+c_dynamic_tests;

--Run the unit tests and display the results in dbms output.
procedure run(p_tests number default c_static_tests);

end;
/
create or replace package body statement_classifier_test is

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


--------------------------------------------------------------------------------
procedure classify(p_statement nclob, p_output out output_rec) is
	v_category varchar2(100);
	v_statement_type varchar2(100);
	v_command_name varchar2(64);
	v_command_type number;
	v_lex_sqlcode number;
	v_lex_sqlerrm varchar2(4000);
begin
	statement_classifier.classify(p_statement, 
		v_category,v_statement_type,v_command_name,v_command_type,v_lex_sqlcode,v_lex_sqlerrm);

	p_output.category := v_category;
	p_output.statement_type := v_statement_type;
	p_output.command_name := v_command_name;
	p_output.command_type := v_command_type;
	p_output.lex_sqlcode := v_lex_sqlcode;
	p_output.lex_sqlerrm := v_lex_sqlerrm;
	p_output.fatal_error := null;
exception when others then
	p_output.fatal_error := dbms_utility.format_error_stack||dbms_utility.format_error_backtrace;
end classify;


-- =============================================================================
-- Test Suites
-- =============================================================================

--------------------------------------------------------------------------------
procedure test_errors is
	v_output output_rec;
begin
	classify('(select * from dual)', v_output);
	assert_equals('No errors 1', null, v_output.lex_sqlcode);
	assert_equals('No errors 1', null, v_output.lex_sqlerrm);

	classify('(select * from dual) /*', v_output);
	assert_equals('Comment error 1', -1742, v_output.lex_sqlcode);
	assert_equals('Comment error 2', 'comment not terminated properly', v_output.lex_sqlerrm);

	classify('(select * from dual) "', v_output);
	assert_equals('Missing double quote error 1', -1740, v_output.lex_sqlcode);
	assert_equals('Missing double quote error 2', 'missing double quote in identifier', v_output.lex_sqlerrm);

	classify('(select 1 "" from dual)', v_output);
	assert_equals('Zero-length identifier 1', -1741, v_output.lex_sqlcode);
	assert_equals('Zero-length identifier 2', 'illegal zero-length identifier', v_output.lex_sqlerrm);

	classify('(select 1 a123456789012345678901234567890 from dual)', v_output);
	assert_equals('Identifier too long error 1', -972, v_output.lex_sqlcode);
	assert_equals('Identifier too long error 2', 'identifier is too long', v_output.lex_sqlerrm);

	classify('(select 1 "a123456789012345678901234567890" from dual)', v_output);
	assert_equals('Identifier too long error 3', -972, v_output.lex_sqlcode);
	assert_equals('Identifier too long error 4', 'identifier is too long', v_output.lex_sqlerrm);

	classify(q'<declare v_test varchar2(100) := q'  '; begin null; end;>', v_output);
	assert_equals('Invalid character 1', -911, v_output.lex_sqlcode);
	assert_equals('Invalid character 2', 'invalid character', v_output.lex_sqlerrm);
	classify(q'<declare v_test varchar2(100) := nq'  '; begin null; end;>', v_output);
	assert_equals('Invalid character 3', -911, v_output.lex_sqlcode);
	assert_equals('Invalid character 4', 'invalid character', v_output.lex_sqlerrm);

	classify('(select * from dual) '' ', v_output);
	assert_equals('String not terminated 1', -1756, v_output.lex_sqlcode);
	assert_equals('String not terminated 2', 'quoted string not properly terminated', v_output.lex_sqlerrm);
	classify(q'<(select * from dual) q'!' >', v_output);
	assert_equals('String not terminated 3', -1756, v_output.lex_sqlcode);
	assert_equals('String not terminated 4', 'quoted string not properly terminated', v_output.lex_sqlerrm);

	--TODO: Test cannot parse

end test_errors;


--------------------------------------------------------------------------------
procedure test_commands is
	v_output output_rec;

	--Helper function that concatenates results for easy string comparison.
	function concat(p_output output_rec) return varchar2 is
	begin
		return nvl(p_output.fatal_error,
			p_output.category||'|'||p_output.statement_type||'|'||p_output.command_name||'|'||p_output.command_type);
	end;
begin
	/*
	DDL
		ADMINISTER KEY MANAGEMENT, ALTER (except ALTER SESSION and ALTER SYSTEM),
		ANALYZE,ASSOCIATE STATISTICS,AUDIT,COMMENT,CREATE,DISASSOCIATE STATISTICS,
		DROP,FLASHBACK,GRANT,NOAUDIT,PURGE,RENAME,REVOKE,TRUNCATE
	DML
		CALL,DELETE,EXPLAIN PLAN,INSERT,LOCK TABLE,MERGE,SELECT,UPDATE
	Transaction Control
		COMMIT,ROLLBACK,SAVEPOINT,SET TRANSACTION,SET CONSTRAINT
	Session Control
		ALTER SESSION,SET ROLE
	System Control
		ALTER SYSTEM
	PL/SQL
		Block
	*/

	--These tests are based on `select * from v$sqlcommand order by command_name;`,
	--and comparing syntx with the manual.
	classify(q'[/*comment*/ adMINister /*asdf*/ kEy manaGEment create keystore 'asdf' identified by qwer]', v_output); assert_equals('ADMINISTER KEY MANAGEMENT', 'DDL|ADMINISTER KEY MANAGEMENT|ADMINISTER KEY MANAGEMENT|238', concat(v_output));
	classify(q'[ alter assemBLY /*I don't think this is a real command but whatever*/]', v_output); assert_equals('ALTER ASSEMBLY', 'DDL|ALTER|ALTER ASSEMBLY|217', concat(v_output));
	classify(q'[ ALTEr AUDIt POLICY myPOLICY drop roles myRole; --comment]', v_output); assert_equals('ALTER AUDIT POLICY', 'DDL|ALTER|ALTER AUDIT POLICY|230', concat(v_output));
	classify(q'[	alter	cluster	schema.my_cluster parallel 8]', v_output); assert_equals('ALTER CLUSTER', 'DDL|ALTER|ALTER CLUSTER|5', concat(v_output));
	classify(q'[alter database cdb1 mount]', v_output); assert_equals('ALTER DATABASE', 'DDL|ALTER|ALTER DATABASE|35', concat(v_output));
	classify(q'[alter shared public database link my_link connect to me identified by "password";]', v_output); assert_equals('ALTER DATABASE LINK', 'DDL|ALTER|ALTER DATABASE LINK|225', concat(v_output));
	classify(q'[ alter dimENSION my_dimension#12 compile;]', v_output); assert_equals('ALTER DIMENSION', 'DDL|ALTER|ALTER DIMENSION|175', concat(v_output));
	classify(q'[/*+useless comment*/ alter diskgroup +orcl13 resize disk '/emcpowersomething/' size 500m;]', v_output); assert_equals('ALTER DISKGROUP', 'DDL|ALTER|ALTER DISK GROUP|193', concat(v_output));
	--Undocumented feature:
	classify(q'[ alter EDITION my_edition unusable]', v_output); assert_equals('ALTER EDITION', 'DDL|ALTER|ALTER EDITION|213', concat(v_output));
	classify(q'[ alter  flashback  archive myarchive set default;]', v_output); assert_equals('ALTER FLASHBACK ARCHIVE', 'DDL|ALTER|ALTER FLASHBACK ARCHIVE|219', concat(v_output));
	classify(q'[ALTER FUNCTION myschema.myfunction compile;]', v_output); assert_equals('ALTER FUNCTION', 'DDL|ALTER|ALTER FUNCTION|92', concat(v_output));
	classify(q'[ alter index asdf rebuild parallel 8]', v_output); assert_equals('ALTER INDEX', 'DDL|ALTER|ALTER INDEX|11', concat(v_output));
	classify(q'[ALTER INDEXTYPE  my_schema.my_indextype compile;]', v_output); assert_equals('ALTER INDEXTYPE', 'DDL|ALTER|ALTER INDEXTYPE|166', concat(v_output));
	classify(q'[ALTER JAVA]', v_output); assert_equals('ALTER JAVA', 'DDL|ALTER|ALTER JAVA|161', concat(v_output));

	--TODO:
	classify(q'[ALTER LIBRARY]', v_output); assert_equals('ALTER LIBRARY', 'DDL|ALTER|ALTER LIBRARY|196', concat(v_output));
	classify(q'[ALTER MATERIALIZED VIEW ]', v_output); assert_equals('ALTER MATERIALIZED VIEW ', 'DDL|ALTER|ALTER MATERIALIZED VIEW |75', concat(v_output));
	classify(q'[ALTER MATERIALIZED VIEW LOG]', v_output); assert_equals('ALTER MATERIALIZED VIEW LOG', 'DDL|ALTER|ALTER MATERIALIZED VIEW LOG|72', concat(v_output));
	classify(q'[ALTER MATERIALIZED ZONEMAP]', v_output); assert_equals('ALTER MATERIALIZED ZONEMAP', 'DDL|ALTER|ALTER MATERIALIZED ZONEMAP|240', concat(v_output));
	classify(q'[ALTER OPERATOR]', v_output); assert_equals('ALTER OPERATOR', 'DDL|ALTER|ALTER OPERATOR|183', concat(v_output));
	classify(q'[ALTER OUTLINE]', v_output); assert_equals('ALTER OUTLINE', 'DDL|ALTER|ALTER OUTLINE|179', concat(v_output));
	classify(q'[ALTER PACKAGE]', v_output); assert_equals('ALTER PACKAGE', 'DDL|ALTER|ALTER PACKAGE|95', concat(v_output));
	classify(q'[ALTER PACKAGE BODY]', v_output); assert_equals('ALTER PACKAGE BODY', 'DDL|ALTER|ALTER PACKAGE BODY|98', concat(v_output));
	classify(q'[ALTER PLUGGABLE DATABASE]', v_output); assert_equals('ALTER PLUGGABLE DATABASE', 'DDL|ALTER|ALTER PLUGGABLE DATABASE|227', concat(v_output));
	classify(q'[ALTER PROCEDURE]', v_output); assert_equals('ALTER PROCEDURE', 'DDL|ALTER|ALTER PROCEDURE|25', concat(v_output));
	classify(q'[ALTER PROFILE]', v_output); assert_equals('ALTER PROFILE', 'DDL|ALTER|ALTER PROFILE|67', concat(v_output));
	classify(q'[ALTER RESOURCE COST]', v_output); assert_equals('ALTER RESOURCE COST', 'DDL|ALTER|ALTER RESOURCE COST|70', concat(v_output));
	classify(q'[ALTER REWRITE EQUIVALENCE]', v_output); assert_equals('ALTER REWRITE EQUIVALENCE', 'DDL|ALTER|ALTER REWRITE EQUIVALENCE|210', concat(v_output));
	classify(q'[ALTER ROLE]', v_output); assert_equals('ALTER ROLE', 'DDL|ALTER|ALTER ROLE|79', concat(v_output));
	classify(q'[ALTER ROLLBACK SEGMENT]', v_output); assert_equals('ALTER ROLLBACK SEGMENT', 'DDL|ALTER|ALTER ROLLBACK SEGMENT|37', concat(v_output));
	classify(q'[ALTER SEQUENCE]', v_output); assert_equals('ALTER SEQUENCE', 'DDL|ALTER|ALTER SEQUENCE|14', concat(v_output));
	classify(q'[ALTER SESSION]', v_output); assert_equals('ALTER SESSION', 'Session Control|ALTER SESSION|ALTER SESSION|42', concat(v_output));
	classify(q'[ALTER SUMMARY]', v_output); assert_equals('ALTER SUMMARY', 'DDL|ALTER|ALTER SUMMARY|172', concat(v_output));
	classify(q'[ALTER SYNONYM]', v_output); assert_equals('ALTER SYNONYM', 'DDL|ALTER|ALTER SYNONYM|192', concat(v_output));
	classify(q'[ALTER SYSTEM]', v_output); assert_equals('ALTER SYSTEM', 'System Control|ALTER SYSTEM|ALTER SYSTEM|49', concat(v_output));
	classify(q'[ALTER TABLE]', v_output); assert_equals('ALTER TABLE', 'DDL|ALTER|ALTER TABLE|15', concat(v_output));
	classify(q'[ALTER TABLESPACE]', v_output); assert_equals('ALTER TABLESPACE', 'DDL|ALTER|ALTER TABLESPACE|40', concat(v_output));
	classify(q'[ALTER TRACING]', v_output); assert_equals('ALTER TRACING', 'DDL|ALTER|ALTER TRACING|58', concat(v_output));
	classify(q'[ALTER TRIGGER]', v_output); assert_equals('ALTER TRIGGER', 'DDL|ALTER|ALTER TRIGGER|60', concat(v_output));
	classify(q'[ALTER TYPE]', v_output); assert_equals('ALTER TYPE', 'DDL|ALTER|ALTER TYPE|80', concat(v_output));
	classify(q'[ALTER TYPE BODY]', v_output); assert_equals('ALTER TYPE BODY', 'DDL|ALTER|ALTER TYPE BODY|82', concat(v_output));
	classify(q'[ALTER USER]', v_output); assert_equals('ALTER USER', 'DDL|ALTER|ALTER USER|43', concat(v_output));
	classify(q'[ALTER VIEW]', v_output); assert_equals('ALTER VIEW', 'DDL|ALTER|ALTER VIEW|88', concat(v_output));
	classify(q'[ANALYZE CLUSTER]', v_output); assert_equals('ANALYZE CLUSTER', 'DDL|ALTER|ANALYZE CLUSTER|64', concat(v_output));
	classify(q'[ANALYZE INDEX]', v_output); assert_equals('ANALYZE INDEX', 'DDL|ALTER|ANALYZE INDEX|63', concat(v_output));
	classify(q'[ANALYZE TABLE]', v_output); assert_equals('ANALYZE TABLE', 'DDL|ALTER|ANALYZE TABLE|62', concat(v_output));
	classify(q'[ASSOCIATE STATISTICS]', v_output); assert_equals('ASSOCIATE STATISTICS', 'DDL|ALTER|ASSOCIATE STATISTICS|168', concat(v_output));
	classify(q'[AUDIT OBJECT]', v_output); assert_equals('AUDIT OBJECT', 'DDL|ALTER|AUDIT OBJECT|30', concat(v_output));
	classify(q'[CALL METHOD]', v_output); assert_equals('CALL METHOD', 'DDL|ALTER|CALL METHOD|170', concat(v_output));
	classify(q'[CHANGE PASSWORD]', v_output); assert_equals('CHANGE PASSWORD', 'DDL|ALTER|CHANGE PASSWORD|190', concat(v_output));
	classify(q'[COMMENT]', v_output); assert_equals('COMMENT', 'DDL|ALTER|COMMENT|29', concat(v_output));
	classify(q'[COMMIT]', v_output); assert_equals('COMMIT', 'Transaction Control|COMMIT|COMMIT|44', concat(v_output));
	classify(q'[CREATE ASSEMBLY]', v_output); assert_equals('CREATE ASSEMBLY', 'DDL|ALTER|CREATE ASSEMBLY|216', concat(v_output));
	classify(q'[CREATE AUDIT POLICY]', v_output); assert_equals('CREATE AUDIT POLICY', 'DDL|ALTER|CREATE AUDIT POLICY|229', concat(v_output));
	classify(q'[CREATE BITMAPFILE]', v_output); assert_equals('CREATE BITMAPFILE', 'DDL|ALTER|CREATE BITMAPFILE|87', concat(v_output));
	classify(q'[CREATE CLUSTER]', v_output); assert_equals('CREATE CLUSTER', 'DDL|ALTER|CREATE CLUSTER|4', concat(v_output));
	classify(q'[CREATE CONTEXT]', v_output); assert_equals('CREATE CONTEXT', 'DDL|ALTER|CREATE CONTEXT|177', concat(v_output));
	classify(q'[CREATE CONTROL FILE]', v_output); assert_equals('CREATE CONTROL FILE', 'DDL|ALTER|CREATE CONTROL FILE|57', concat(v_output));
	classify(q'[CREATE DATABASE]', v_output); assert_equals('CREATE DATABASE', 'DDL|ALTER|CREATE DATABASE|34', concat(v_output));
	classify(q'[CREATE DATABASE LINK]', v_output); assert_equals('CREATE DATABASE LINK', 'DDL|ALTER|CREATE DATABASE LINK|32', concat(v_output));
	classify(q'[CREATE DIMENSION]', v_output); assert_equals('CREATE DIMENSION', 'DDL|ALTER|CREATE DIMENSION|174', concat(v_output));
	classify(q'[CREATE DIRECTORY]', v_output); assert_equals('CREATE DIRECTORY', 'DDL|ALTER|CREATE DIRECTORY|157', concat(v_output));
	classify(q'[CREATE DISK GROUP]', v_output); assert_equals('CREATE DISK GROUP', 'DDL|ALTER|CREATE DISK GROUP|194', concat(v_output));
	classify(q'[CREATE EDITION]', v_output); assert_equals('CREATE EDITION', 'DDL|ALTER|CREATE EDITION|212', concat(v_output));
	classify(q'[CREATE FLASHBACK ARCHIVE]', v_output); assert_equals('CREATE FLASHBACK ARCHIVE', 'DDL|ALTER|CREATE FLASHBACK ARCHIVE|218', concat(v_output));
	classify(q'[CREATE FUNCTION]', v_output); assert_equals('CREATE FUNCTION', 'DDL|ALTER|CREATE FUNCTION|91', concat(v_output));
	classify(q'[CREATE INDEX]', v_output); assert_equals('CREATE INDEX', 'DDL|ALTER|CREATE INDEX|9', concat(v_output));
	classify(q'[CREATE INDEXTYPE]', v_output); assert_equals('CREATE INDEXTYPE', 'DDL|ALTER|CREATE INDEXTYPE|164', concat(v_output));
	classify(q'[CREATE JAVA]', v_output); assert_equals('CREATE JAVA', 'DDL|ALTER|CREATE JAVA|160', concat(v_output));
	classify(q'[CREATE LIBRARY]', v_output); assert_equals('CREATE LIBRARY', 'DDL|ALTER|CREATE LIBRARY|159', concat(v_output));
	classify(q'[CREATE MATERIALIZED VIEW ]', v_output); assert_equals('CREATE MATERIALIZED VIEW ', 'DDL|ALTER|CREATE MATERIALIZED VIEW |74', concat(v_output));
	classify(q'[CREATE MATERIALIZED VIEW LOG]', v_output); assert_equals('CREATE MATERIALIZED VIEW LOG', 'DDL|ALTER|CREATE MATERIALIZED VIEW LOG|71', concat(v_output));
	classify(q'[CREATE MATERIALIZED ZONEMAP]', v_output); assert_equals('CREATE MATERIALIZED ZONEMAP', 'DDL|ALTER|CREATE MATERIALIZED ZONEMAP|239', concat(v_output));
	classify(q'[CREATE OPERATOR]', v_output); assert_equals('CREATE OPERATOR', 'DDL|ALTER|CREATE OPERATOR|163', concat(v_output));
	classify(q'[CREATE OUTLINE]', v_output); assert_equals('CREATE OUTLINE', 'DDL|ALTER|CREATE OUTLINE|180', concat(v_output));
	classify(q'[CREATE PACKAGE]', v_output); assert_equals('CREATE PACKAGE', 'DDL|ALTER|CREATE PACKAGE|94', concat(v_output));
	classify(q'[CREATE PACKAGE BODY]', v_output); assert_equals('CREATE PACKAGE BODY', 'DDL|ALTER|CREATE PACKAGE BODY|97', concat(v_output));
	classify(q'[CREATE PFILE]', v_output); assert_equals('CREATE PFILE', 'DDL|ALTER|CREATE PFILE|188', concat(v_output));
	classify(q'[CREATE PLUGGABLE DATABASE]', v_output); assert_equals('CREATE PLUGGABLE DATABASE', 'DDL|ALTER|CREATE PLUGGABLE DATABASE|226', concat(v_output));
	classify(q'[CREATE PROCEDURE]', v_output); assert_equals('CREATE PROCEDURE', 'DDL|ALTER|CREATE PROCEDURE|24', concat(v_output));
	classify(q'[CREATE PROFILE]', v_output); assert_equals('CREATE PROFILE', 'DDL|ALTER|CREATE PROFILE|65', concat(v_output));
	classify(q'[CREATE RESTORE POINT]', v_output); assert_equals('CREATE RESTORE POINT', 'DDL|ALTER|CREATE RESTORE POINT|206', concat(v_output));
	classify(q'[CREATE ROLE]', v_output); assert_equals('CREATE ROLE', 'DDL|ALTER|CREATE ROLE|52', concat(v_output));
	classify(q'[CREATE ROLLBACK SEGMENT]', v_output); assert_equals('CREATE ROLLBACK SEGMENT', 'DDL|ALTER|CREATE ROLLBACK SEGMENT|36', concat(v_output));
	classify(q'[CREATE SCHEMA]', v_output); assert_equals('CREATE SCHEMA', 'DDL|ALTER|CREATE SCHEMA|56', concat(v_output));
	classify(q'[CREATE SCHEMA SYNONYM]', v_output); assert_equals('CREATE SCHEMA SYNONYM', 'DDL|ALTER|CREATE SCHEMA SYNONYM|222', concat(v_output));
	classify(q'[CREATE SEQUENCE]', v_output); assert_equals('CREATE SEQUENCE', 'DDL|ALTER|CREATE SEQUENCE|13', concat(v_output));
	classify(q'[CREATE SPFILE]', v_output); assert_equals('CREATE SPFILE', 'DDL|ALTER|CREATE SPFILE|187', concat(v_output));
	classify(q'[CREATE SUMMARY]', v_output); assert_equals('CREATE SUMMARY', 'DDL|ALTER|CREATE SUMMARY|171', concat(v_output));
	classify(q'[CREATE SYNONYM]', v_output); assert_equals('CREATE SYNONYM', 'DDL|ALTER|CREATE SYNONYM|19', concat(v_output));
	classify(q'[CREATE TABLE]', v_output); assert_equals('CREATE TABLE', 'DDL|ALTER|CREATE TABLE|1', concat(v_output));
	classify(q'[CREATE TABLESPACE]', v_output); assert_equals('CREATE TABLESPACE', 'DDL|ALTER|CREATE TABLESPACE|39', concat(v_output));
	classify(q'[CREATE TRIGGER]', v_output); assert_equals('CREATE TRIGGER', 'DDL|ALTER|CREATE TRIGGER|59', concat(v_output));
	classify(q'[CREATE TYPE]', v_output); assert_equals('CREATE TYPE', 'DDL|ALTER|CREATE TYPE|77', concat(v_output));
	classify(q'[CREATE TYPE BODY]', v_output); assert_equals('CREATE TYPE BODY', 'DDL|ALTER|CREATE TYPE BODY|81', concat(v_output));
	classify(q'[CREATE USER]', v_output); assert_equals('CREATE USER', 'DDL|ALTER|CREATE USER|51', concat(v_output));
	classify(q'[CREATE VIEW]', v_output); assert_equals('CREATE VIEW', 'DDL|ALTER|CREATE VIEW|21', concat(v_output));
	classify(q'[DECLARE REWRITE EQUIVALENCE]', v_output); assert_equals('DECLARE REWRITE EQUIVALENCE', 'DDL|ALTER|DECLARE REWRITE EQUIVALENCE|209', concat(v_output));
	classify(q'[DELETE]', v_output); assert_equals('DELETE', 'DDL|ALTER|DELETE|7', concat(v_output));
	classify(q'[DISASSOCIATE STATISTICS]', v_output); assert_equals('DISASSOCIATE STATISTICS', 'DDL|ALTER|DISASSOCIATE STATISTICS|169', concat(v_output));
	classify(q'[DROP ASSEMBLY]', v_output); assert_equals('DROP ASSEMBLY', 'DDL|ALTER|DROP ASSEMBLY|215', concat(v_output));
	classify(q'[DROP AUDIT POLICY]', v_output); assert_equals('DROP AUDIT POLICY', 'DDL|ALTER|DROP AUDIT POLICY|231', concat(v_output));
	classify(q'[DROP BITMAPFILE]', v_output); assert_equals('DROP BITMAPFILE', 'DDL|ALTER|DROP BITMAPFILE|89', concat(v_output));
	classify(q'[DROP CLUSTER]', v_output); assert_equals('DROP CLUSTER', 'DDL|ALTER|DROP CLUSTER|8', concat(v_output));
	classify(q'[DROP CONTEXT]', v_output); assert_equals('DROP CONTEXT', 'DDL|ALTER|DROP CONTEXT|178', concat(v_output));
	classify(q'[DROP DATABASE]', v_output); assert_equals('DROP DATABASE', 'DDL|ALTER|DROP DATABASE|203', concat(v_output));
	classify(q'[DROP DATABASE LINK]', v_output); assert_equals('DROP DATABASE LINK', 'DDL|ALTER|DROP DATABASE LINK|33', concat(v_output));
	classify(q'[DROP DIMENSION]', v_output); assert_equals('DROP DIMENSION', 'DDL|ALTER|DROP DIMENSION|176', concat(v_output));
	classify(q'[DROP DIRECTORY]', v_output); assert_equals('DROP DIRECTORY', 'DDL|ALTER|DROP DIRECTORY|158', concat(v_output));
	classify(q'[DROP DISK GROUP]', v_output); assert_equals('DROP DISK GROUP', 'DDL|ALTER|DROP DISK GROUP|195', concat(v_output));
	classify(q'[DROP EDITION]', v_output); assert_equals('DROP EDITION', 'DDL|ALTER|DROP EDITION|214', concat(v_output));
	classify(q'[DROP FLASHBACK ARCHIVE]', v_output); assert_equals('DROP FLASHBACK ARCHIVE', 'DDL|ALTER|DROP FLASHBACK ARCHIVE|220', concat(v_output));
	classify(q'[DROP FUNCTION]', v_output); assert_equals('DROP FUNCTION', 'DDL|ALTER|DROP FUNCTION|93', concat(v_output));
	classify(q'[DROP INDEX]', v_output); assert_equals('DROP INDEX', 'DDL|ALTER|DROP INDEX|10', concat(v_output));
	classify(q'[DROP INDEXTYPE]', v_output); assert_equals('DROP INDEXTYPE', 'DDL|ALTER|DROP INDEXTYPE|165', concat(v_output));
	classify(q'[DROP JAVA]', v_output); assert_equals('DROP JAVA', 'DDL|ALTER|DROP JAVA|162', concat(v_output));
	classify(q'[DROP LIBRARY]', v_output); assert_equals('DROP LIBRARY', 'DDL|ALTER|DROP LIBRARY|84', concat(v_output));
	classify(q'[DROP MATERIALIZED VIEW ]', v_output); assert_equals('DROP MATERIALIZED VIEW ', 'DDL|ALTER|DROP MATERIALIZED VIEW |76', concat(v_output));
	classify(q'[DROP MATERIALIZED VIEW  LOG]', v_output); assert_equals('DROP MATERIALIZED VIEW  LOG', 'DDL|ALTER|DROP MATERIALIZED VIEW  LOG|73', concat(v_output));
	classify(q'[DROP MATERIALIZED ZONEMAP]', v_output); assert_equals('DROP MATERIALIZED ZONEMAP', 'DDL|ALTER|DROP MATERIALIZED ZONEMAP|241', concat(v_output));
	classify(q'[DROP OPERATOR]', v_output); assert_equals('DROP OPERATOR', 'DDL|ALTER|DROP OPERATOR|167', concat(v_output));
	classify(q'[DROP OUTLINE]', v_output); assert_equals('DROP OUTLINE', 'DDL|ALTER|DROP OUTLINE|181', concat(v_output));
	classify(q'[DROP PACKAGE]', v_output); assert_equals('DROP PACKAGE', 'DDL|ALTER|DROP PACKAGE|96', concat(v_output));
	classify(q'[DROP PACKAGE BODY]', v_output); assert_equals('DROP PACKAGE BODY', 'DDL|ALTER|DROP PACKAGE BODY|99', concat(v_output));
	classify(q'[DROP PLUGGABLE DATABASE]', v_output); assert_equals('DROP PLUGGABLE DATABASE', 'DDL|ALTER|DROP PLUGGABLE DATABASE|228', concat(v_output));
	classify(q'[DROP PROCEDURE]', v_output); assert_equals('DROP PROCEDURE', 'DDL|ALTER|DROP PROCEDURE|68', concat(v_output));
	classify(q'[DROP PROFILE]', v_output); assert_equals('DROP PROFILE', 'DDL|ALTER|DROP PROFILE|66', concat(v_output));
	classify(q'[DROP RESTORE POINT]', v_output); assert_equals('DROP RESTORE POINT', 'DDL|ALTER|DROP RESTORE POINT|207', concat(v_output));
	classify(q'[DROP REWRITE EQUIVALENCE]', v_output); assert_equals('DROP REWRITE EQUIVALENCE', 'DDL|ALTER|DROP REWRITE EQUIVALENCE|211', concat(v_output));
	classify(q'[DROP ROLE]', v_output); assert_equals('DROP ROLE', 'DDL|ALTER|DROP ROLE|54', concat(v_output));
	classify(q'[DROP ROLLBACK SEGMENT]', v_output); assert_equals('DROP ROLLBACK SEGMENT', 'DDL|ALTER|DROP ROLLBACK SEGMENT|38', concat(v_output));
	classify(q'[DROP SCHEMA SYNONYM]', v_output); assert_equals('DROP SCHEMA SYNONYM', 'DDL|ALTER|DROP SCHEMA SYNONYM|224', concat(v_output));
	classify(q'[DROP SEQUENCE]', v_output); assert_equals('DROP SEQUENCE', 'DDL|ALTER|DROP SEQUENCE|16', concat(v_output));
	classify(q'[DROP SUMMARY]', v_output); assert_equals('DROP SUMMARY', 'DDL|ALTER|DROP SUMMARY|173', concat(v_output));
	classify(q'[DROP SYNONYM]', v_output); assert_equals('DROP SYNONYM', 'DDL|ALTER|DROP SYNONYM|20', concat(v_output));
	classify(q'[DROP TABLE]', v_output); assert_equals('DROP TABLE', 'DDL|ALTER|DROP TABLE|12', concat(v_output));
	classify(q'[DROP TABLESPACE]', v_output); assert_equals('DROP TABLESPACE', 'DDL|ALTER|DROP TABLESPACE|41', concat(v_output));
	classify(q'[DROP TRIGGER]', v_output); assert_equals('DROP TRIGGER', 'DDL|ALTER|DROP TRIGGER|61', concat(v_output));
	classify(q'[DROP TYPE]', v_output); assert_equals('DROP TYPE', 'DDL|ALTER|DROP TYPE|78', concat(v_output));
	classify(q'[DROP TYPE BODY]', v_output); assert_equals('DROP TYPE BODY', 'DDL|ALTER|DROP TYPE BODY|83', concat(v_output));
	classify(q'[DROP USER]', v_output); assert_equals('DROP USER', 'DDL|ALTER|DROP USER|53', concat(v_output));
	classify(q'[DROP VIEW]', v_output); assert_equals('DROP VIEW', 'DDL|ALTER|DROP VIEW|22', concat(v_output));
	classify(q'[Do not use 184]', v_output); assert_equals('Do not use 184', 'DDL|ALTER|Do not use 184|184', concat(v_output));
	classify(q'[Do not use 185]', v_output); assert_equals('Do not use 185', 'DDL|ALTER|Do not use 185|185', concat(v_output));
	classify(q'[Do not use 186]', v_output); assert_equals('Do not use 186', 'DDL|ALTER|Do not use 186|186', concat(v_output));
	classify(q'[EXPLAIN]', v_output); assert_equals('EXPLAIN', 'DDL|ALTER|EXPLAIN|50', concat(v_output));
	classify(q'[FLASHBACK DATABASE]', v_output); assert_equals('FLASHBACK DATABASE', 'DDL|ALTER|FLASHBACK DATABASE|204', concat(v_output));
	classify(q'[FLASHBACK TABLE]', v_output); assert_equals('FLASHBACK TABLE', 'DDL|ALTER|FLASHBACK TABLE|205', concat(v_output));
	classify(q'[GRANT OBJECT]', v_output); assert_equals('GRANT OBJECT', 'DDL|ALTER|GRANT OBJECT|17', concat(v_output));
	classify(q'[INSERT]', v_output); assert_equals('INSERT', 'DDL|ALTER|INSERT|2', concat(v_output));
	classify(q'[LOCK TABLE]', v_output); assert_equals('LOCK TABLE', 'DDL|ALTER|LOCK TABLE|26', concat(v_output));
	classify(q'[NO-OP]', v_output); assert_equals('NO-OP', 'DDL|ALTER|NO-OP|27', concat(v_output));
	classify(q'[NOAUDIT OBJECT]', v_output); assert_equals('NOAUDIT OBJECT', 'DDL|ALTER|NOAUDIT OBJECT|31', concat(v_output));
	classify(q'[PL/SQL EXECUTE]', v_output); assert_equals('PL/SQL EXECUTE', 'DDL|ALTER|PL/SQL EXECUTE|47', concat(v_output));
	classify(q'[PURGE DBA RECYCLEBIN]', v_output); assert_equals('PURGE DBA RECYCLEBIN', 'DDL|ALTER|PURGE DBA RECYCLEBIN|198', concat(v_output));
	classify(q'[PURGE INDEX]', v_output); assert_equals('PURGE INDEX', 'DDL|ALTER|PURGE INDEX|201', concat(v_output));
	classify(q'[PURGE TABLE]', v_output); assert_equals('PURGE TABLE', 'DDL|ALTER|PURGE TABLE|200', concat(v_output));
	classify(q'[PURGE TABLESPACE]', v_output); assert_equals('PURGE TABLESPACE', 'DDL|ALTER|PURGE TABLESPACE|199', concat(v_output));
	classify(q'[PURGE USER RECYCLEBIN]', v_output); assert_equals('PURGE USER RECYCLEBIN', 'DDL|ALTER|PURGE USER RECYCLEBIN|197', concat(v_output));
	classify(q'[RENAME]', v_output); assert_equals('RENAME', 'DDL|ALTER|RENAME|28', concat(v_output));
	classify(q'[REVOKE OBJECT]', v_output); assert_equals('REVOKE OBJECT', 'DDL|ALTER|REVOKE OBJECT|18', concat(v_output));
	classify(q'[ROLLBACK]', v_output); assert_equals('ROLLBACK', 'Transaction Control|ROLLBACK|ROLLBACK|45', concat(v_output));
	classify(q'[SAVEPOINT]', v_output); assert_equals('SAVEPOINT', 'Transaction Control|SAVEPOINT|SAVEPOINT|46', concat(v_output));
	classify(q'[SELECT]', v_output); assert_equals('SELECT', 'DDL|ALTER|SELECT|3', concat(v_output));
	--TODO
	classify(q'[SET CONSTRAINTS]', v_output); assert_equals('SET CONSTRAINT', 'Transaction Control|SET CONSTRAINT|SET CONSTRAINTS|90', concat(v_output));
	classify(q'[SET ROLE]', v_output); assert_equals('SET ROLE', 'Session Control|SET ROLE|SET ROLE|55', concat(v_output));
	classify(q'[SET TRANSACTION]', v_output); assert_equals('SET TRANSACTION', 'Transaction Control|SET TRANSACTION|SET TRANSACTION|48', concat(v_output));
	classify(q'[TRUNCATE CLUSTER]', v_output); assert_equals('TRUNCATE CLUSTER', 'DDL|ALTER|TRUNCATE CLUSTER|86', concat(v_output));
	classify(q'[TRUNCATE TABLE]', v_output); assert_equals('TRUNCATE TABLE', 'DDL|ALTER|TRUNCATE TABLE|85', concat(v_output));
	classify(q'[UNDROP OBJECT]', v_output); assert_equals('UNDROP OBJECT', 'DDL|ALTER|UNDROP OBJECT|202', concat(v_output));
	classify(q'[UPDATE]', v_output); assert_equals('UPDATE', 'DDL|ALTER|UPDATE|6', concat(v_output));
	classify(q'[UPDATE INDEXES]', v_output); assert_equals('UPDATE INDEXES', 'DDL|ALTER|UPDATE INDEXES|182', concat(v_output));
	classify(q'[UPDATE JOIN INDEX]', v_output); assert_equals('UPDATE JOIN INDEX', 'DDL|ALTER|UPDATE JOIN INDEX|191', concat(v_output));
	classify(q'[UPSERT]', v_output); assert_equals('UPSERT', 'DDL|ALTER|UPSERT|189', concat(v_output));
	classify(q'[VALIDATE INDEX]', v_output); assert_equals('VALIDATE INDEX', 'DDL|ALTER|VALIDATE INDEX|23', concat(v_output));

	/*
	PL/SQL
		Block
	*/

end test_commands;


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
	v_lex_sqlcode number;
	v_lex_sqlerrm varchar2(4000);
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
	if bitand(p_tests, c_errors)        > 0 then test_errors; end if;
	if bitand(p_tests, c_commands)      > 0 then test_commands; end if;
	if bitand(p_tests, c_dynamic_tests) > 0 then dynamic_tests; end if;

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
