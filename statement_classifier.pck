create or replace package statement_classifier is
/*
## Purpose ##

Determine the category, statement type, command name, and command type (id) for
a single SQL or PL/SQL statement.  This package does not fully parse and
validate a statement.  It only provides enough information to help other code
decide the proper context for running a statement.

Category is DDL, DML, Transaction Control, Session Control, System Control, or PL/SQL.
 - Statement type is more descriptive, for example INSERT, SELECT, or CREATE.
  - Command name and command type are based on V$SQLCOMMAND, and are more specific
    versions than statement type.  For example, "CREATE PLUGGABLE DATABASE", 226.

Some example uses:
- A SELECT statement may require inspecting the results.
- A DML statement may benefit from looking at SQL%ROWCOUNT.
- PL/SQL can be prevented from running.


## References ##

The category names and associations are based on the Oracle 12c "Types of SQL
Statements" chapter of "Database SQL Language Reference", as well as the
"Blocks" chapter of the "Database PL/SQL Language Reference".

SQL: http://docs.oracle.com/database/121/SQLRF/statements_1001.htm#SQLRF30001
PL/SQL: http://docs.oracle.com/database/121/LNPLS/block.htm#LNPLS01303

Putting them together, here are the relationships:

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

Command name and command type are based on V$SQLCOMMAND.  The COMMAND_TYPE
column can be found in V$SQL, DBA_HIST_SQLTEXT, etc.


## Lexer details ##

Classifying the category and statement type does not require a full scanner
and parser.  Many shortcuts are possible because only the first two keywords
are needed.  None of the statements allow literals, operators, or other
identifiers before the necessary keywords.  So the lexer only needs to deal
with comments, whitespace, a few operators, and identifiers.

A simple review of the SQL Language Reference shows that 99% of the time
a statement type can be identified simply by the name of the statement type.
For example, "CREATE" must start with keyword CREATE.  The only exceptions are
SELECT can start with "WITH" or "(", and a PL/SQL Block can start with "<<",
"DECLARE", or "BEGIN".

See PLSQL_LEXER for more details on possible tokens.


## Example ##

declare
	v_category varchar2(19);
	v_statement_type varchar2(25);
begin
	statement_classifier.classify('select * from dual', v_category, v_statement_type);
	dbms_output.put_line('Category: '||v_category);
	dbms_output.put_line('Statement Type: '||v_statement_type);
end;


*/
	procedure classify(
		p_statement in clob,
		p_category out varchar2,
		p_statement_type out varchar2,
		p_command_name out varchar2,
		p_command_type out number
	);

end;
/
create or replace package body statement_classifier is

--Constants
C_DDL                 constant varchar2(100) := 'DDL';
C_DML                 constant varchar2(100) := 'DML';
C_Transaction_Control constant varchar2(100) := 'Transaction Control';
C_Session_Control     constant varchar2(100) := 'Session Control';
C_System_Control      constant varchar2(100) := 'System Control';
C_PLSQL               constant varchar2(100) := 'PL/SQL';


--------------------------------------------------------------------------------
--Return the category and statement type for a single statement.
procedure classify(
		p_statement in clob,
		p_category out varchar2,
		p_statement_type out varchar2,
		p_command_name out varchar2,
		p_command_type out number
) is
	v_tokens_with_extra_stuff token_table := plsql_lexer.tokenize(p_statement);
	v_tokens token_table := token_table();
	type string_table is table of varchar2(4000) index by pls_integer; 
	v_types string_table;
	v_values string_table;
	v_symbols_1 varchar2(4000);
	v_symbols_1_to_2 varchar2(4000);
	v_symbols_1_to_3 varchar2(4000);
	v_symbols_1_to_4 varchar2(4000);
begin
	--DEBUG:
	--dbms_output.put_line(print_tokens(v_tokens_with_whitespace));

	--Remove whitespace, comments, and optional keywords.
	--All of the optional keywords in the SQL Language Reference are not used for
	--determining the category, statement type, command name, or command type.
	--For example, ALTER [SHARED|PUBLIC]? DATABASE LINK is DDL/ALTER/ALTER DATABASE LINK/225.
	--The keywords SHARED and PUBLIC can be discarded.  Luckily, *all* those
	--optional keywords can *always* be discarded, leaving a nice linear path.
	for i in 1 .. v_tokens_with_extra_stuff.count loop
		if v_tokens_with_extra_stuff(i).type not in ('whitespace', 'comment')
			and not
			(
				v_tokens_with_extra_stuff(i).type = 'symbol'
				and upper(v_tokens_with_extra_stuff(i).type) in
				(
					'AND','BIGFILE','BITMAP','COMPILE','EDITIONABLE','FORCE','FORCE','GLOBAL',
					'NO','NOFORCE','NONEDITIONABLE','OR','PRIVATE','PUBLIC','REPLACE','RESOLVE',
					'SHARED','SMALLFILE','STANDBY','TEMPORARY','UNDO','UNIQUE'
				)
			)
		then
			v_tokens.extend;
			v_tokens(v_tokens.count) := v_tokens_with_extra_stuff(i);
		end if;
	end loop;

	--Get first 4 token types and values in more convenient formats, and in upper case for values.
	for i in 1 .. greatest(v_tokens.count, 4) loop
		if v_tokens.exists(i) then
			v_types(i) := v_tokens(i).type;
			v_values(i) := upper(v_tokens(i).value);

			if v_types(i) = 'symbol' and i = 1 then
				v_symbols_1 := upper(v_tokens(i).value);
			elsif v_types(i) = 'symbol' and i = 2 then
				v_symbols_1_to_2 := upper(v_symbols_1 || ' ' || v_tokens(i).value);
			elsif v_types(i) = 'symbol' and i = 3 then
				v_symbols_1_to_3 := upper(v_symbols_1_to_2 || ' ' || v_tokens(i).value);
			elsif v_types(i) = 'symbol' and i = 4 then
				v_symbols_1_to_4 := upper(v_symbols_1_to_3 || ' ' || v_tokens(i).value);
			end if;
		end if;
	end loop;

	--See the description in the package specification for details of this.
	--
	--This SQL generates most of the code for classification of tokens.
	--The category and statement types are modified, and some exceptions are commented.
	--The real list has a few exceptions (such as "SELECT", which can start with a
	--"(" or a "WITH"), and some differences in order.  For example,
	--"ALTER DATABASE LINK" is more specific than "ALTER DATABASE" and is listed first.
	--
	/*
		select v$sqlcommand.*,
		'	elsif '||
			--TRIM and REPLACE remove some extra spaces.
			case regexp_count(trim(replace(command_name, '  ', ' ')), ' ')
				when 0 then 'v_symbols_1'
				when 1 then 'v_symbols_1_to_2'
				when 2 then 'v_symbols_1_to_3'
				when 3 then 'v_symbols_1_to_4'
				else '????'
			end||' = '''||command_name||''' then
				p_category := ''DDL''; p_statement_type := '''||
				regexp_substr(command_name, '^[^ ]*') --Statement type is usually just the first word.
				||'''; p_command_name := '''||command_name||'''; p_command_type := '||command_type||';'
			v_plsql
		from v$sqlcommand
		order by command_name;
	*/
	if v_symbols_1_to_3 = 'ADMINISTER KEY MANAGEMENT' then --Not in "Types of SQL Statements".
		p_category := C_DDL; p_statement_type := 'ADMINISTER'; p_command_name := 'ADMINISTER KEY MANAGEMENT'; p_command_type := 238;
	elsif v_symbols_1_to_2 = 'ALTER ASSEMBLY' then --I don't think this is a real command.
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER ASSEMBLY'; p_command_type := 217;
	elsif v_symbols_1_to_3 = 'ALTER AUDIT POLICY' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER AUDIT POLICY'; p_command_type := 230;
	elsif v_symbols_1_to_2 = 'ALTER CLUSTER' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER CLUSTER'; p_command_type := 5;
	elsif v_symbols_1_to_3 = 'ALTER DATABASE LINK' then --Moved above "ALTER DATABASE" to capture more specific case first.
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER DATABASE LINK'; p_command_type := 225;
	elsif v_symbols_1_to_2 = 'ALTER DATABASE' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER DATABASE'; p_command_type := 35;
	elsif v_symbols_1_to_2 = 'ALTER DIMENSION' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER DIMENSION'; p_command_type := 175;
	elsif v_symbols_1_to_2 = 'ALTER DISKGROUP' then --The real command is "DISKGROUP", not "DISK GROUP".
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER DISK GROUP'; p_command_type := 193;
	--Not sure if this is real.  It's not documented, and doesn't work on my 12.1.0.2.
	--But it's mentioned in some respectable websites.  Guess it doesn't hurt to include it.
	elsif v_symbols_1_to_2 = 'ALTER EDITION' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER EDITION'; p_command_type := 213;
	elsif v_symbols_1_to_3 = 'ALTER FLASHBACK ARCHIVE' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER FLASHBACK ARCHIVE'; p_command_type := 219;
	elsif v_symbols_1_to_2 = 'ALTER FUNCTION' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER FUNCTION'; p_command_type := 92;
	elsif v_symbols_1_to_2 = 'ALTER INDEX' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER INDEX'; p_command_type := 11;
	elsif v_symbols_1_to_2 = 'ALTER INDEXTYPE' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER INDEXTYPE'; p_command_type := 166;
	elsif v_symbols_1_to_2 = 'ALTER JAVA' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER JAVA'; p_command_type := 161;
	elsif v_symbols_1_to_2 = 'ALTER LIBRARY' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER LIBRARY'; p_command_type := 196;
	elsif v_symbols_1_to_4 = 'ALTER MATERIALIZED VIEW LOG' then --Moved above "ALTER MATERIALIZED VIEW" to capture more specific case first.
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER MATERIALIZED VIEW LOG'; p_command_type := 72;
	elsif v_symbols_1_to_3 = 'ALTER MATERIALIZED VIEW' then --The command name has an extra space at the end.
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER MATERIALIZED VIEW '; p_command_type := 75;
	elsif v_symbols_1_to_3 = 'ALTER MATERIALIZED ZONEMAP' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER MATERIALIZED ZONEMAP'; p_command_type := 240;
	elsif v_symbols_1_to_2 = 'ALTER OPERATOR' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER OPERATOR'; p_command_type := 183;
	elsif v_symbols_1_to_2 = 'ALTER OUTLINE' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER OUTLINE'; p_command_type := 179;
	elsif v_symbols_1_to_3 = 'ALTER PACKAGE BODY' then --Moved above "ALTER PACKAGE" to capture more specific case first.
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER PACKAGE BODY'; p_command_type := 98;
	elsif v_symbols_1_to_2 = 'ALTER PACKAGE' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER PACKAGE'; p_command_type := 95;
	elsif v_symbols_1_to_3 = 'ALTER PLUGGABLE DATABASE' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER PLUGGABLE DATABASE'; p_command_type := 227;
	elsif v_symbols_1_to_2 = 'ALTER PROCEDURE' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER PROCEDURE'; p_command_type := 25;
	elsif v_symbols_1_to_2 = 'ALTER PROFILE' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER PROFILE'; p_command_type := 67;
	elsif v_symbols_1_to_3 = 'ALTER RESOURCE COST' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER RESOURCE COST'; p_command_type := 70;
	elsif v_symbols_1_to_3 = 'ALTER REWRITE EQUIVALENCE' then --I don't think this is a real commmand.
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER REWRITE EQUIVALENCE'; p_command_type := 210;
	elsif v_symbols_1_to_2 = 'ALTER ROLE' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER ROLE'; p_command_type := 79;
	elsif v_symbols_1_to_3 = 'ALTER ROLLBACK SEGMENT' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER ROLLBACK SEGMENT'; p_command_type := 37;
	elsif v_symbols_1_to_2 = 'ALTER SEQUENCE' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER SEQUENCE'; p_command_type := 14;
	elsif v_symbols_1_to_2 = 'ALTER SESSION' then --Different than other ALTERs.
		p_category := C_Session_Control; p_statement_type := 'ALTER SESSION'; p_command_name := 'ALTER SESSION'; p_command_type := 42;
	elsif v_symbols_1_to_2 = 'ALTER SUMMARY' then --An old version of "ALTER SNAPSHOT"?  Not sure if this is still used.
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER SUMMARY'; p_command_type := 172;
	elsif v_symbols_1_to_2 = 'ALTER SYNONYM' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER SYNONYM'; p_command_type := 192;
	elsif v_symbols_1_to_2 = 'ALTER SYSTEM' then --Different than other ALTERs.
		p_category := C_System_Control; p_statement_type := 'ALTER SYSTEM'; p_command_name := 'ALTER SYSTEM'; p_command_type := 49;
	elsif v_symbols_1_to_2 = 'ALTER TABLE' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER TABLE'; p_command_type := 15;
	elsif v_symbols_1_to_2 = 'ALTER TABLESPACE' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER TABLESPACE'; p_command_type := 40;
	elsif v_symbols_1_to_2 = 'ALTER TRACING' then --I don't think this is a real commmand.
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER TRACING'; p_command_type := 58;
	elsif v_symbols_1_to_2 = 'ALTER TRIGGER' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER TRIGGER'; p_command_type := 60;
	elsif v_symbols_1_to_3 = 'ALTER TYPE BODY' then --Moved above "ALTER PACKAGE" to capture more specific case first.
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER TYPE BODY'; p_command_type := 82;
	elsif v_symbols_1_to_2 = 'ALTER TYPE' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER TYPE'; p_command_type := 80;
	elsif v_symbols_1_to_2 = 'ALTER USER' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER USER'; p_command_type := 43;
	elsif v_symbols_1_to_2 = 'ALTER VIEW' then
		p_category := C_DDL; p_statement_type := 'ALTER'; p_command_name := 'ALTER VIEW'; p_command_type := 88;
	elsif v_symbols_1_to_2 = 'ANALYZE CLUSTER' then --Syntax diagram is incorrect, statement must start with "ANALYZE".
		p_category := C_DDL; p_statement_type := 'ANALYZE'; p_command_name := 'ANALYZE CLUSTER'; p_command_type := 64;
	elsif v_symbols_1_to_2 = 'ANALYZE INDEX' then
		p_category := C_DDL; p_statement_type := 'ANALYZE'; p_command_name := 'ANALYZE INDEX'; p_command_type := 63;
	elsif v_symbols_1_to_2 = 'ANALYZE TABLE' then
		p_category := C_DDL; p_statement_type := 'ANALYZE'; p_command_name := 'ANALYZE TABLE'; p_command_type := 62;
	elsif v_symbols_1_to_2 = 'ASSOCIATE STATISTICS' then
		p_category := C_DDL; p_statement_type := 'ASSOCIATE STATISTICS'; p_command_name := 'ASSOCIATE STATISTICS'; p_command_type := 168;
	elsif v_symbols_1_to_2 = 'AUDIT' then --The command name is more specific than the command.
		p_category := C_DDL; p_statement_type := 'AUDIT'; p_command_name := 'AUDIT OBJECT'; p_command_type := 30;
	elsif v_symbols_1 = 'CALL' then --The command name is more specific than the command.
		p_category := C_DML; p_statement_type := 'CALL'; p_command_name := 'CALL METHOD'; p_command_type := 170;
	elsif v_symbols_1_to_2 = 'CHANGE PASSWORD' then --I don't think this is a real command.
		p_category := C_DDL; p_statement_type := 'CHANGE'; p_command_name := 'CHANGE PASSWORD'; p_command_type := 190;
	elsif v_symbols_1 = 'COMMENT' then
		p_category := C_DDL; p_statement_type := 'COMMENT'; p_command_name := 'COMMENT'; p_command_type := 29;
	elsif v_symbols_1 = 'COMMIT' then
		p_category := C_Transaction_Control; p_statement_type := 'COMMIT'; p_command_name := 'COMMIT'; p_command_type := 44;
	elsif v_symbols_1_to_2 = 'CREATE ASSEMBLY' then --I don't think this is a real command.
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE ASSEMBLY'; p_command_type := 216;
	elsif v_symbols_1_to_3 = 'CREATE AUDIT POLICY' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE AUDIT POLICY'; p_command_type := 229;
	elsif v_symbols_1_to_2 = 'CREATE BITMAPFILE' then --I don't think this is a real command.
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE BITMAPFILE'; p_command_type := 87;
	elsif v_symbols_1_to_2 = 'CREATE CLUSTER' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE CLUSTER'; p_command_type := 4;
	elsif v_symbols_1_to_2 = 'CREATE CONTEXT' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE CONTEXT'; p_command_type := 177;
	elsif v_symbols_1_to_3 = 'CREATE CONTROL FILE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE CONTROL FILE'; p_command_type := 57;
	elsif v_symbols_1_to_3 = 'CREATE DATABASE LINK' then --Moved above "CREATE DATABASE" to capture more specific case first.
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE DATABASE LINK'; p_command_type := 32;
	elsif v_symbols_1_to_2 = 'CREATE DATABASE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE DATABASE'; p_command_type := 34;
	elsif v_symbols_1_to_2 = 'CREATE DIMENSION' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE DIMENSION'; p_command_type := 174;
	elsif v_symbols_1_to_2 = 'CREATE DIRECTORY' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE DIRECTORY'; p_command_type := 157;
	elsif v_symbols_1_to_3 = 'CREATE DISKGROUP' then --Command name has extra space, real command is "DISKGROUP".
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE DISK GROUP'; p_command_type := 194;
	elsif v_symbols_1_to_2 = 'CREATE EDITION' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE EDITION'; p_command_type := 212;
	elsif v_symbols_1_to_3 = 'CREATE FLASHBACK ARCHIVE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE FLASHBACK ARCHIVE'; p_command_type := 218;
	elsif v_symbols_1_to_2 = 'CREATE FUNCTION' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE FUNCTION'; p_command_type := 91;
	elsif v_symbols_1_to_2 = 'CREATE INDEX' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE INDEX'; p_command_type := 9;
	elsif v_symbols_1_to_2 = 'CREATE INDEXTYPE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE INDEXTYPE'; p_command_type := 164;
	elsif v_symbols_1_to_2 = 'CREATE JAVA' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE JAVA'; p_command_type := 160;
	elsif v_symbols_1_to_2 = 'CREATE LIBRARY' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE LIBRARY'; p_command_type := 159;
	elsif v_symbols_1_to_4 = 'CREATE MATERIALIZED VIEW LOG' then --Moved above "CREATE MATERIALIZED VIEW" to capture more specific case first.
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE MATERIALIZED VIEW LOG'; p_command_type := 71;
	elsif v_symbols_1_to_3 = 'CREATE MATERIALIZED VIEW' then --Extra space in command name.
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE MATERIALIZED VIEW '; p_command_type := 74;
	elsif v_symbols_1_to_3 = 'CREATE MATERIALIZED ZONEMAP' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE MATERIALIZED ZONEMAP'; p_command_type := 239;
	elsif v_symbols_1_to_2 = 'CREATE OPERATOR' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE OPERATOR'; p_command_type := 163;
	elsif v_symbols_1_to_2 = 'CREATE OUTLINE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE OUTLINE'; p_command_type := 180;
	elsif v_symbols_1_to_3 = 'CREATE PACKAGE BODY' then --Moved above "CREATE PACKAGE" to capture more specific case first.
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE PACKAGE BODY'; p_command_type := 97;
	elsif v_symbols_1_to_2 = 'CREATE PACKAGE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE PACKAGE'; p_command_type := 94;
	elsif v_symbols_1_to_2 = 'CREATE PFILE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE PFILE'; p_command_type := 188;
	elsif v_symbols_1_to_3 = 'CREATE PLUGGABLE DATABASE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE PLUGGABLE DATABASE'; p_command_type := 226;
	elsif v_symbols_1_to_2 = 'CREATE PROCEDURE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE PROCEDURE'; p_command_type := 24;
	elsif v_symbols_1_to_2 = 'CREATE PROFILE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE PROFILE'; p_command_type := 65;
	elsif v_symbols_1_to_3 = 'CREATE RESTORE POINT' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE RESTORE POINT'; p_command_type := 206;
	elsif v_symbols_1_to_2 = 'CREATE ROLE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE ROLE'; p_command_type := 52;
	elsif v_symbols_1_to_3 = 'CREATE ROLLBACK SEGMENT' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE ROLLBACK SEGMENT'; p_command_type := 36;
	elsif v_symbols_1_to_3 = 'CREATE SCHEMA SYNONYM' then --Undocumented feature.  Moved above "CREATE SCHEMA" to capture more specific case first.
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE SCHEMA SYNONYM'; p_command_type := 222;
	elsif v_symbols_1_to_2 = 'CREATE SCHEMA' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE SCHEMA'; p_command_type := 56;
	elsif v_symbols_1_to_2 = 'CREATE SEQUENCE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE SEQUENCE'; p_command_type := 13;
	elsif v_symbols_1_to_2 = 'CREATE SPFILE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE SPFILE'; p_command_type := 187;
	elsif v_symbols_1_to_2 = 'CREATE SUMMARY' then --Not a real command, I think this is an old version of "SNAPSHOT".
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE SUMMARY'; p_command_type := 171;
	elsif v_symbols_1_to_2 = 'CREATE SYNONYM' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE SYNONYM'; p_command_type := 19;
	elsif v_symbols_1_to_2 = 'CREATE TABLE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE TABLE'; p_command_type := 1;
	elsif v_symbols_1_to_2 = 'CREATE TABLESPACE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE TABLESPACE'; p_command_type := 39;
	elsif v_symbols_1_to_2 = 'CREATE TRIGGER' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE TRIGGER'; p_command_type := 59;
	elsif v_symbols_1_to_3 = 'CREATE TYPE BODY' then --Moved above "CREATE TYPE" to capture more specific case first.
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE TYPE BODY'; p_command_type := 81;
	elsif v_symbols_1_to_2 = 'CREATE TYPE' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE TYPE'; p_command_type := 77;
	elsif v_symbols_1_to_2 = 'CREATE USER' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE USER'; p_command_type := 51;
	elsif v_symbols_1_to_2 = 'CREATE VIEW' then
		p_category := C_DDL; p_statement_type := 'CREATE'; p_command_name := 'CREATE VIEW'; p_command_type := 21;
	/*
	elsif v_symbols_1_to_3 = 'DECLARE REWRITE EQUIVALENCE' then --Not a real command and could interfere with PL/SQL parsing.
		p_category := C_DDL; p_statement_type := 'DECLARE'; p_command_name := 'DECLARE REWRITE EQUIVALENCE'; p_command_type := 209;
	*/
	elsif v_symbols_1 = 'DELETE' then
		p_category := C_DML; p_statement_type := 'DELETE'; p_command_name := 'DELETE'; p_command_type := 7;
	elsif v_symbols_1_to_2 = 'DISASSOCIATE STATISTICS' then
		p_category := C_DDL; p_statement_type := 'DISASSOCIATE'; p_command_name := 'DISASSOCIATE STATISTICS'; p_command_type := 169;
	elsif v_symbols_1_to_2 = 'DROP ASSEMBLY' then --I don't think this is a real command.
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP ASSEMBLY'; p_command_type := 215;
	elsif v_symbols_1_to_3 = 'DROP AUDIT POLICY' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP AUDIT POLICY'; p_command_type := 231;
	elsif v_symbols_1_to_2 = 'DROP BITMAPFILE' then --I don't think this is a real command.
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP BITMAPFILE'; p_command_type := 89;
	elsif v_symbols_1_to_2 = 'DROP CLUSTER' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP CLUSTER'; p_command_type := 8;
	elsif v_symbols_1_to_2 = 'DROP CONTEXT' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP CONTEXT'; p_command_type := 178;
	elsif v_symbols_1_to_3 = 'DROP DATABASE LINK' then --Moved above "DROP DATABASE" to capture more specific case first.
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP DATABASE LINK'; p_command_type := 33;
	elsif v_symbols_1_to_2 = 'DROP DATABASE' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP DATABASE'; p_command_type := 203;
	elsif v_symbols_1_to_2 = 'DROP DIMENSION' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP DIMENSION'; p_command_type := 176;
	elsif v_symbols_1_to_2 = 'DROP DIRECTORY' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP DIRECTORY'; p_command_type := 158;
	elsif v_symbols_1_to_2 = 'DROP DISKGROUP' then --Command has an extra space in the name, it should be "DISKGROUP".
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP DISK GROUP'; p_command_type := 195;
	elsif v_symbols_1_to_2 = 'DROP EDITION' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP EDITION'; p_command_type := 214;
	elsif v_symbols_1_to_3 = 'DROP FLASHBACK ARCHIVE' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP FLASHBACK ARCHIVE'; p_command_type := 220;
	elsif v_symbols_1_to_2 = 'DROP FUNCTION' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP FUNCTION'; p_command_type := 93;
	elsif v_symbols_1_to_2 = 'DROP INDEX' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP INDEX'; p_command_type := 10;
	elsif v_symbols_1_to_2 = 'DROP INDEXTYPE' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP INDEXTYPE'; p_command_type := 165;
	elsif v_symbols_1_to_2 = 'DROP JAVA' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP JAVA'; p_command_type := 162;
	elsif v_symbols_1_to_2 = 'DROP LIBRARY' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP LIBRARY'; p_command_type := 84;
	elsif v_symbols_1_to_4 = 'DROP MATERIALIZED VIEW LOG' then --Command has an extra space in the name.  Moved above "DROP MATERIALIZED VIEW" to capture more specific case first.
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP MATERIALIZED VIEW  LOG'; p_command_type := 73;
	elsif v_symbols_1_to_3 = 'DROP MATERIALIZED VIEW' then --Command has an extra space at the end.
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP MATERIALIZED VIEW '; p_command_type := 76;
	elsif v_symbols_1_to_3 = 'DROP MATERIALIZED ZONEMAP' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP MATERIALIZED ZONEMAP'; p_command_type := 241;
	elsif v_symbols_1_to_2 = 'DROP OPERATOR' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP OPERATOR'; p_command_type := 167;
	elsif v_symbols_1_to_2 = 'DROP OUTLINE' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP OUTLINE'; p_command_type := 181;
	elsif v_symbols_1_to_3 = 'DROP PACKAGE BODY' then --Moved above "DROP PACKAGE" to capture more specific case first.
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP PACKAGE BODY'; p_command_type := 99;
	elsif v_symbols_1_to_2 = 'DROP PACKAGE' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP PACKAGE'; p_command_type := 96;
	elsif v_symbols_1_to_3 = 'DROP PLUGGABLE DATABASE' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP PLUGGABLE DATABASE'; p_command_type := 228;
	elsif v_symbols_1_to_2 = 'DROP PROCEDURE' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP PROCEDURE'; p_command_type := 68;
	elsif v_symbols_1_to_2 = 'DROP PROFILE' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP PROFILE'; p_command_type := 66;
	elsif v_symbols_1_to_3 = 'DROP RESTORE POINT' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP RESTORE POINT'; p_command_type := 207;
	elsif v_symbols_1_to_3 = 'DROP REWRITE EQUIVALENCE' then --I don't think this is a real command.
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP REWRITE EQUIVALENCE'; p_command_type := 211;
	elsif v_symbols_1_to_2 = 'DROP ROLE' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP ROLE'; p_command_type := 54;
	elsif v_symbols_1_to_3 = 'DROP ROLLBACK SEGMENT' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP ROLLBACK SEGMENT'; p_command_type := 38;
	elsif v_symbols_1_to_3 = 'DROP SCHEMA SYNONYM' then --Undocumented feature
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP SCHEMA SYNONYM'; p_command_type := 224;
	elsif v_symbols_1_to_2 = 'DROP SEQUENCE' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP SEQUENCE'; p_command_type := 16;
	elsif v_symbols_1_to_2 = 'DROP SUMMARY' then --I think this is an old version of "SNAPSHOT".
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP SUMMARY'; p_command_type := 173;
	elsif v_symbols_1_to_2 = 'DROP SYNONYM' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP SYNONYM'; p_command_type := 20;
	elsif v_symbols_1_to_2 = 'DROP TABLE' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP TABLE'; p_command_type := 12;
	elsif v_symbols_1_to_2 = 'DROP TABLESPACE' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP TABLESPACE'; p_command_type := 41;
	elsif v_symbols_1_to_2 = 'DROP TRIGGER' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP TRIGGER'; p_command_type := 61;
	elsif v_symbols_1_to_3 = 'DROP TYPE BODY' then --Moved above "DROP TYPE" to capture more specific case first.
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP TYPE BODY'; p_command_type := 83;
	elsif v_symbols_1_to_2 = 'DROP TYPE' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP TYPE'; p_command_type := 78;
	elsif v_symbols_1_to_2 = 'DROP USER' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP USER'; p_command_type := 53;
	elsif v_symbols_1_to_2 = 'DROP VIEW' then
		p_category := C_DDL; p_statement_type := 'DROP'; p_command_name := 'DROP VIEW'; p_command_type := 22;
	/*
	elsif v_symbols_1_to_4 = 'Do not use 184' then
		p_category := C_DDL; p_statement_type := 'Do'; p_command_name := 'Do not use 184'; p_command_type := 184;
	elsif v_symbols_1_to_4 = 'Do not use 185' then
		p_category := C_DDL; p_statement_type := 'Do'; p_command_name := 'Do not use 185'; p_command_type := 185;
	elsif v_symbols_1_to_4 = 'Do not use 186' then
		p_category := C_DDL; p_statement_type := 'Do'; p_command_name := 'Do not use 186'; p_command_type := 186;
	*/
	elsif v_symbols_1 = 'EXPLAIN' then --Statement type is more specific than command name.
		p_category := C_DML; p_statement_type := 'EXPLAIN PLAN'; p_command_name := 'EXPLAIN'; p_command_type := 50;
	elsif v_symbols_1_to_2 = 'FLASHBACK DATABASE' then
		p_category := C_DDL; p_statement_type := 'FLASHBACK'; p_command_name := 'FLASHBACK DATABASE'; p_command_type := 204;
	elsif v_symbols_1_to_2 = 'FLASHBACK TABLE' then
		p_category := C_DDL; p_statement_type := 'FLASHBACK'; p_command_name := 'FLASHBACK TABLE'; p_command_type := 205;
	elsif v_symbols_1_to_2 = 'GRANT OBJECT' then
		p_category := C_DDL; p_statement_type := 'GRANT'; p_command_name := 'GRANT OBJECT'; p_command_type := 17;
	elsif v_symbols_1 = 'INSERT' then
		p_category := C_DML; p_statement_type := 'INSERT'; p_command_name := 'INSERT'; p_command_type := 2;
	elsif v_symbols_1_to_2 = 'LOCK TABLE' then
		p_category := C_DML; p_statement_type := 'LOCK'; p_command_name := 'LOCK TABLE'; p_command_type := 26;
	elsif v_symbols_1 = 'NO-OP' then --I don't think this is a real command.
		p_category := C_DDL; p_statement_type := 'NO-OP'; p_command_name := 'NO-OP'; p_command_type := 27;
	elsif v_symbols_1 = 'NOAUDIT' then --Command name is more specific than statement type.
		p_category := C_DDL; p_statement_type := 'NOAUDIT'; p_command_name := 'NOAUDIT OBJECT'; p_command_type := 31;
	elsif (v_types(1) = '<' and v_types(2) = '<') --PL/SQL is custom.
			or v_types(1)='symbol' and v_values(1) in ('DECLARE', 'BEGIN') then
		p_category := C_PLSQL; p_statement_type := 'Block'; p_command_name := 'PL/SQL EXECUTE'; p_command_type := 47;
	elsif v_symbols_1_to_2 = 'PURGE DBA_RECYCLEBIN' then --Command name space instead of underscore.
		p_category := C_DDL; p_statement_type := 'PURGE'; p_command_name := 'PURGE DBA RECYCLEBIN'; p_command_type := 198;
	elsif v_symbols_1_to_2 = 'PURGE INDEX' then
		p_category := C_DDL; p_statement_type := 'PURGE'; p_command_name := 'PURGE INDEX'; p_command_type := 201;
	elsif v_symbols_1_to_2 = 'PURGE TABLE' then
		p_category := C_DDL; p_statement_type := 'PURGE'; p_command_name := 'PURGE TABLE'; p_command_type := 200;
	elsif v_symbols_1_to_2 = 'PURGE TABLESPACE' then
		p_category := C_DDL; p_statement_type := 'PURGE'; p_command_name := 'PURGE TABLESPACE'; p_command_type := 199;
	elsif v_symbols_1_to_2 = 'PURGE RECYCLEBIN' then --Command name has extra "USER".
		p_category := C_DDL; p_statement_type := 'PURGE'; p_command_name := 'PURGE USER RECYCLEBIN'; p_command_type := 197;
	elsif v_symbols_1 = 'RENAME' then
		p_category := C_DDL; p_statement_type := 'RENAME'; p_command_name := 'RENAME'; p_command_type := 28;
	elsif v_symbols_1 = 'REVOKE' then --Command has extra "OBJECT".
		p_category := C_DDL; p_statement_type := 'REVOKE'; p_command_name := 'REVOKE OBJECT'; p_command_type := 18;
	elsif v_symbols_1 = 'ROLLBACK' then
		p_category := C_Transaction_Control; p_statement_type := 'ROLLBACK'; p_command_name := 'ROLLBACK'; p_command_type := 45;
	elsif v_symbols_1 = 'SAVEPOINT' then
		p_category := C_Transaction_Control; p_statement_type := 'SAVEPOINT'; p_command_name := 'SAVEPOINT'; p_command_type := 46;
	elsif v_types(1) = '(' or v_symbols_1 in ('SELECT', 'WITH') then --SELECT is custom.
		p_category := C_DML; p_statement_type := 'SELECT';  p_command_name := 'SELECT'; p_command_type := 3;
	elsif v_symbols_1_to_2 in ('SET CONSTRAINTS', 'SET CONSTRAINT') then --Custom, there are two forms.
		p_category := C_Transaction_Control; p_statement_type := 'SET CONSTRAINT'; p_command_name := 'SET CONSTRAINTS'; p_command_type := 90;
	elsif v_symbols_1_to_2 = 'SET ROLE' then
		p_category := C_Session_Control; p_statement_type := 'SET ROLE'; p_command_name := 'SET ROLE'; p_command_type := 55;
	elsif v_symbols_1_to_2 = 'SET TRANSACTION' then
		p_category := C_Transaction_Control; p_statement_type := 'SET TRANSACTION'; p_command_name := 'SET TRANSACTION'; p_command_type := 48;
	elsif v_symbols_1_to_2 = 'TRUNCATE CLUSTER' then
		p_category := C_DDL; p_statement_type := 'TRUNCATE'; p_command_name := 'TRUNCATE CLUSTER'; p_command_type := 86;
	elsif v_symbols_1_to_2 = 'TRUNCATE TABLE' then
		p_category := C_DDL; p_statement_type := 'TRUNCATE'; p_command_name := 'TRUNCATE TABLE'; p_command_type := 85;
	elsif v_symbols_1_to_2 = 'UNDROP OBJECT' then --Not a real command.
		p_category := C_DDL; p_statement_type := 'UNDROP'; p_command_name := 'UNDROP OBJECT'; p_command_type := 202;
	elsif v_symbols_1 = 'UPDATE' then
		p_category := C_DML; p_statement_type := 'UPDATE'; p_command_name := 'UPDATE'; p_command_type := 6;
	--These are not real commands (they are part of alter table) and they could be ambiguous with an UPDATE statement
	--if there was a table named "INDEXES" or "JOIN".
	--elsif v_symbols_1_to_2 = 'UPDATE INDEXES' then
	--	p_category := C_DDL; p_statement_type := 'UPDATE'; p_command_name := 'UPDATE INDEXES'; p_command_type := 182;
	--elsif v_symbols_1_to_3 = 'UPDATE JOIN INDEX' then --Not a real command as far as I can tell.
	--	p_category := C_DDL; p_statement_type := 'UPDATE'; p_command_name := 'UPDATE JOIN INDEX'; p_command_type := 191;
	elsif v_symbols_1 = 'MERGE' then --Command name is different than real name.
		p_category := C_DML; p_statement_type := 'MERGE'; p_command_name := 'UPSERT'; p_command_type := 189;
	--Not a real command, this is part of ANALYZE.
	--elsif v_symbols_1_to_2 = 'VALIDATE INDEX' then
	--	p_category := C_DDL; p_statement_type := 'VALIDATE'; p_command_name := 'VALIDATE INDEX'; p_command_type := 23;
	else
		raise_application_error(-20001, 'Cannot classify statement.');
	end if;
end classify;

end;
/
