create or replace package statement_semicolon_remover is
--Copyright (C) 2015 Jon Heller.  This program is licensed under the LGPLv3.

function remove(
	p_abstract_tokens in token_table,
	p_command_name    in varchar2
) return nclob;


/*

** PURPOSE **

Statement Semicolon Remover removes the terminating semicolon from the end of a
statement.  This is helpful when the statement will be executed as dynamic SQL.

Dynamic SQL requires some commands must include a terminating semicolon and
other commands must not include a semicolon.  For example, "create table ..."
must not have a semicolon, but "create procedure ..." must end with a semicolon.

Only the last semicolon is removed.  Just like with SQL*Plus, statements
that ends with two semicolons will not work correctly.

** EXAMPLE **

TODO

** PARAMETES **

- p_abstract_tokens (IN):  The tokens for a statement, probably generated by tokenizer.
- p_command_name (IN)   :  The command name for the statement, as defined by V$SQLCOMMAND.
                           This value will probably come from STATEMENT_CLASSIFIER.

*/

end;
/
create or replace package body statement_semicolon_remover is

--------------------------------------------------------------------------------
--Build the statement excluding the last semicolon, if any.
function build_statement_wo_semicolon(p_abstract_tokens token_table)
return nclob is
	v_statement nclob;
	v_semicolon_index number := -1;
begin
	--Find the index of the last semicolon token.
	--Only count it if it's the last concrete token.
	for i in reverse 1 .. p_abstract_tokens.count loop
		--Record index if it's a semicolon.
		if p_abstract_tokens(i).type = ';' then
			v_semicolon_index := i;
		--Quit the loop if another concrete token is found.
		elsif p_abstract_tokens(i).type not in ('comment', 'whitespace', 'EOF') then
			exit;
		end if;
	end loop;

	--Put together string, excluding the semicolon;
	for i in 1 .. p_abstract_tokens.count loop
		if i <> v_semicolon_index then
			v_statement := v_statement || p_abstract_tokens(i).value;
		end if;
	end loop;

	return v_statement;
end build_statement_wo_semicolon;


--------------------------------------------------------------------------------
--Return the statement without any modification.
function build_statement_as_is(p_abstract_tokens token_table)
return nclob is
	v_statement nclob;
begin
	for i in 1 .. p_abstract_tokens.count loop
		v_statement := v_statement || p_abstract_tokens(i).value;
	end loop;

	return v_statement;
end build_statement_as_is;


--------------------------------------------------------------------------------
--Remove extra semicolons, if any, to prepare for dynamic execution.
function remove(
	p_abstract_tokens in token_table,
	p_command_name    in varchar2
) return nclob is
begin
	--Remove semicolons from these:
	if p_command_name in (
		'ADMINISTER KEY MANAGEMENT','ALTER ASSEMBLY','ALTER AUDIT POLICY','ALTER CLUSTER','ALTER DATABASE',
		'ALTER DATABASE LINK','ALTER DIMENSION','ALTER DISK GROUP','ALTER EDITION','ALTER FLASHBACK ARCHIVE',
		'ALTER FUNCTION','ALTER INDEX','ALTER INDEXTYPE','ALTER JAVA','ALTER LIBRARY','ALTER MATERIALIZED VIEW ',
		'ALTER MATERIALIZED VIEW LOG','ALTER MATERIALIZED ZONEMAP','ALTER OPERATOR','ALTER OUTLINE',
		'ALTER PACKAGE','ALTER PACKAGE BODY','ALTER PLUGGABLE DATABASE','ALTER PROCEDURE','ALTER PROFILE',
		'ALTER RESOURCE COST',/*'ALTER REWRITE EQUIVALENCE',*/'ALTER ROLE','ALTER ROLLBACK SEGMENT',
		'ALTER SEQUENCE','ALTER SESSION',/*'ALTER SUMMARY',*/'ALTER SYNONYM','ALTER SYSTEM','ALTER TABLE',
		'ALTER TABLESPACE','ALTER TRACING','ALTER TRIGGER','ALTER TYPE','ALTER TYPE BODY','ALTER USER',
		'ALTER VIEW','ANALYZE CLUSTER','ANALYZE INDEX','ANALYZE TABLE','ASSOCIATE STATISTICS','AUDIT OBJECT',
		'CALL METHOD',/*'CHANGE PASSWORD',*/'COMMENT','COMMIT',/*'CREATE ASSEMBLY',*/'CREATE AUDIT POLICY',
		/*'CREATE BITMAPFILE',*/'CREATE CLUSTER','CREATE CONTEXT','CREATE CONTROL FILE','CREATE DATABASE',
		'CREATE DATABASE LINK','CREATE DIMENSION','CREATE DIRECTORY','CREATE DISK GROUP','CREATE EDITION',
		'CREATE FLASHBACK ARCHIVE',/*'CREATE FUNCTION',*/'CREATE INDEX','CREATE INDEXTYPE',/*'CREATE JAVA',*/
		/*'CREATE LIBRARY',*/'CREATE MATERIALIZED VIEW ','CREATE MATERIALIZED VIEW LOG','CREATE MATERIALIZED ZONEMAP',
		'CREATE OPERATOR','CREATE OUTLINE',/*'CREATE PACKAGE',*//*'CREATE PACKAGE BODY',*/'CREATE PFILE',
		'CREATE PLUGGABLE DATABASE',/*'CREATE PROCEDURE',*/'CREATE PROFILE','CREATE RESTORE POINT','CREATE ROLE',
		'CREATE ROLLBACK SEGMENT','CREATE SCHEMA','CREATE SCHEMA SYNONYM','CREATE SEQUENCE','CREATE SPFILE',
		/*'CREATE SUMMARY',*/'CREATE SYNONYM','CREATE TABLE','CREATE TABLESPACE',/*'CREATE TRIGGER',*//*'CREATE TYPE',*/
		/*'CREATE TYPE BODY',*/'CREATE USER','CREATE VIEW',/*'DECLARE REWRITE EQUIVALENCE',*/'DELETE',
		'DISASSOCIATE STATISTICS','DROP ASSEMBLY','DROP AUDIT POLICY',/*'DROP BITMAPFILE',*/'DROP CLUSTER',
		'DROP CONTEXT','DROP DATABASE','DROP DATABASE LINK','DROP DIMENSION','DROP DIRECTORY','DROP DISK GROUP',
		'DROP EDITION','DROP FLASHBACK ARCHIVE','DROP FUNCTION','DROP INDEX','DROP INDEXTYPE','DROP JAVA',
		'DROP LIBRARY','DROP MATERIALIZED VIEW ','DROP MATERIALIZED VIEW  LOG','DROP MATERIALIZED ZONEMAP',
		'DROP OPERATOR','DROP OUTLINE','DROP PACKAGE','DROP PACKAGE BODY','DROP PLUGGABLE DATABASE',
		'DROP PROCEDURE','DROP PROFILE','DROP RESTORE POINT',/*'DROP REWRITE EQUIVALENCE',*/'DROP ROLE',
		'DROP ROLLBACK SEGMENT','DROP SCHEMA SYNONYM','DROP SEQUENCE',/*'DROP SUMMARY',*/'DROP SYNONYM',
		'DROP TABLE','DROP TABLESPACE','DROP TRIGGER','DROP TYPE','DROP TYPE BODY','DROP USER','DROP VIEW',
		/*'Do not use 184',*//*'Do not use 185',*//*'Do not use 186',*/'EXPLAIN','FLASHBACK DATABASE',
		'FLASHBACK TABLE','GRANT OBJECT','INSERT','LOCK TABLE',/*'NO-OP',*/'NOAUDIT OBJECT',/*'PL/SQL EXECUTE',*/
		'PURGE DBA RECYCLEBIN','PURGE INDEX','PURGE TABLE','PURGE TABLESPACE','PURGE USER RECYCLEBIN','RENAME',
		'REVOKE OBJECT','ROLLBACK','SAVEPOINT','SELECT','SET CONSTRAINTS','SET ROLE','SET TRANSACTION',
		'TRUNCATE CLUSTER','TRUNCATE TABLE',/*'UNDROP OBJECT',*/'UPDATE',/*'UPDATE INDEXES',*/
		/*'UPDATE JOIN INDEX',*/'UPSERT'/*'VALIDATE INDEX',*/
	) then
		return build_statement_wo_semicolon(p_abstract_tokens);
	--Do nothing for these, they can end with a semicolon:
	elsif p_command_name in (
		'CREATE ASSEMBLY',
		'CREATE FUNCTION',
		'CREATE JAVA',
		'CREATE LIBRARY',
		'CREATE PACKAGE',
		'CREATE PACKAGE BODY',
		'CREATE PROCEDURE',
		'CREATE TRIGGER',
		'CREATE TYPE',
		'CREATE TYPE BODY',
		'PL/SQL EXECUTE',
		'Invalid',
		'Nothing'
	) then
		return build_statement_as_is(p_abstract_tokens);
	else
		raise_application_error(-20000, 'Cannot determine if statement needs a semicolon.'||
			'  The command name "'||p_command_name||'" is not recognized.');
	end if;
end remove;

end;
/
