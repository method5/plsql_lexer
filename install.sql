--Purpose: Install PLSQL_LEXER.
--How to run:
--	alter session set current_schema=&schema_name;
--	@install

--#1: Stop the script at first error, make the installation less noisy.
whenever sqlerror exit failure
whenever oserror exit failure
set feedback off


--#2: Installation banner
prompt
prompt ============================
prompt = PLSQL_LEXER Installation =
prompt ============================
prompt


--#3: Quit if program is already installed.
prompt Looking for existing installation...
--Look for type names, throw error if any are found.
declare
	v_installed_types varchar2(4000);
begin
	select listagg(type_name, ',') within group (order by type_name)
	into v_installed_types
	from all_types
	where type_name in ('NCLOB_TABLE', 'NVARCHAR2_TABLE',
		'TOKEN', 'TOKEN_TABLE', 'TOKEN_TABLE_TABLE')
		and owner = sys_context('userenv', 'current_schema');

	if v_installed_types is not null then
		raise_application_error(-20000, 'Installation failed, the following '||
			'types already exist.  Either run @uninstall.sql or manually remove '||
			'these types: '||v_installed_types);
	end if;
end;
/


--#4: Install types.
prompt Installing types...
create or replace type nclob_table is table of nclob;
/
create or replace type nvarchar2_table is table of nvarchar2(2 char);
/
create or replace type token is object
(
	type                varchar2(4000), --String to represent token type.  See the constants in TOKENIZER.
	value               nclob,          --The text of the token.
	line_number         number,         --The line number the token starts at - useful for printing warning and error information.
	column_number       number,         --The column number the token starts at - useful for printing warning and error information.
	first_char_position number,         --First character position of token in the whole string - useful for inserting before a token.
	last_char_position  number,         --Last character position of token in the whole string  - useful for inserting after a token.
	sqlcode             number,         --Error code of serious parsing problem.
	sqlerrm             varchar2(4000)  --Error message of serious parsing problem.
);
/
--Use VARRAY because it is guaranteed to maintain order.
create or replace type token_table is varray(2147483647) of token;
/
--Use TABLE here to avoid an ORA-7445 error.
--TODO: Can I use a varray of a smaller size to avoid the error?
create or replace type token_table_table is table of token_table;
/


--#5: Install packages.
prompt Installing packages...

start tokenizer.plsql
start statement_classifier.plsql
start statement_splitter.plsql
start statement_feedback.plsql
start statement_terminator.plsql


--#6: Verify installation and print success message.
prompt Verifying installation...

--Display all invalid objects.
column owner format a30;
column object_name format a30;
column object_type format a13;

select owner, object_name, object_type
from all_objects
where object_name in ('TOKENIZER', 'STATEMENT_CLASSIFIER', 'STATEMENT_SPLITTER', 'STATEMENT_FEEDBACK', 'STATEMENT_TERMINATOR')
	and owner = sys_context('userenv', 'current_schema')
	and status <> 'VALID';

--Raise error if any packages are invalid.
--(Because compilation errors may be "warnings" that won't fail the script.)
declare
	v_count number;
begin
	select count(*)
	into v_count
	from all_objects
	where object_name in ('TOKENIZER', 'STATEMENT_CLASSIFIER', 'STATEMENT_SPLITTER', 'STATEMENT_FEEDBACK', 'STATEMENT_TERMINATOR')
		and owner = sys_context('userenv', 'current_schema')
		and status <> 'VALID';

	if v_count >= 1 then
		raise_application_error(-20000, 'Installation failed, the above objects '||
			'are invalid.');
	end if;
end;
/


prompt
prompt Installation successful.

--#7: Return SQL*Plus to normal environment.
whenever sqlerror continue
whenever oserror continue
set feedback on
