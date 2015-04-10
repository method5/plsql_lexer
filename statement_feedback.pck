create or replace package statement_feedback is
--Copyright (C) 2015 Jon Heller.  This program is licensed under the LGPLv3.

procedure get_feedback_message(
		p_statement in nclob,
		p_rowcount in number,
		p_success_message out varchar2,
		p_compile_warning_message out varchar2
);


/*
## Purpose ##

Generate a feedback message for a successful SQL or PL/SQL statement, similar to SQL*Plus.

This can help when processing dynamic SQL and PL/SQL statements.  Here are some examples:
- Table created
- Table altered.
- Table dropped.
- PL/SQL procedure successfully completed.
- 5 rows inserted.
- Warning: Package Body altered with compilation errors.
- no rows selected


## Example ##

--Test Statement_Feedback.
declare
	v_statement varchar2(32767);
	v_success_message varchar2(100);
	v_compile_warning_message varchar2(100);
	v_has_compile_warning boolean := false;

	v_success_with_compilation_err exception;
	pragma exception_init(v_success_with_compilation_err, -24344);

begin
	--Test statement.
	v_statement := 'create table some_test_table1(a number)';

	--Execute the statement and catch compile warning errors.
	begin
		execute immediate v_statement;
	exception when v_success_with_compilation_err then
		v_has_compile_warning := true;
	end;

	--Get the feedback message.
	statement_feedback.get_feedback_message(
		p_statement => v_statement,
		p_rowcount => sql%rowcount,
		p_success_message => v_success_message,
		p_compile_warning_message => v_compile_warning_message
	);

	--Display the success message or warning message
	if v_has_compile_warning then
		dbms_output.put_line(v_compile_warning_message);
	else
		dbms_output.put_line(v_success_message);
	end if;
end;


## Parameters ##

p_statement - The SQL or PL/SQL statement that was executed successfully.
              Most of the messages are obvious.  Only the SELECT message is
              unusual - this package will not display the results, only a
              message like "no rows selected".
p_rowcount - The number of rows modified by the statement.
             If it does not apply, pass in NULL.
p_success_message - The message SQL*Plus would display if the statement was successful.
p_compile_warning_message - The message SQL*Plus would display if a PL/SQL object compiled with errors.
                            Catch "ORA-24344: success with compilation error" to detect this situation.
*/

end;
/
create or replace package body statement_feedback is

--------------------------------------------------------------------------------
procedure get_feedback_message(
		p_statement in nclob,
		p_rowcount in number,
		p_success_message out varchar2,
		p_compile_warning_message out varchar2
) is
	v_category       varchar2(100);
	v_statement_type varchar2(100);
	v_command_name   varchar2(64);
	v_command_type   number;
	v_lex_sqlcode    number;
	v_lex_sqlerrm    varchar2(4000);
begin
	--Classify the statement.
	statement_classifier.classify(p_statement,
		v_category,v_statement_type,v_command_name,v_command_type,v_lex_sqlcode,v_lex_sqlerrm
	);

	--If classification failed then return NULLs.
	if v_category is null then
		null;
	--If classification succeeded, set the outputs.
	else
		--These are one-offs and exceptions.
		--Note that some of these seem to have extra spaces because the command names
		--do not always perfectly line up with the real syntax.

		if v_command_name = 'ADMINISTER KEY MANAGEMENT' then
			p_success_message := 'keystore altered.';
		elsif v_command_name = 'ALTER DISK GROUP' then
			p_success_message := 'Diskgroup altered.';
		elsif v_command_name = 'ALTER MATERIALIZED VIEW ' then
			p_success_message := 'Materialized view altered.';
		elsif v_command_name = 'ANALYZE CLUSTER' then
			p_success_message := 'Cluster analyzed.';
		elsif v_command_name = 'ANALYZE INDEX' then
			p_success_message := 'Index analyzed.';
		elsif v_command_name = 'ANALYZE TABLE' then
			p_success_message := 'Table analyzed.';
		elsif v_command_name = 'ASSOCIATE STATISTICS' then
			p_success_message := 'Statistics associated.';
		elsif v_command_name = 'AUDIT OBJECT' then
			p_success_message := 'Audit succeeded.';
		elsif v_command_name = 'CALL' then
			p_success_message := 'Call completed.';
		elsif v_command_name = 'COMMENT' then
			p_success_message := 'Comment created.';
		elsif v_command_name = 'COMMIT' then
			p_success_message := 'Commit complete.';
		elsif v_command_name = 'CREATE DISK GROUP' then
			p_success_message := 'Diskgroup created.';
		elsif v_command_name = 'CREATE MATERIALIZED VIEW ' then
			p_success_message := 'Materialized view created.';
		elsif v_command_name = 'DELETE' then
			if p_rowcount is null then
				p_success_message := 'ERROR: Unknown number of rows deleted.';
			elsif p_rowcount = 1 then
				p_success_message := '1 row deleted.';
			else
				p_success_message := p_rowcount||' rows deleted.';
			end if;
		elsif v_command_name = 'DISASSOCIATE STATISTICS' then
			p_success_message := 'Statistics disassociated.';
		elsif v_command_name = 'DROP DISK GROUP' then
			p_success_message := 'Diskgroup dropped.';
		elsif v_command_name = 'DROP MATERIALIZED VIEW  LOG' then
			p_success_message := 'Materialized view log dropped.';
		elsif v_command_name = 'DROP MATERIALIZED VIEW ' then
			p_success_message := 'Drop materialized view.';
		elsif v_command_name = 'EXPLAIN' then
			p_success_message := 'Explained.';
		elsif v_command_name = 'FLASHBACK DATABASE' then
			p_success_message := 'Flashback complete.';
		elsif v_command_name = 'FLASHBACK TABLE' then
			p_success_message := 'Flashback complete.';
		elsif v_command_name = 'GRANT OBJECT' then
			p_success_message := 'Grant succeeded.';
		elsif v_command_name = 'INSERT' then
			if p_rowcount is null then
				p_success_message := 'ERROR: Unknown number of rows created.';
			elsif p_rowcount = 1 then
				p_success_message := '1 row created.';
			else
				p_success_message := p_rowcount||' rows created.';
			end if;
		elsif v_command_name = 'LOCK TABLE' then
			p_success_message := 'Table(s) Locked.';
		elsif v_command_name = 'NOAUDIT OBJECT' then
			p_success_message := 'Noaudit succeeded.';
		elsif v_command_name = 'PL/SQL EXECUTE' then
			p_success_message := 'PL/SQL procedure successfully completed.';
		elsif v_command_name = 'PURGE DBA_RECYCLEBIN' then
			p_success_message := 'DBA Recyclebin purged.';
		elsif v_command_name = 'PURGE INDEX' then
			p_success_message := 'Index purged.';
		elsif v_command_name = 'PURGE TABLE' then
			p_success_message := 'Table purged.';
		elsif v_command_name = 'PURGE TABLESPACE' then
			p_success_message := 'Tablespace purged.';
		elsif v_command_name = 'PURGE USER RECYCLEBIN' then
			p_success_message := 'Recyclebin purged.';
		elsif v_command_name = 'RENAME' then
			p_success_message := 'Table renamed.';
		elsif v_command_name = 'REVOKE OBJECT' then
			p_success_message := 'Revoke succeeded.';
		elsif v_command_name = 'ROLLBACK' then
			p_success_message := 'Rollback complete.';
		elsif v_command_name = 'SAVEPOINT' then
			p_success_message := 'Savepoint created.';
		elsif v_command_name = 'SELECT' then
			if p_rowcount is null then
				p_success_message := 'ERROR: Unknown number of rows selected.';
			elsif p_rowcount = 0 then
				p_success_message := 'no rows selected';
			elsif p_rowcount = 1 then
				p_success_message := '1 row selected.';
			else
				p_success_message := p_rowcount||' rows selected.';
			end if;
		elsif v_command_name = 'SET CONSTRAINTS' then
			p_success_message := 'Constraint set.';
		elsif v_command_name = 'SET ROLE' then
			p_success_message := 'Role set.';
		elsif v_command_name = 'SET TRANSACTION' then
			p_success_message := 'Transaction set.';
		elsif v_command_name = 'TRUNCATE CLUSTER' then
			p_success_message := 'Cluster truncated.';
		elsif v_command_name = 'TRUNCATE TABLE' then
			p_success_message := 'Table truncated.';
		elsif v_command_name = 'UPDATE' then
			if p_rowcount is null then
				p_success_message := 'ERROR: Unknown number of rows updated.';
			elsif p_rowcount = 1 then
				p_success_message := '1 row updated.';
			else
				p_success_message := p_rowcount||' rows updated.';
			end if;
		elsif v_command_name = 'UPSERT' then
			if p_rowcount is null then
				p_success_message := 'ERROR: Unknown number of rows merged.';
			elsif p_rowcount = 1 then
				p_success_message := '1 row merged.';
			else
				p_success_message := p_rowcount||' rows merged.';
			end if;

		--Standard "ALTER", "CREATE", and "DROP".
		--Remove first word, change to lower case, initialize first letter, add verb.
		elsif v_command_name like 'ALTER %' then
			p_success_message := lower(replace(v_command_name, 'ALTER '))||' altered.';
			p_success_message := upper(substr(p_success_message, 1, 1))||substr(p_success_message, 2);
		elsif v_command_name like 'CREATE %' then
			p_success_message := lower(replace(v_command_name, 'CREATE '))||' created.';
			p_success_message := upper(substr(p_success_message, 1, 1))||substr(p_success_message, 2);
		elsif v_command_name like 'DROP %' then
			p_success_message := lower(replace(v_command_name, 'DROP '))||' dropped.';
			p_success_message := upper(substr(p_success_message, 1, 1))||substr(p_success_message, 2);


		--Print error message if statement type could not be determined.
		else
			p_success_message := 'Cannot determine statement type.';
		end if;


		--Get compile warning message for PL/SQL objects
		if v_command_name like 'ALTER%'
			and
			(
				v_command_name like '%ASSEMBLY' or
				v_command_name like '%FUNCTION' or
				v_command_name like '%JAVA' or
				v_command_name like '%LIBRARY' or
				v_command_name like '%PACKAGE' or
				v_command_name like '%PACKAGE BODY' or
				v_command_name like '%PROCEDURE' or
				v_command_name like '%TRIGGER' or
				v_command_name like '%TYPE' or
				v_command_name like '%TYPE BODY'
			) then
				p_compile_warning_message := 'Warning: '||initcap(replace(v_command_name, 'ALTER '))
					||' altered with compilation errors.';
		elsif v_command_name like 'CREATE%'
			and
			(
				v_command_name like '%ASSEMBLY' or
				v_command_name like '%FUNCTION' or
				v_command_name like '%JAVA' or
				v_command_name like '%LIBRARY' or
				v_command_name like '%PACKAGE' or
				v_command_name like '%PACKAGE BODY' or
				v_command_name like '%PROCEDURE' or
				v_command_name like '%TRIGGER' or
				v_command_name like '%TYPE' or
				v_command_name like '%TYPE BODY'
			) then
				p_compile_warning_message := 'Warning: '||initcap(replace(v_command_name, 'ALTER '))
					||' created with compilation errors.';
		end if;
	end if;

end get_feedback_message;

end;
/
