create or replace type nclob_table is table of nclob;
/
create or replace type nvarchar2_table is table of nvarchar2(2 char);
/
create or replace type token is object
(
      type     varchar2(4000),
      value    nclob,
      --Although called "SQL" code and errm, these may also apply to PL/SQL.
      --They would not match the real PL/SQL error messages, but the information
      --should still be helpful to parse broken code.
      sqlcode  number,
      sqlerrm  varchar2(4000)
);
/

--Use VARRAY because it is guaranteed to maintain order.
create or replace type token_table is varray(2147483647) of token;
/
--Use TABLE here to avoid an ORA-7445 error.
--TODO: Can I use a varray of a smaller size to avoid the error?
create or replace type token_table_table is table of token_table;
/

prompt Installing tokenizer.plsql
start tokenizer.plsql
prompt Installing statement_classifier.plsql
start statement_classifier.plsql
prompt Installing statement_splitter.plsql
start statement_splitter.plsql
prompt Installing statement_feedback.plsql
start statement_feedback.plsql
prompt Installing statement_terminator.plsql
start statement_terminator.plsql
