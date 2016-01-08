prompt Installing unit tests
prompt Installing tests/tokenizer_test.plsql
start tests/tokenizer_test.plsql
prompt Installing tests/statement_classifier_test.plsql
start tests/statement_classifier_test.plsql
prompt Installing tests/statement_splitter_test.plsql
start tests/statement_splitter_test.plsql
prompt Installing tests/statement_terminator_test.plsql
start tests/statement_terminator_test.plsql
prompt Installing tests/plsql_lexer_test.plsql
start tests/plsql_lexer_test.plsql

prompt Running unit tests, this may take a minute.
prompt Do not trust any packages with errors.

set serveroutput on
set linesize 1000
begin
	plsql_lexer_test.run_static_tests;
end;
/


prompt Done installing and running unit tests.
