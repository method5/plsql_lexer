prompt Uninstalling
drop type nclob_table;
drop type nvarchar2_table;
drop type token_table_table;
drop type token_table;
drop type token;

drop package tokenizer;
drop package statement_classifier;
drop package statement_splitter;
drop package statement_feedback;
drop package statement_terminator;

prompt Attempting to remove unit tests, if pressent
drop package tokenizer_test;
drop package statement_classifier_test;
drop package statement_splitter_test;
drop package statement_terminator_test;
drop package plsql_lexer_test;
prompt Done
