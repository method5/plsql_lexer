create or replace package plsql_lexer_test is
--Copyright (C) 2015 Jon Heller.  This program is licensed under the LGPLv3.

/*
== Purpose ==

Store constants used by multiple packages for PLSQL_LEXER unit tests, and
procedures that call multiple tests at once.
*/

C_PASS_MESSAGE varchar2(200) := '
  _____         _____ _____
 |  __ \ /\    / ____/ ____|
 | |__) /  \  | (___| (___
 |  ___/ /\ \  \___ \\___ \
 | |  / ____ \ ____) |___) |
 |_| /_/    \_\_____/_____/';

C_FAIL_MESSAGE varchar2(200) := '
  ______      _____ _
 |  ____/\   |_   _| |
 | |__ /  \    | | | |
 |  __/ /\ \   | | | |
 | | / ____ \ _| |_| |____
 |_|/_/    \_\_____|______|';

procedure run_static_tests;
procedure run_dynamic_tests;
procedure run_all_tests;

end;
/
create or replace package body plsql_lexer_test is

--------------------------------------------------------------------------------
--Run all dynamic tests.
--This should be fairly quick and does about 95% of the testing.
procedure run_static_tests is
begin
	statement_classifier_test.run(p_tests => statement_classifier_test.c_static_tests);
	statement_splitter_test.run(p_tests => statement_splitter_test.c_static_tests);
	statement_terminator_test.run(p_tests => statement_terminator_test.c_static_tests);
	tokenizer_test.run(p_tests => tokenizer_test.c_static_tests);
end;

--------------------------------------------------------------------------------
--Run all dynamic tests.
--This may take a long time and provides only a little extra value.
procedure run_dynamic_tests is
begin
	statement_classifier_test.run(p_tests => statement_classifier_test.c_dynamic_tests);
	statement_splitter_test.run(p_tests => statement_splitter_test.c_dynamic_tests);
	statement_terminator_test.run(p_tests => statement_terminator_test.c_dynamic_tests);
	tokenizer_test.run(p_tests => tokenizer_test.c_dynamic_tests);
end;

--------------------------------------------------------------------------------
--Run all tests for all packages.  This may take a long time.
procedure run_all_tests is
begin
	run_static_tests;
	run_dynamic_tests;
end;

end;
/
