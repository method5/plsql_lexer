`PLSQL_LEXER`
============

PL/SQL Lexer is a toolkit for solving real-world language problems, in PL/SQL.

## Procedures

See the individual packages for details on each procedure.

- `tokenizer.tokenize` - Create tokens for SQL and PL/SQL statements.

		function tokenize(p_source nclob) return token_table;

- `tokenizer.print_tokens` - Print an array of tokens, for testing.

		function print_tokens(p_tokens token_table) return nclob;

- `split_statements` - Split a series of statements separated by ";" or "/".

		function split(p_statements in nclob) return nclob_table;
		TODO

- `statement_classifier.classify_statement` - Classify a statement as DDL, PL/SQL, SELECT, ALTER, etc.

		procedure classify(
			p_statement       in nclob,
			p_category       out varchar2,
			p_statement_type out varchar2,
			p_command_name   out varchar2,
			p_command_type   out number,
			p_lex_sqlcode    out number,
			p_lex_sqlerrm    out varchar2
		);

- `statement_feedback.get_feedback_message` - Get a message similar to SQL*Plus feedback messages.

		procedure get_feedback_message(
				p_statement in nclob,
				p_rowcount in number,
				p_success_message out varchar2,
				p_compile_warning_message out varchar2
		);

- `remove_extra_terminators` - Remove extra terminators to run as dynamic SQL.

		TODO


## How to Install

1. Create objects on the desired schema:

        alter session set current_schema=&SCHEMA_NAME;
        
        create or replace type nclob_table is table of nclob;
        
        create or replace type nvarchar2_table is table of nvarchar2(2 char);
        
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
        
        --Use VARRAY because it is guaranteed to maintain order.
        create or replace type token_table is varray(2147483647) of token;

2. Install packages on the desired schema:

        alter session set current_schema=&SCHEMA_NAME;
        @tokenizer.pck
        @statement_splitter.pck
        @statement_classifier.pck
        @statement_feedback.pck

3. Install unit tests (optional):

        @/tests/tokenizer_test.pck
        @/tests/statement_classifier_test.pck
        @/tests/statement_splitter_test.pck


## License
`plsql_lexer` is licensed under the LGPL.