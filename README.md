`PLSQL_LEXER`
============

PL/SQL Lexer is a toolkit for solving real-world language problems, in PL/SQL.

## Procedures

See the individual packages for details on each procedure.

- `tokenizer.tokenize` - Create tokens for SQL and PL/SQL statements.

		function tokenize(p_source nclob) return token_table;

- `tokenizer.concatenate` - Create NCLOB from tokens.

		function concatenate(p_tokens in token_table) return nclob;

- `statement_splitter.split_by_semicolon` - Split statements terminated by semicolons.

		function split_by_semicolon(
			p_tokens in token_table
		) return token_table_table;
		
- `statement_splitter.split_by_sqlplus_delimiter` - Split statements like SQL*Plus - look for a delimeter on line with only whitespace, even if it's inside a string or comment.

		function split_by_sqlplus_delimiter(
			p_statements        in nclob,
			p_sqlplus_delimiter in nclob
		) return nclob_table;
		
- `statement_splitter.split_by_semi_and_sqlplus_del` - Split statements like SQL*Plus and also split by a semicolon.

		function split_by_semi_and_sqlplus_del(
			p_statements        in nclob,
			p_sqlplus_delimiter in nclob
		) return token_table_table;

- `statement_classifier.classify` - Classify a statement as DDL, PL/SQL, SELECT, ALTER, etc.

		procedure classify(
			p_tokens          in token_table,
			p_category       out varchar2,
			p_statement_type out varchar2,
			p_command_name   out varchar2,
			p_command_type   out number,
			p_lex_sqlcode    out number,
			p_lex_sqlerrm    out varchar2,
			p_start_index     in number default 1
		);

- `statement_feedback.get_feedback_message` - Get a message similar to SQL*Plus feedback messages.

		procedure get_feedback_message(
			p_tokens                   in token_table,
			p_rowcount                 in number,
			p_success_message         out varchar2,
			p_compile_warning_message out varchar2
		);

- `statement_semicolon_remover.remove` - Remove extra semicolons to prepare statement to run as dyanmic SQL.

		function remove(
			p_tokens       in token_table,
			p_command_name in varchar2
		) return nclob;


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
        --Use TABLE here to avoid an ORA-7445 error.
        --TODO: Can I use a varray of a smaller size to avoid the error?
        create or replace type token_table_table is table of token_table;

2. Install packages on the desired schema:

        alter session set current_schema=&SCHEMA_NAME;
        @tokenizer.pck
        @statement_classifier.pck
        @statement_splitter.pck
        @statement_feedback.pck
        @statement_semicolon_remover.pck

3. Install unit tests (optional):

        @/tests/tokenizer_test.pck
        @/tests/statement_classifier_test.pck
        @/tests/statement_splitter_test.pck


## License
`plsql_lexer` is licensed under the LGPL.