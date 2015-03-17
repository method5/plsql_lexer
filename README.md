`STATEMENT_CLASSIFIER`
============

Statement classifier is a PL/SQL package to thoroughly classify a SQL or PL/SQL statement.  For example, to determine if a statement is PL/SQL, DDL, or DML.


## Example

    declare
        v_category       varchar2(100);
        v_statement_type varchar2(100);
        v_command_name   varchar2(64);
        v_command_type   number;
        v_lex_sqlcode    number;
        v_lex_sqlerrm    varchar2(4000);
    begin
        statement_classifier.classify(
            '(with test as (select * from dual) select * from test)',
            v_category,
            v_statement_type,
            v_command_name,
            v_command_type,
            v_lex_sqlcode,
            v_lex_sqlerrm
        );
    
        dbms_output.put_line('Category      : '||v_category);
        dbms_output.put_line('Statement Type: '||v_statement_type);
        dbms_output.put_line('Command Name  : '||v_command_name);
        dbms_output.put_line('Command Type  : '||v_command_type);
        dbms_output.put_line('Lex SQLCODE   : '||v_lex_sqlcode);
        dbms_output.put_line('Lex SQLERRM   : '||v_lex_sqlerrm);
    end;
    /

Results:

    Category      : DML
    Statement Type: SELECT
    Command Name  : SELECT
    Command Type  : 3
    Lex SQLCODE   : 
    Lex SQLERRM   : 


## How to Install

1. Create objects on the desired schema:

        alter session set current_schema=SCHEMA_NAME;
        
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
        
        --Use VARRAY because it is gaurenteed to maintain order.
        create or replace type token_table is varray(2147483647) of token;

2. Install packages on the desired schema:

        alter session set current_schema=SCHEMA_NAME;
        @plsql_lexer.pck
        @statement_classifier.pck

3. Install unit tests (optional):

        @/tests/plsql_lexer_test.pck
        @/tests/statement_classifier_test.pck

## License
`statement_classifier` is licensed under the LGPL.
