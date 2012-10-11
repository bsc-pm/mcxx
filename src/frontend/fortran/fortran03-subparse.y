/*!if GRAMMAR_PROLOGUE*/

%type<ast> subparsing

%token<token_atrib> SUBPARSE_PROGRAM_UNIT "<subparse-program-unit>"
%token<token_atrib> SUBPARSE_EXPRESSION "<subparse-expression>"
%token<token_atrib> SUBPARSE_STATEMENT "<subparse-statement>"

/*!endif*/
/*!if GRAMMAR_RULES*/

program : subparsing
{
    *parsed_tree = $1;
}
;

subparsing : SUBPARSE_STATEMENT EOS block
{
    $$ = $3;
}
| SUBPARSE_EXPRESSION EOS expr EOS
{
    $$ = $3;
}
| SUBPARSE_EXPRESSION EOS common_name EOS
{
    $$ = $3;
}
| SUBPARSE_PROGRAM_UNIT EOS program_unit_seq
{
    $$ = $3;
}
;

/*!endif*/
