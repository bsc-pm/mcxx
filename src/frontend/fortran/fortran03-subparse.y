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

subparsing : SUBPARSE_STATEMENT block
{
    $$ = $2;
}
| SUBPARSE_EXPRESSION expr
{
    $$ = $2;
}
| SUBPARSE_PROGRAM_UNIT program_unit
{
    $$ = $2;
}
;

/*!endif*/
