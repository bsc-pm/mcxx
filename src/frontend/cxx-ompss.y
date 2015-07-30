/*!if GRAMMAR_PROLOGUE*/
%token<token_atrib> SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION "<ompss-dependency-expression>"

%type<ast> ompss_dependency_expr
%type<ast> ompss_old_multidependences

/*!endif*/
/*!if GRAMMAR_RULES*/

subparsing : SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION ompss_dependency_expr
{
    $$ = $2;
}
;

ompss_dependency_expr : assignment_expression
{
    $$ = $1;
}
| ompss_old_multidependences
{
    $$ = $1;
}
;

ompss_old_multidependences : '{' multiexpression_body '}'
{
    $$ = $2;
}
/*!endif*/
