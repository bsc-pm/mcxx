/*!if GRAMMAR_PROLOGUE*/
%token<token_atrib> SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION "<ompss-dependency-expression>"

%type<ast> ompss_dependency_expr
%type<ast> ompss_multi_dependency

/*!endif*/
/*!if GRAMMAR_RULES*/

subparsing : SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION ompss_dependency_expr EOS
{
    $$ = $2;
}
;

ompss_dependency_expr : expr
{
    $$ = $1;
}
| ompss_multi_dependency
{
    $$ = $1;
}
;

ompss_multi_dependency : '[' multiexpression_implied_do ']'
{
    $$ = $2;
}
| TOKEN_LPARENT_SLASH multiexpression_implied_do TOKEN_SLASH_RPARENT
{
    $$ = $2;
}
;


/*!endif*/
