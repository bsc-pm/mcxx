/*!if GRAMMAR_PROLOGUE*/
%token<token_atrib> SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION "<ompss-dependency-expression>"

%type<ast> ompss_dependency_expr
%type<ast> ompss_multi_dependency
%type<ast> ompss_single_dependency
%type<ast> ompss_implied_do_control
%type<ast> ompss_implied_do

/*!endif*/
/*!if GRAMMAR_RULES*/

subparsing : SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION ompss_dependency_expr EOS
{
    $$ = $2;
}
;

ompss_dependency_expr : ompss_single_dependency
{
    $$ = $1;
}
| ompss_multi_dependency
{
    $$ = $1;
}
;

ompss_single_dependency : expr
{
    $$ = $1;
}
;

ompss_multi_dependency : '[' ompss_implied_do ']'
{
    $$ = $2;
}
| TOKEN_LPARENT_SLASH ompss_implied_do TOKEN_SLASH_RPARENT
{
    $$ = $2;
}

ompss_implied_do : ompss_single_dependency ',' ompss_implied_do_control
{
    $$ = ASTMake2(AST_OMPSS_MULTI_DEPENDENCY, $1, $3, ast_get_locus($1), NULL);
}
| ompss_implied_do ',' ompss_implied_do_control
{
    $$ = ASTMake2(AST_OMPSS_MULTI_DEPENDENCY, $1, $3, ast_get_locus($1), NULL);
}
;

ompss_implied_do_control : ac_do_variable '=' int_expr ',' int_expr comma_int_expr_opt
{
    AST symbol = $1;
    AST range = ASTMake3(AST_OMPSS_ITERATOR_RANGE_SECTION, $3, $5, $6, ast_get_locus($3), NULL);

    $$ = ASTMake2(AST_OMPSS_ITERATOR, symbol, range, ast_get_locus(symbol), NULL);
}
;

/*!endif*/
