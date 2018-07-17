/*!if GRAMMAR_PROLOGUE*/

%type<ast> multiexpression
%type<ast> multiexpression_implied_do
%type<ast> multiexpression_iterator
%type<ast> multiexpression_iterator_list
%type<ast> multiexpression_expression_list

/*!endif*/
/*!if GRAMMAR_RULES*/

primary : multiexpression
;

multiexpression : '{' '/' multiexpression_implied_do '/' '}'
{
    $$ = $3;
}
;

multiexpression_implied_do : expr ',' multiexpression_iterator_list
{
    $$ = ASTMake2(AST_MULTIEXPRESSION, $1, $3, ast_get_locus($1), NULL);
}
;

multiexpression_iterator_list : multiexpression_iterator
{
	$$ = ASTListLeaf($1);
}
| multiexpression_iterator_list ',' multiexpression_iterator
{
	$$ = ASTList($1, $3);
}
;

multiexpression_iterator : ac_do_variable '=' int_expr ',' int_expr comma_int_expr_opt
{
    AST symbol = $1;
    AST range = ASTMake3(AST_MULTIEXPRESSION_RANGE_SECTION, $3, $5, $6, ast_get_locus($3), NULL);
    $$ = ASTMake2(AST_MULTIEXPRESSION_ITERATOR, symbol, range, ast_get_locus(symbol), NULL);
}
| ac_do_variable '=' '{' multiexpression_expression_list '}'
{
    AST symbol = $1;
    AST range = ASTMake1(AST_MULTIEXPRESSION_RANGE_DISCRETE, $4, ast_get_locus($4), NULL);
    $$ = ASTMake2(AST_MULTIEXPRESSION_ITERATOR, symbol, range, ast_get_locus(symbol), NULL);
}
;

multiexpression_expression_list : expr
{
    $$ = ASTListLeaf($1);
}
| multiexpression_expression_list ',' expr
{
    $$ = ASTList($1, $3);
}
;

/*!endif*/

