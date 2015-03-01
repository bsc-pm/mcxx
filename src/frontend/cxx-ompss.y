/*!if GRAMMAR_PROLOGUE*/
%token<token_atrib> SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION "<ompss-dependency-expression>"

%type<ast> ompss_dependency_expr
%type<ast> ompss_multi_dependency
%type<ast> ompss_single_dependency
%type<ast> ompss_iterated_dep_body
%type<ast> ompss_iterator_dep
%type<ast> ompss_iterator_range
%type<ast> ompss_iterator_range_size
%type<ast> ompss_iterator_range_section

/*!endif*/
/*!if GRAMMAR_RULES*/

subparsing : SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION ompss_dependency_expr
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

ompss_single_dependency : assignment_expression
{
    $$ = $1;
}
;

ompss_multi_dependency : '{' ompss_iterated_dep_body '}'
{
    $$ = $2;
}
;

ompss_iterated_dep_body : ompss_single_dependency ',' ompss_iterator_dep
{
    $$ = ASTMake2(AST_OMPSS_MULTI_DEPENDENCY, $1, $3, ast_get_locus($1), NULL);
}
| ompss_iterated_dep_body ',' ompss_iterator_dep
{
    $$ = ASTMake2(AST_OMPSS_MULTI_DEPENDENCY, $1, $3, ast_get_locus($1), NULL);
}
;

ompss_iterator_dep : identifier_token '=' ompss_iterator_range
{
    AST symbol = ASTLeaf(AST_SYMBOL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
    $$ = ASTMake2(AST_OMPSS_ITERATOR, symbol, $3, ast_get_locus(symbol), NULL);
}
;

ompss_iterator_range : ompss_iterator_range_size
{
    $$ = $1;
}
| ompss_iterator_range_section
{
    $$ = $1;
}
;

ompss_iterator_range_section : assignment_expression ':' assignment_expression
{
    $$ = ASTMake3(AST_OMPSS_ITERATOR_RANGE_SECTION, $1, $3, NULL, ast_get_locus($1), NULL);
}
| assignment_expression ':' assignment_expression ':' assignment_expression
{
    $$ = ASTMake3(AST_OMPSS_ITERATOR_RANGE_SECTION, $1, $3, $5, ast_get_locus($1), NULL);
}
;

ompss_iterator_range_size : assignment_expression ';' assignment_expression
{
    $$ = ASTMake3(AST_OMPSS_ITERATOR_RANGE_SIZE, $1, $3, NULL, ast_get_locus($1), NULL);
}
| assignment_expression ';' assignment_expression ':' assignment_expression
{
    $$ = ASTMake3(AST_OMPSS_ITERATOR_RANGE_SIZE, $1, $3, $5, ast_get_locus($1), NULL);
}
;

/*!endif*/
