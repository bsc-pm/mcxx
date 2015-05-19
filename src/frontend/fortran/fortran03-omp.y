/*!if GRAMMAR_PROLOGUE*/
%token<token_atrib> SUBPARSE_OPENMP_DECLARE_REDUCTION
%token<token_atrib> SUBPARSE_OPENMP_DEPEND_ITEM

%type<ast> omp_declare_reduction
%type<ast> omp_dr_reduction_id
%type<ast> omp_dr_typename_list
%type<ast> omp_dr_typename
%type<ast> omp_dr_combiner
%type<ast> omp_dr_initializer

%type<token_atrib> omp_dr_operator
%type<ast> omp_dr_identifier

%type<ast> omp_depend_item

/*!endif*/
/*!if GRAMMAR_RULES*/

subparsing : SUBPARSE_OPENMP_DECLARE_REDUCTION omp_declare_reduction EOS
{
    $$ = $2;
}
| SUBPARSE_OPENMP_DEPEND_ITEM omp_depend_item EOS
{
    $$ = $2;
}
;

omp_declare_reduction : omp_dr_reduction_id ':' omp_dr_typename_list ':' omp_dr_combiner
{
    $$ = ASTMake4(AST_OMP_DECLARE_REDUCTION, $1, $3, $5, NULL, ast_get_locus($1), NULL);
}
| omp_dr_reduction_id ':' omp_dr_typename_list ':' omp_dr_combiner ':' omp_dr_initializer
{
    $$ = ASTMake4(AST_OMP_DECLARE_REDUCTION, $1, $3, $5, $7, ast_get_locus($1), NULL);
}
;

omp_dr_reduction_id : omp_dr_operator
{
    $$ = ASTLeaf(AST_OMP_DR_OPERATOR, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| omp_dr_identifier
{
    $$ = $1;
}
;

omp_dr_identifier : name
{
    $$ = $1;
}
;

omp_dr_typename_list : omp_dr_typename
{
    $$ = ASTListLeaf($1);
}
| omp_dr_typename_list ',' omp_dr_typename
{
    $$ = ASTList($1, $3);
}
;

omp_dr_operator : '+'
| '-'
| '*'
| TOKEN_LOGICAL_AND
| TOKEN_LOGICAL_OR
| TOKEN_LOGICAL_EQUIVALENT
| TOKEN_LOGICAL_NOT_EQUIVALENT
| USER_DEFINED_OPERATOR
;

omp_dr_typename : declaration_type_spec
{
    $$ = $1;
}
;

omp_dr_combiner : name '=' expr
{
    $$ = ASTMake2(AST_ASSIGNMENT, $1, $3, ast_get_locus($1), NULL);
}
| function_reference
{
    ast_set_text($1, "call");
    $$ = $1;
}
;

omp_dr_initializer : name '=' expr
{
    $$ = ASTMake2(AST_ASSIGNMENT, $1, $3, ast_get_locus($1), NULL);
}
| function_reference
{
    ast_set_text($1, "call");
    $$ = $1;
}
;

omp_depend_item : data_ref
{
    $$ = $1;
}
;

/*!endif*/
