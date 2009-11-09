/*!if GRAMMAR_PROLOGUE*/

%token<token_atrib> SUBPARSE_OMP_UDR_DECLARE "<subparse-omp-udr-declare>"

%type<ast> omp_udr_operator_list
%type<ast> omp_udr_operator
%type<ast> omp_udr_builtin_op
%type<ast> omp_udr_type_specifier
%type<ast> omp_udr_declare_arg

/*!endif*/
/*!if GRAMMAR_RULES*/

omp_udr_declare_arg : omp_udr_operator_list ':' omp_udr_type_specifier
{
    $$ = ASTMake3(AST_OMP_UDR_DECLARE_ARG, NULL, $1, $3, ASTFileName($1), ASTLine($1), NULL);
}
/*!if CPLUSPLUS*/
| TEMPLATE '<' template_parameter_list '>' omp_udr_operator_list ':' omp_udr_type_specifier
{
    $$ = ASTMake3(AST_OMP_UDR_DECLARE_ARG, $3, $5, $7, $1.token_file, $1.token_line, NULL);
}
/*!endif*/
;

/*!if C99*/
omp_udr_type_specifier: type_id
{
    $$ = ASTListLeaf($1);
}
;
/*!endif*/
/*!if CPLUSPLUS*/
omp_udr_type_specifier: type_id
{
    $$ = ASTListLeaf($1);
}
| omp_udr_type_specifier ',' type_id
{
    $$ = ASTList($1, $3);
}
;
/*!endif*/


omp_udr_operator_list : omp_udr_operator
{
    $$ = ASTListLeaf($1);
}
| omp_udr_operator_list ',' omp_udr_operator
{
    $$ = ASTList($1, $3);
}
;

omp_udr_operator : id_expression
{
    $$ = $1;
}
| omp_udr_builtin_op
{
    $$ = $1;
}
| '.' unqualified_id
{
    $$ = ASTMake1(AST_OMP_UDR_MEMBER_OP, $2, $1.token_file, $1.token_line, $1.token_text);
}
;

omp_udr_builtin_op : '+'
{
    $$ = ASTLeaf(AST_OMP_UDR_BUILTIN_OP, $1.token_file, $1.token_line, $1.token_text);
}
| '-'
{
    $$ = ASTLeaf(AST_OMP_UDR_BUILTIN_OP, $1.token_file, $1.token_line, $1.token_text);
}
| '*'
{
    $$ = ASTLeaf(AST_OMP_UDR_BUILTIN_OP, $1.token_file, $1.token_line, $1.token_text);
}
| '/'
{
    $$ = ASTLeaf(AST_OMP_UDR_BUILTIN_OP, $1.token_file, $1.token_line, $1.token_text);
}
| '&'
{
    $$ = ASTLeaf(AST_OMP_UDR_BUILTIN_OP, $1.token_file, $1.token_line, $1.token_text);
}
| '|'
{
    $$ = ASTLeaf(AST_OMP_UDR_BUILTIN_OP, $1.token_file, $1.token_line, $1.token_text);
}
| '^'
{
    $$ = ASTLeaf(AST_OMP_UDR_BUILTIN_OP, $1.token_file, $1.token_line, $1.token_text);
}
| ANDAND
{
    $$ = ASTLeaf(AST_OMP_UDR_BUILTIN_OP, $1.token_file, $1.token_line, $1.token_text);
}
| OROR
{
    $$ = ASTLeaf(AST_OMP_UDR_BUILTIN_OP, $1.token_file, $1.token_line, $1.token_text);
}
;

subparsing: SUBPARSE_OMP_UDR_DECLARE omp_udr_declare_arg
{
    $$ = $2;
}

/*!endif*/
