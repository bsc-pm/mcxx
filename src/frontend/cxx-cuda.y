/*!if GRAMMAR_PROLOGUE */
%token<token_atrib> CUDA_DEVICE "__device__" 
%token<token_atrib> CUDA_GLOBAL "__global__"
%token<token_atrib> CUDA_HOST "__host__"
%token<token_atrib> CUDA_CONSTANT "__constant__"
%token<token_atrib> CUDA_SHARED "__shared__"
%token<token_atrib> CUDA_KERNEL_LEFT "<<<"
%token<token_atrib> CUDA_KERNEL_RIGHT ">>>"

%type<ast> cuda_specifiers
%type<ast> cuda_kernel_call
%type<ast> cuda_kernel_arguments
%type<ast> cuda_kernel_config_list

%type<token_atrib> cuda_kernel_config_left
%type<token_atrib> cuda_kernel_config_right

/*!endif*/
/*!if GRAMMAR_RULES*/
nontype_specifier_without_attribute : cuda_specifiers
{
    $$ = $1;
}
;

postfix_expression : cuda_kernel_call
{
    $$ = $1;
}
;

cuda_specifiers : CUDA_DEVICE
{
    $$ = ASTLeaf(AST_CUDA_DEVICE, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| CUDA_GLOBAL
{
    $$ = ASTLeaf(AST_CUDA_GLOBAL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| CUDA_HOST
{
    $$ = ASTLeaf(AST_CUDA_HOST, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| CUDA_CONSTANT
{
    $$ = ASTLeaf(AST_CUDA_CONSTANT, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| CUDA_SHARED
{
    $$ = ASTLeaf(AST_CUDA_SHARED, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

cuda_kernel_call : postfix_expression cuda_kernel_arguments '(' ')'
{
    $$ = ASTMake3(AST_CUDA_KERNEL_CALL, $1, $2, NULL, ast_get_locus($1), NULL);
}
| postfix_expression cuda_kernel_arguments '(' expression_list ')'
{
    $$ = ASTMake3(AST_CUDA_KERNEL_CALL, $1, $2, $4, ast_get_locus($1), NULL);
}
;

cuda_kernel_arguments : cuda_kernel_config_left cuda_kernel_config_list cuda_kernel_config_right
{
    $$ = $2;
}
;

cuda_kernel_config_list : assignment_expression ',' assignment_expression ',' assignment_expression ',' assignment_expression
{
    $$ = ASTMake4(AST_CUDA_KERNEL_ARGUMENTS, $1, $3, $5, $7, ast_get_locus($1), NULL);
}
| assignment_expression ',' assignment_expression ',' assignment_expression
{
    $$ = ASTMake4(AST_CUDA_KERNEL_ARGUMENTS, $1, $3, $5, NULL, ast_get_locus($1), NULL);
}
| assignment_expression ',' assignment_expression
{
    $$ = ASTMake4(AST_CUDA_KERNEL_ARGUMENTS, $1, $3, NULL, NULL, ast_get_locus($1), NULL);
}
;

cuda_kernel_config_left : CUDA_KERNEL_LEFT
{
    $$ = $1;
}
;

/*!if C99*/
cuda_kernel_config_right : CUDA_KERNEL_RIGHT
{
    $$ = $1;
}
;
/*!endif*/
/*!if CPLUSPLUS*/
// C++2011
cuda_kernel_config_right : AB1 AB1 '>'
{
    $$ = $1;
}
// C++2003
| CUDA_KERNEL_RIGHT
{
    $$ = $1;
}
;
/*!endif*/

/*!endif*/
