/*!if GRAMMAR_PROLOGUE*/

%type<ast> subparse_type_list

// Subparsing
%token<token_atrib> SUBPARSE_EXPRESSION "<subparse-expression>"
%token<token_atrib> SUBPARSE_EXPRESSION_LIST "<subparse-expression-list>"
%token<token_atrib> SUBPARSE_STATEMENT "<subparse-statement>"
%token<token_atrib> SUBPARSE_DECLARATION "<subparse-declaration>"
%token<token_atrib> SUBPARSE_MEMBER "<subparse-member>"
%token<token_atrib> SUBPARSE_TYPE "<subparse-type>"
%token<token_atrib> SUBPARSE_TYPE_LIST "<subparse-type-list>"

%token<token_atrib> SUBPARSE_OMP_UDR_DECLARE "<subparse-omp-udr-declare>"
%type<ast> omp_udr_operator_list
%type<ast> omp_udr_operator
%type<ast> omp_udr_builtin_op
%type<ast> omp_udr_type_specifier
%type<ast> omp_udr_declare_arg

/*!endif*/
/*!if GRAMMAR_RULES*/

translation_unit : subparsing
{
	*parsed_tree = $1;
}
;

subparsing : SUBPARSE_EXPRESSION expression
{
	$$ = $2;
}
| SUBPARSE_STATEMENT statement_seq
{
	$$ = $2;
}
| SUBPARSE_STATEMENT
{
	$$ = NULL;
}
| SUBPARSE_MEMBER member_specification_seq
{
	$$ = $2;
}
| SUBPARSE_DECLARATION declaration_sequence
{
	$$ = $2;
}
| SUBPARSE_DECLARATION
{
    $$ = NULL;
}
| SUBPARSE_TYPE type_id
{
    $$ = $2;
}
| SUBPARSE_TYPE_LIST subparse_type_list
{
    $$ = $2;
}
| SUBPARSE_EXPRESSION_LIST expression_list
{
    $$ = $2;
}
| SUBPARSE_OMP_UDR_DECLARE omp_udr_declare_arg
{
    $$ = $2;
}
;

subparse_type_list : type_specifier_seq
{
    $$ = ASTListLeaf($1);
}
| subparse_type_list ',' type_specifier_seq
{
    $$ = ASTList($1, $3);
}
;

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

/*!endif*/
