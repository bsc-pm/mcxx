/*!if GRAMMAR_PROLOGUE*/

%token<token_atrib> SUBPARSE_OMP_UDR_DECLARE "<subparse-omp-udr-declare>"
%token<token_atrib> SUBPARSE_OMP_UDR_DECLARE_2 "<subparse-omp-udr-declare-2>"

%type<ast> omp_udr_operator_list
%type<ast> omp_udr_operator
%type<ast> omp_udr_operator_2
%type<ast> omp_udr_builtin_op
%type<ast> omp_udr_type_specifier
%type<ast> omp_udr_type_specifier_2
%type<ast> omp_udr_declare_arg
%type<ast> omp_udr_declare_arg_2
%type<ast> omp_udr_expression

%token<token_atrib> SUBPARSE_OMP_UDR_IDENTITY "<subparse-omp-udr-identity>"
%token<token_atrib> OMP_UDR_CONSTRUCTOR "constructor"

%type<ast> omp_udr_identity
/*!if CPLUSPLUS*/
%type<ast> omp_udr_constructor_arguments
/*!endif*/

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


omp_udr_declare_arg_2 : omp_udr_operator_2 ':' omp_udr_type_specifier_2 ':' omp_udr_expression
{
    $$ = ASTMake3(AST_OMP_UDR_DECLARE_ARG_2, $1, $3, $5, ASTFileName($1), ASTLine($1), NULL);
}
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

omp_udr_type_specifier_2: type_id
{
    $$ = ASTListLeaf($1);
}
| omp_udr_type_specifier_2 ',' type_id
{
    $$ = ASTList($1, $3);
}


omp_udr_operator_2:  IDENTIFIER
{
    $$ = ASTLeaf(AST_SYMBOL, $1.token_file, $1.token_line, $1.token_text);
}
| omp_udr_builtin_op
{
	$$ = $1;
	struct { const char *op; const char *name; } map[] =
    { 
        { "+", "_plus_"},
        { "-", "_minus_"},
        { "*", "_mult_"},
        { "/", "_div_"},
        { "&", "_and_"},
        { "|", "_or_"},
        { "^", "_exp_"},
        { "&&", "_andand_"},
        { "||", "_oror_"},
        { NULL, NULL }
    };

	int i; 
	char found = 0;
	for (i = 0; map[i].op != NULL && !found; i++)
	{
		if ((found = (strcmp(ast_get_text($$), map[i].op) == 0)))
        {
            ast_set_text($$, map[i].name);
		    break;
        }
	}
	if (!found)
    {
		internal_error("Unhandled operator '%s'", ast_get_text($$));
    }
}
;

omp_udr_expression: expression
{
    $$ = $1;
}
;


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
/*!if CPLUSPLUS*/
| omp_udr_builtin_op
{
    $$ = $1;
}
| '.' unqualified_id
{
    $$ = ASTMake1(AST_OMP_UDR_MEMBER_OP, $2, $1.token_file, $1.token_line, $1.token_text);
}
/*!endif*/
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
;

subparsing: SUBPARSE_OMP_UDR_DECLARE_2 omp_udr_declare_arg_2
{
    $$ = $2;
}
;

subparsing: SUBPARSE_OMP_UDR_IDENTITY omp_udr_identity
{
    $$ = $2;
}
;

omp_udr_identity: initializer_clause
{
    $$ = $1;
}
/*!if CPLUSPLUS*/
| OMP_UDR_CONSTRUCTOR omp_udr_constructor_arguments
{
    $$ = ASTMake1(AST_OMP_UDR_CONSTRUCTOR, $2, $1.token_file, $1.token_line, $1.token_text);
}
| OMP_UDR_CONSTRUCTOR 
{
    $$ = ASTMake1(AST_OMP_UDR_CONSTRUCTOR,
            ASTMake1(AST_OMP_UDR_CONSTRUCTOR_ARGUMENTS, NULL, $1.token_file, $1.token_line, NULL),
            $1.token_file, $1.token_line, $1.token_text);
}
/*!endif*/
;

/*!if CPLUSPLUS*/
omp_udr_constructor_arguments : '(' ')'
{
    $$ = ASTMake1(AST_OMP_UDR_CONSTRUCTOR_ARGUMENTS, NULL, $1.token_file, $1.token_line, NULL);
}
| '(' expression_list ')'
{
    $$ = ASTMake1(AST_OMP_UDR_CONSTRUCTOR_ARGUMENTS, $2, $1.token_file, $1.token_line, NULL);
}
;
/*!endif*/

/*!endif*/
