/*!if GRAMMAR_PROLOGUE*/
%token<token_atrib> SUBPARSE_OPENMP_DECLARE_REDUCTION "<omp-declare-reduction>"
%token<token_atrib> SUBPARSE_OPENMP_DEPEND_ITEM "<omp-depend-item>"

%type<ast> omp_declare_reduction
%type<ast> omp_dr_reduction_id
%type<ast> omp_dr_typename_list
%type<ast> omp_dr_typename
%type<ast> omp_dr_combiner
%type<ast> omp_dr_initializer

%type<ast> omp_depend_item

%type<token_atrib> omp_dr_operator
%type<ast> omp_dr_identifier

/*!endif*/
/*!if GRAMMAR_RULES*/

subparsing : SUBPARSE_OPENMP_DECLARE_REDUCTION omp_declare_reduction
{
    $$ = $2;
}
| SUBPARSE_OPENMP_DEPEND_ITEM omp_depend_item
{
    $$ = $2;
}
;

omp_declare_reduction : omp_dr_reduction_id ':' omp_dr_typename_list ':' omp_dr_combiner
{
    $$ = ASTMake4(AST_OMP_DECLARE_REDUCTION, $1, $3, $5, NULL, ASTFileName($1), ASTLine($1), NULL);
}
| omp_dr_reduction_id ':' omp_dr_typename_list ':' omp_dr_combiner ':' omp_dr_initializer
{
    $$ = ASTMake4(AST_OMP_DECLARE_REDUCTION, $1, $3, $5, $7, ASTFileName($1), ASTLine($1), NULL);
}
;

omp_dr_reduction_id : omp_dr_operator
{
    $$ = ASTLeaf(AST_OMP_DR_OPERATOR, $1.token_file, $1.token_line, $1.token_text);
}
| omp_dr_identifier
{
    $$ = $1;
}
;

omp_dr_identifier : IDENTIFIER
{
    $$ = ASTLeaf(AST_OMP_DR_IDENTIFIER, $1.token_file, $1.token_line, $1.token_text);
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
| '&'
| '|'
| '^'
| ANDAND
| OROR
;

/*!if C99*/
omp_dr_typename : type_specifier_seq
{
    $$ = $1;
}
;
/*!endif*/

/*!if CPLUSPLUS*/
omp_dr_typename : type_specifier_seq_0
{
    $$ = $1;
}
| type_specifier_seq_ended_with_identifier
{
    $$ = $1;
}
;
/*!endif*/

omp_dr_combiner : expression
{
    $$ = $1;
}
;

/*!if C99*/
omp_dr_initializer : id_expression initializer %merge<ambiguityHandler>
{
    AST declarator_id = ASTMake1(AST_DECLARATOR_ID_EXPR, $1, ASTFileName($1), ASTLine($1), NULL);
    AST declarator = ASTMake1(AST_DECLARATOR, declarator_id, ASTFileName($1), ASTLine($1), NULL);

    $$ = ASTMake2(AST_INIT_DECLARATOR, declarator, $2, ASTFileName($1), ASTLine($1), NULL);
}
| postfix_expression '(' ')' %merge<ambiguityHandler>
{
	$$ = ASTMake2(AST_FUNCTION_CALL, $1, NULL, ASTFileName($1), ASTLine($1), NULL);
}
| postfix_expression '(' expression_list ')' %merge<ambiguityHandler>
{
	$$ = ASTMake2(AST_FUNCTION_CALL, $1, $3, ASTFileName($1), ASTLine($1), NULL);
}
;
/*!endif*/
/*!if CPLUSPLUS*/

/* Here there is the usual T(x) ambiguity. Easily solvable checking if T is omp_priv or not */
omp_dr_initializer : unqualified_name initializer %merge<ambiguityHandler>
{
    AST declarator_id = ASTMake1(AST_DECLARATOR_ID_EXPR, $1, ASTFileName($1), ASTLine($1), NULL);
    AST declarator = ASTMake1(AST_DECLARATOR, declarator_id, ASTFileName($1), ASTLine($1), NULL);

    $$ = ASTMake2(AST_INIT_DECLARATOR, declarator, $2, ASTFileName($1), ASTLine($1), NULL);
}
| postfix_expression '(' ')' %merge<ambiguityHandler>
{
	$$ = ASTMake2(AST_FUNCTION_CALL, $1, NULL, ASTFileName($1), ASTLine($1), NULL);
}
| postfix_expression '(' expression_list ')' %merge<ambiguityHandler>
{
	$$ = ASTMake2(AST_FUNCTION_CALL, $1, $3, ASTFileName($1), ASTLine($1), NULL);
}
;
/*!endif*/

omp_depend_item : id_expression
{
    $$ = $1;
}
| omp_depend_item '[' expression ':' expression ']'
{
    // Note that this is to be interpreted as a [lower:size] (not [lower:upper]),
    // so we create an AST_ARRAY_SECTION_SIZE here
    $$ = ASTMake4(AST_ARRAY_SECTION_SIZE, $1, $3, $5, NULL, ASTFileName($1), ASTLine($1), NULL);
}
;

/*!endif*/
