/*!if GRAMMAR_PROLOGUE*/

%type<ast> shape_seq
%type<ast> shape
%type<ast> noshape_cast_expression
%type<ast> mercurium_extended_type_specifiers

%type<ast> multiexpression
%type<ast> multiexpression_body
%type<ast> multiexpression_iterator
%type<ast> multiexpression_range
%type<ast> multiexpression_range_size
%type<ast> multiexpression_range_section
%type<ast> multiexpression_range_discrete

%token<token_atrib> MCC_BYTE "<byte-type-spec>"
%token<token_atrib> MCC_BOOL "<bool-type-spec>"
%token<token_atrib> MCC_MASK "<mask-type-spec>"
%token<token_atrib> MCC_ARRAY_SUBSCRIPT_CHECK "@array-subscript-check@"
%token<token_atrib> MCC_CONST_VALUE_CHECK "@const-value-check@"

/*!endif*/
/*!if GRAMMAR_RULES*/

primary_expression : MCC_ARRAY_SUBSCRIPT_CHECK '(' assignment_expression ',' constant_expression ')'
{
    $$ = ASTMake2(AST_MCC_ARRAY_SUBSCRIPT_CHECK, $3, $5, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);
}
| MCC_CONST_VALUE_CHECK '(' assignment_expression ')'
{
    $$ = ASTMake1(AST_MCC_CONSTANT_VALUE_CHECK, $3, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);
}
;

postfix_expression : postfix_expression '[' expression_opt ':' expression_opt ']'
{
    $$ = ASTMake4(AST_ARRAY_SECTION, $1, $3, $5, NULL, ast_get_locus($1), NULL);
}
| postfix_expression '[' expression_opt ':' expression_opt ':' expression ']'
{
    $$ = ASTMake4(AST_ARRAY_SECTION, $1, $3, $5, $7, ast_get_locus($1), NULL);
}
| postfix_expression '[' expression ';' expression ']'
{
    $$ = ASTMake4(AST_ARRAY_SECTION_SIZE, $1, $3, $5, NULL, ast_get_locus($1), NULL);
}
| postfix_expression '[' expression ';' expression ':' expression ']'
{
    $$ = ASTMake4(AST_ARRAY_SECTION_SIZE, $1, $3, $5, $7, ast_get_locus($1), NULL);
}
;

noshape_cast_expression : unary_expression %merge<ambiguityHandler>
{
	$$ = $1;
}
| '(' type_id ')' cast_expression %merge<ambiguityHandler>
{
	$$ = ASTMake2(AST_CAST, $2, $4, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);
}
;

cast_expression : shape_seq noshape_cast_expression %merge<ambiguityHandler>
{
    $$ = ASTMake2(AST_SHAPING_EXPRESSION, $1, $2, ast_get_locus($1), NULL);
}
;

shape_seq : shape_seq shape %dprec 2
{
    $$ = ASTList($1, $2);
}
| shape %dprec 1
{
    $$ = ASTListLeaf($1);
}
;

shape: '[' expression ']'
{
    $$ = $2;
}
;

iteration_statement : FOR '[' symbol_literal_ref ']' '(' for_init_statement condition_opt ';' expression_opt ')' statement
{
    AST loop_control = ASTMake3(AST_LOOP_CONTROL, $6, $7, $9, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);
	$$ = ASTMake4(AST_FOR_STATEMENT, loop_control, $11, NULL, $3, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);

}
;

/*!if C99*/
nontype_specifier_without_attribute : mercurium_extended_type_specifiers
{
    $$ = $1;
}
;
/*!endif*/

/*!if CPLUSPLUS*/
type_specifier_SUSL : mercurium_extended_type_specifiers
{
    $$ = $1;
}
;
/*!endif*/

mercurium_extended_type_specifiers : MCC_BOOL
{
	$$ = ASTLeaf(AST_MCC_BOOL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| MCC_MASK
{
	$$ = ASTLeaf(AST_MCC_MASK, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

primary_expression : multiexpression;

multiexpression : '{' '/' multiexpression_body '/' '}'
{
    $$ = $3;
}
;

multiexpression_body : assignment_expression ',' multiexpression_iterator
{
    $$ = ASTMake2(AST_MULTIEXPRESSION, $1, $3, ast_get_locus($1), NULL);
}
| multiexpression_body ',' multiexpression_iterator
{
    $$ = ASTMake2(AST_MULTIEXPRESSION, $1, $3, ast_get_locus($1), NULL);
}
;

multiexpression_iterator : identifier_token '=' multiexpression_range
{
    AST symbol = ASTLeaf(AST_SYMBOL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
    $$ = ASTMake2(AST_MULTIEXPRESSION_ITERATOR, symbol, $3, ast_get_locus(symbol), NULL);
}
;

multiexpression_range : multiexpression_range_size
{
    $$ = $1;
}
| multiexpression_range_section
{
    $$ = $1;
}
| multiexpression_range_discrete
{
    $$ = $1;
}
;

multiexpression_range_section : assignment_expression ':' assignment_expression
{
    $$ = ASTMake3(AST_MULTIEXPRESSION_RANGE_SECTION, $1, $3, NULL, ast_get_locus($1), NULL);
}
| assignment_expression ':' assignment_expression ':' assignment_expression
{
    $$ = ASTMake3(AST_MULTIEXPRESSION_RANGE_SECTION, $1, $3, $5, ast_get_locus($1), NULL);
}
;

multiexpression_range_size : assignment_expression ';' assignment_expression
{
    $$ = ASTMake3(AST_MULTIEXPRESSION_RANGE_SIZE, $1, $3, NULL, ast_get_locus($1), NULL);
}
| assignment_expression ';' assignment_expression ':' assignment_expression
{
    $$ = ASTMake3(AST_MULTIEXPRESSION_RANGE_SIZE, $1, $3, $5, ast_get_locus($1), NULL);
}
;

multiexpression_range_discrete : '{' expression_list '}'
{
    $$ = ASTMake1(AST_MULTIEXPRESSION_RANGE_DISCRETE, $2, ast_get_locus($2), NULL);
}

/*!endif*/
