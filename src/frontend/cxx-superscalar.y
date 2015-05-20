/*!if GRAMMAR_PROLOGUE*/

// Lexical Symbol for superscalar regions
%token<token_atrib> TWO_DOTS ".."

// Tokens for subparsing
%token<token_atrib> SUBPARSE_SUPERSCALAR_DECLARATOR "<subparse-superscalar-declarator>"
%token<token_atrib> SUBPARSE_SUPERSCALAR_DECLARATOR_LIST "<subparse-superscalar-declarator-list>"
%token<token_atrib> SUBPARSE_SUPERSCALAR_EXPRESSION "<subparse-superscalar-expression>"

// Tokens for rules
%type<ast> superscalar_declarator superscalar_declarator_list opt_superscalar_region_spec_list superscalar_region_spec_list superscalar_region_spec

/*!endif*/
/*!if GRAMMAR_RULES*/

// Grammar entry point
subparsing : SUBPARSE_SUPERSCALAR_DECLARATOR superscalar_declarator opt_superscalar_region_spec_list
{
	$$ = ASTMake2(AST_SUPERSCALAR_DECLARATOR, $2, $3, ast_get_locus($2), NULL);
}
| SUBPARSE_SUPERSCALAR_DECLARATOR_LIST superscalar_declarator_list
{
    $$ = $2;
}
| SUBPARSE_SUPERSCALAR_EXPRESSION expression opt_superscalar_region_spec_list
{
	$$ = ASTMake2(AST_SUPERSCALAR_EXPRESSION, $2, $3, ast_get_locus($2), NULL);
}
;

superscalar_declarator_list : superscalar_declarator opt_superscalar_region_spec_list
{
	AST ss_decl = ASTMake2(AST_SUPERSCALAR_DECLARATOR, $1, $2, ast_get_locus($1), NULL);
    $$ = ASTListLeaf(ss_decl);
}
| superscalar_declarator_list ',' superscalar_declarator opt_superscalar_region_spec_list
{
	AST ss_decl = ASTMake2(AST_SUPERSCALAR_DECLARATOR, $3, $4, ast_get_locus($3), NULL);
    $$ = ASTList($1, ss_decl);
}
;

superscalar_declarator : declarator_id
{
	$$ = $1;
}
| superscalar_declarator '[' assignment_expression ']'
{
	$$ = ASTMake4(AST_DECLARATOR_ARRAY, $1, $3, NULL, NULL, ast_get_locus($1), NULL);
}
;

opt_superscalar_region_spec_list :
/* NULL */
{
	$$ = NULL;
}
| superscalar_region_spec_list
;

superscalar_region_spec_list : superscalar_region_spec
{
	$$ = ASTListLeaf($1);
}
| superscalar_region_spec_list superscalar_region_spec
{
	$$ = ASTList($1, $2);
}
;

superscalar_region_spec : '{' '}'
{
	$$ = ASTLeaf(AST_SUPERSCALAR_REGION_SPEC_FULL, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);
}
| '{' expression '}'
{
	$$ = ASTMake1(AST_SUPERSCALAR_REGION_SPEC_SINGLE, $2, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);
}
| '{' expression TWO_DOTS expression '}'
{
	$$ = ASTMake2(AST_SUPERSCALAR_REGION_SPEC_RANGE, $2, $4, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);
}
| '{' expression ':' expression '}'
{
	$$ = ASTMake2(AST_SUPERSCALAR_REGION_SPEC_LENGTH, $2, $4, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);
}
;


/*!endif*/
