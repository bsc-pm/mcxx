/*!if GRAMMAR_PROLOGUE*/

// Lexical Symbol for superscalar regions
%token<token_atrib> TWO_DOTS ".."

// Tokens for subparsing
%token<token_atrib> SUBPARSE_SUPERSCALAR_DECLARATOR "<subparse-superscalar-declarator>"

// Tokens for rules
%type<ast> superscalar_declarator opt_superscalar_region_spec_list superscalar_region_spec_list superscalar_region_spec

/*!endif*/
/*!if GRAMMAR_RULES*/

// Grammar entry point
subparsing : SUBPARSE_SUPERSCALAR_DECLARATOR superscalar_declarator opt_superscalar_region_spec_list
{
	$$ = ASTMake2(AST_SUPERSCALAR_DECLARATOR, $2, $3, ASTFileName($2), ASTLine($2), NULL);
}
;

superscalar_declarator : declarator_id
{
	$$ = $1;
}
| superscalar_declarator '[' assignment_expression ']'
{
	$$ = ASTMake4(AST_DECLARATOR_ARRAY, $1, $3, NULL, NULL, ASTFileName($1), ASTLine($1), NULL);
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
	$$ = ASTLeaf(AST_SUPERSCALAR_REGION_SPEC_FULL, $1.token_file, $1.token_line, NULL);
}
| '{' expression '}'
{
	$$ = ASTMake1(AST_SUPERSCALAR_REGION_SPEC_SINGLE, $2, $1.token_file, $1.token_line, NULL);
}
| '{' expression TWO_DOTS expression '}'
{
	$$ = ASTMake2(AST_SUPERSCALAR_REGION_SPEC_RANGE, $2, $4, $1.token_file, $1.token_line, NULL);
}
| '{' expression ':' expression '}'
{
	$$ = ASTMake2(AST_SUPERSCALAR_REGION_SPEC_LENGTH, $2, $4, $1.token_file, $1.token_line, NULL);
}
;


/*!endif*/
