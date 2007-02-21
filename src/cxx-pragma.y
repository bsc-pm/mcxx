/*!if GRAMMAR_PROLOGUE*/

%token<token_atrib> PRAGMA_CUSTOM
%token<token_atrib> PRAGMA_CUSTOM_NEWLINE
%token<token_atrib> PRAGMA_CUSTOM_DIRECTIVE
%token<token_atrib> PRAGMA_CUSTOM_CLAUSE

%type<ast> pragma_custom_directive
%type<ast> pragma_custom_line
%type<ast> pragma_custom_construct
%type<ast> pragma_custom_clause
%type<ast> pragma_custom_clause_seq
%type<ast> pragma_custom_clause_opt_seq

/*!endif*/
/*!if GRAMMAR_RULES*/

// Grammar entry point
no_if_statement : pragma_custom_construct
{
    $$ = $1;
}
;

declaration : pragma_custom_directive
{
	$$ = $1;
}
;

// Pragma custom

pragma_custom_directive : PRAGMA_CUSTOM pragma_custom_line 
{
	$$ = ASTMake1(AST_PRAGMA_CUSTOM_DIRECTIVE, $2, $1.token_line, $1.token_text);
}
;

pragma_custom_line : PRAGMA_CUSTOM_DIRECTIVE pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake1(AST_PRAGMA_CUSTOM_LINE, $2, $1.token_line, $1.token_text);
}
;

pragma_custom_construct : PRAGMA_CUSTOM pragma_custom_line statement
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, $1.token_line, $1.token_text);
}
;

pragma_custom_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| pragma_custom_clause_seq
{
	$$ = $1;
}
;

pragma_custom_clause_seq : pragma_custom_clause
{
	$$ = ASTListLeaf($1);
}
| pragma_custom_clause_seq ',' pragma_custom_clause
{
	$$ = ASTList($1, $3);
}
| pragma_custom_clause_seq pragma_custom_clause
{
	$$ = ASTList($1, $2);
}
;

pragma_custom_clause : PRAGMA_CUSTOM_CLAUSE '(' expression_list ')'
{
	$$ = ASTMake1(AST_PRAGMA_CUSTOM_CLAUSE, $3, $1.token_line, $1.token_text);
}
| PRAGMA_CUSTOM_CLAUSE '(' ')'
{
	$$ = ASTMake1(AST_PRAGMA_CUSTOM_CLAUSE, NULL, $1.token_line, $1.token_text);
}
| PRAGMA_CUSTOM_CLAUSE 
{
	$$ = ASTMake1(AST_PRAGMA_CUSTOM_CLAUSE, NULL, $1.token_line, $1.token_text);
}
;

/*!endif*/
