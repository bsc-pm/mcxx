/*!if GRAMMAR_PROLOGUE*/

%token<token_atrib> VERBATIM_PRAGMA "<verbatim pragma>"
%token<token_atrib> VERBATIM_CONSTRUCT "<verbatim construct>"
%token<token_atrib> VERBATIM_TYPE "<verbatim type clause>"
%token<token_atrib> VERBATIM_TEXT "<verbatim text>"

%type<ast> verbatim_construct

%token<token_atrib> PRAGMA_CUSTOM "<pragma-custom>"
%token<token_atrib> PRAGMA_CUSTOM_NEWLINE "<pragma-custom-newline>"
%token<token_atrib> PRAGMA_CUSTOM_DIRECTIVE "<pragma-custom-directive>"
%token<token_atrib> PRAGMA_CUSTOM_CONSTRUCT "<pragma-custom-construct>"
%token<token_atrib> PRAGMA_CUSTOM_CLAUSE "<pragma-custom-clause>"

%token<token_atrib> PRAGMA_CLAUSE_ARG_TEXT "<pragma-clause-argument-text>"

%type<ast> pragma_custom_directive
%type<ast> pragma_custom_line_directive
%type<ast> pragma_custom_line_construct
%type<ast> pragma_custom_construct_statement
%type<ast> pragma_custom_construct_declaration
%type<ast> pragma_custom_construct_member_declaration
%type<ast> pragma_custom_clause
%type<ast> pragma_custom_clause_seq
%type<ast> pragma_custom_clause_opt_seq

// %type<ast> pragma_expression_entity
// %type<ast> pragma_expression_entity_list

%type<ast> pragma_clause_arg_list

%type<text> pragma_clause_arg
%type<text> pragma_clause_arg_item 
%type<text> pragma_clause_arg_text

/*!endif*/
/*!if GRAMMAR_RULES*/

// Grammar entry point
no_if_statement : pragma_custom_construct_statement
{
    $$ = $1;
}
| pragma_custom_directive
{
    $$ = $1;
}
;

declaration : pragma_custom_construct_declaration
{
    $$ = $1;
}
| pragma_custom_directive
{
	$$ = $1;
}
;

member_declaration : pragma_custom_construct_member_declaration
{
    $$ = $1;
}
| pragma_custom_directive
{
    $$ = $1;
}
;

// Pragma custom

pragma_custom_directive : PRAGMA_CUSTOM pragma_custom_line_directive
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_DIRECTIVE, $2, NULL, $1.token_file, $1.token_line, $1.token_text);
}
;

pragma_custom_construct_declaration : PRAGMA_CUSTOM pragma_custom_line_construct declaration
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, $1.token_file, $1.token_line, $1.token_text);
}
;

pragma_custom_construct_member_declaration : PRAGMA_CUSTOM pragma_custom_line_construct member_declaration
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, $1.token_file, $1.token_line, $1.token_text);
}
;

pragma_custom_construct_statement : PRAGMA_CUSTOM pragma_custom_line_construct statement
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, $1.token_file, $1.token_line, $1.token_text);
}
;

pragma_custom_line_directive : PRAGMA_CUSTOM_DIRECTIVE pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $2, NULL, $1.token_file, $1.token_line, $1.token_text);
}
| PRAGMA_CUSTOM_DIRECTIVE '(' pragma_clause_arg_list ')' pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $5, $3, $1.token_file, $1.token_line, $1.token_text);
}
| PRAGMA_CUSTOM_NEWLINE
{
    // This is a degenerated case caused by wrong designed pragmas
    $$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, NULL, NULL, NULL, 0, NULL);
}
;

pragma_custom_line_construct : PRAGMA_CUSTOM_CONSTRUCT pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $2, NULL, $1.token_file, $1.token_line, $1.token_text);
}
| PRAGMA_CUSTOM_CONSTRUCT '(' pragma_clause_arg_list ')' pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $5, $3, $1.token_file, $1.token_line, $1.token_text);
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

pragma_custom_clause : PRAGMA_CUSTOM_CLAUSE '(' pragma_clause_arg_list ')'
{
	$$ = ASTMake1(AST_PRAGMA_CUSTOM_CLAUSE, $3, $1.token_file, $1.token_line, $1.token_text);
}
| PRAGMA_CUSTOM_CLAUSE '(' ')'
{
	$$ = ASTMake1(AST_PRAGMA_CUSTOM_CLAUSE, NULL, $1.token_file, $1.token_line, $1.token_text);
}
| PRAGMA_CUSTOM_CLAUSE 
{
	$$ = ASTMake1(AST_PRAGMA_CUSTOM_CLAUSE, NULL, $1.token_file, $1.token_line, $1.token_text);
}
;

pragma_clause_arg_list : pragma_clause_arg
{
    AST node = ASTLeaf(AST_PRAGMA_CLAUSE_ARG, NULL, 0, $1);

    $$ = ASTListLeaf(node);
}
;

pragma_clause_arg : pragma_clause_arg_item
{
    $$ = $1;
}
| pragma_clause_arg pragma_clause_arg_item
{
    $$ = strappend($1, $2);
}
;

pragma_clause_arg_item : pragma_clause_arg_text
{
    $$ = $1;
}
;

pragma_clause_arg_text : PRAGMA_CLAUSE_ARG_TEXT
{
    $$ = $1.token_text;
}
;

// Verbatim construct

verbatim_construct : VERBATIM_PRAGMA VERBATIM_TYPE '(' IDENTIFIER ')' VERBATIM_TEXT
{
    AST ident = ASTLeaf(AST_SYMBOL, $4.token_file, $4.token_line, $4.token_text);

    $$ = ASTMake1(AST_VERBATIM, ident, $1.token_file, $1.token_line, $6.token_text);
}
| VERBATIM_PRAGMA VERBATIM_TEXT
{
    $$ = ASTMake1(AST_VERBATIM, NULL, $1.token_file, $1.token_line, $2.token_text);
}
;

block_declaration : verbatim_construct
{
    $$ = $1;
}
;

member_declaration : verbatim_construct
{
    $$ = $1;
}
;

/*!endif*/
