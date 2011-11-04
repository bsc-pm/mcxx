/*!if GRAMMAR_PROLOGUE*/

/*!ifnot FORTRAN2003*/
%token<token_atrib> VERBATIM_PRAGMA "<verbatim pragma>"
%token<token_atrib> VERBATIM_CONSTRUCT "<verbatim construct>"
%token<token_atrib> VERBATIM_TYPE "<verbatim type clause>"
%token<token_atrib> VERBATIM_TEXT "<verbatim text>"

%type<ast> verbatim_construct
/*!endif*/

%token<token_atrib> PRAGMA_CUSTOM "<pragma-custom>"
%token<token_atrib> PRAGMA_CUSTOM_NEWLINE "<pragma-custom-newline>"
%token<token_atrib> PRAGMA_CUSTOM_DIRECTIVE "<pragma-custom-directive>"
%token<token_atrib> PRAGMA_CUSTOM_CONSTRUCT "<pragma-custom-construct>"
%token<token_atrib> PRAGMA_CUSTOM_END_CONSTRUCT "<pragma-custom-end-construct>"
%token<token_atrib> PRAGMA_CUSTOM_CONSTRUCT_NOEND "<pragma-custom-construct-noend>"
%token<token_atrib> PRAGMA_CUSTOM_END_CONSTRUCT_NOEND "<pragma-custom-end-construct-noend>"

%token<token_atrib> PRAGMA_CUSTOM_CLAUSE "<pragma-custom-clause>"

%token<token_atrib> PRAGMA_CLAUSE_ARG_TEXT "<pragma-clause-argument-text>"

%type<ast> pragma_custom_directive
%type<ast> pragma_custom_line_directive
%type<ast> pragma_custom_line_construct
%type<ast> pragma_custom_construct_statement
/*!ifnot FORTRAN2003*/
%type<ast> pragma_custom_construct_declaration
%type<ast> pragma_custom_construct_member_declaration
/*!endif*/
/*!if FORTRAN2003*/
%type<ast2> pragma_custom_construct_range
%type<ast2> pragma_custom_noend_construct_range
%type<ast> pragma_custom_noend_line_construct
%type<ast> pragma_custom_end_construct
%type<ast> pragma_custom_end_construct_noend
%type<ast> pragma_custom_construct_program_unit
/*!endif*/
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
/*!ifnot FORTRAN2003*/
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
/*!endif*/
/*!if FORTRAN2003*/
non_top_level_program_unit_stmts: pragma_custom_construct_statement
{
    $$ = $1;
}
| pragma_custom_directive
{
    $$ = $1;
}
;
program_unit : pragma_custom_construct_program_unit
{
    $$ = $1;
}
;
/*!endif*/

// Pragma custom

pragma_custom_directive : PRAGMA_CUSTOM pragma_custom_line_directive
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_DIRECTIVE, $2, NULL, $1.token_file, $1.token_line, $1.token_text);
}
;

/*!ifnot FORTRAN2003*/
pragma_custom_construct_declaration : PRAGMA_CUSTOM pragma_custom_line_construct declaration
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, NULL, $1.token_file, $1.token_line, $1.token_text);
}
;

pragma_custom_construct_member_declaration : PRAGMA_CUSTOM pragma_custom_line_construct member_declaration
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, NULL, $1.token_file, $1.token_line, $1.token_text);
}
;

pragma_custom_construct_statement : PRAGMA_CUSTOM pragma_custom_line_construct statement
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, NULL, $1.token_file, $1.token_line, $1.token_text);
}
;
/*!endif*/
/*!if FORTRAN2003*/
pragma_custom_construct_statement : PRAGMA_CUSTOM pragma_custom_line_construct pragma_custom_construct_range
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3[0], $3[1], $1.token_file, $1.token_line, $1.token_text);
}
| PRAGMA_CUSTOM pragma_custom_noend_line_construct pragma_custom_noend_construct_range
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3[0], $3[1], $1.token_file, $1.token_line, $1.token_text);
}
;

// This case allows a sequence of statements but forces a end construct to appear
pragma_custom_construct_range : block pragma_custom_end_construct
{
    $$[0] = $1;
    $$[1] = $2;
}
;

// These cases only allows a single statements but does not require an end construct to appear
pragma_custom_noend_construct_range : non_top_level_program_unit_stmts pragma_custom_end_construct_noend
{
    $$[0] = ASTMake1(AST_COMPOUND_STATEMENT, ASTListLeaf($1), ASTFileName($1), ASTLine($1), NULL);
    $$[1] = $2;
}
| non_top_level_program_unit_stmts
{
    $$[0] = ASTMake1(AST_COMPOUND_STATEMENT, ASTListLeaf($1), ASTFileName($1), ASTLine($1), NULL);
    $$[1] = NULL;
}
;

pragma_custom_end_construct : PRAGMA_CUSTOM PRAGMA_CUSTOM_END_CONSTRUCT pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $3, NULL, $2.token_file, $2.token_line, $2.token_text);
}
;

pragma_custom_end_construct_noend : PRAGMA_CUSTOM PRAGMA_CUSTOM_END_CONSTRUCT_NOEND pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $3, NULL, $2.token_file, $2.token_line, $2.token_text);
}
;

pragma_custom_construct_program_unit : PRAGMA_CUSTOM pragma_custom_line_construct program_unit
{
	$$ = ASTMake3(AST_PRAGMA_CUSTOM_CONSTRUCT, $2, $3, NULL, $1.token_file, $1.token_line, $1.token_text);
}
;

pragma_custom_noend_line_construct : PRAGMA_CUSTOM_CONSTRUCT_NOEND pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $2, NULL, $1.token_file, $1.token_line, $1.token_text);
}
| PRAGMA_CUSTOM_CONSTRUCT_NOEND '(' pragma_clause_arg_list ')' pragma_custom_clause_opt_seq PRAGMA_CUSTOM_NEWLINE
{
	$$ = ASTMake2(AST_PRAGMA_CUSTOM_LINE, $5, $3, $1.token_file, $1.token_line, $1.token_text);
}
;
/*!endif*/

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

/*!ifnot FORTRAN2003*/
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

common_block_declaration : verbatim_construct
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

/*!endif*/
