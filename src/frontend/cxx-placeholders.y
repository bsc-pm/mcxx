/*!if GRAMMAR_PROLOGUE*/

%type<ast> statement_placeholder
%token<token_atrib> STATEMENT_PLACEHOLDER "<statement-placeholder>"

/*!endif*/
/*!if GRAMMAR_RULES*/

/*!if FORTRAN2003*/
non_top_level_program_unit_stmt : statement_placeholder
;
/*!endif*/
/*!ifnot FORTRAN2003*/
statement : statement_placeholder
{
    $$ = $1;
}
;
/*!endif*/

/*!ifnot FORTRAN2003*/
statement_placeholder : STATEMENT_PLACEHOLDER 
{
    // This is an empty statement
    $$ = ASTLeaf(AST_STATEMENT_PLACEHOLDER, make_locus($1.token_file, $1.token_line, 0), $1.token_text);
}
;
/*!endif*/
/*!if FORTRAN2003*/
statement_placeholder : STATEMENT_PLACEHOLDER eos
{
    // This is an empty statement
    $$ = ASTLeaf(AST_STATEMENT_PLACEHOLDER, make_locus($1.token_file, $1.token_line, 0), $1.token_text);
}
;
/*!endif*/

/*!endif*/

// This is code

