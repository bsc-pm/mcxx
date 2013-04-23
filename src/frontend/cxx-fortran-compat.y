/*!if GRAMMAR_PROLOGUE*/

%type<ast> fortran_allocate_statement

%token<token_atrib> C_FORTRAN_ALLOCATE

/*!endif*/

/*!if GRAMMAR_RULES*/

no_if_statement : fortran_allocate_statement
{
    $$ = $1;
}
;

fortran_allocate_statement : C_FORTRAN_ALLOCATE '(' expression ')' ';'
{
    $$ = ASTMake1(AST_FORTRAN_ALLOCATE_STATEMENT, $3, make_locus($1.token_file, $1.token_line, 0), NULL);
}
;

/*!endif*/
