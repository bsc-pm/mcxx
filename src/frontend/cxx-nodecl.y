/*!if GRAMMAR_PROLOGUE*/
%token<token_atrib> NODECL_LITERAL_EXPR "<nodecl-literal-expression>"
%token<token_atrib> NODECL_LITERAL_STMT "<nodecl-literal-statement>"

%type<ast> nodecl_literal_expr
%type<ast> nodecl_literal_stmt
%type<ast> nodecl_literal_attribute_seq
%type<ast> nodecl_string_literal

/*!endif*/

/*!if GRAMMAR_RULES*/

nodecl_literal_expr : NODECL_LITERAL_EXPR '(' nodecl_literal_attribute_seq  ')'
{
    $$ = ASTMake1(AST_NODECL_LITERAL, $3, $1.token_file, $1.token_line, NULL);
}
;

nodecl_literal_stmt : NODECL_LITERAL_STMT '(' nodecl_literal_attribute_seq ')'
{
    $$ = ASTMake1(AST_NODECL_LITERAL, $3, $1.token_file, $1.token_line, NULL);
}
;

nodecl_literal_attribute_seq : nodecl_literal_attribute_seq ',' nodecl_string_literal
{
    $$ = ASTList($1, $3);
}
| nodecl_string_literal
{
    $$ = ASTListLeaf($1);
}
;

/*!if FORTRAN2003*/
nodecl_string_literal : CHAR_LITERAL
/*!endif*/
/*!ifnot FORTRAN2003*/
nodecl_string_literal : STRING_LITERAL
/*!endif*/
{
    $$ = ASTLeaf(AST_STRING_LITERAL, $1.token_file, $1.token_line, $1.token_text);
}
;

/* Entry points in the grammars */
/*!ifnot FORTRAN2003*/
primary_expression : nodecl_literal_expr
{
    $$ = $1;
}
;

no_if_statement : nodecl_literal_stmt
{
    $$ = $1;
}
;
/*!endif*/

/*!if FORTRAN2003*/
primary : nodecl_literal_expr
;
non_top_level_program_unit_stmt : nodecl_literal_stmt
;
/*!endif*/

/*!endif*/
