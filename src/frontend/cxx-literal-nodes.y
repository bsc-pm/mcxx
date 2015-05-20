/*!if GRAMMAR_PROLOGUE*/
%token<token_atrib> NODECL_LITERAL_EXPR "<nodecl-literal-expression>"
%token<token_atrib> NODECL_LITERAL_STMT "<nodecl-literal-statement>"

%token<token_atrib> SYMBOL_LITERAL_REF "<symbol-literal-reference>"
%token<token_atrib> TYPE_LITERAL_REF "<type-literal-reference>"

%type<ast> nodecl_literal_expr
%type<ast> nodecl_literal_stmt
%type<ast> nodecl_literal_attribute_seq
%type<ast> nodecl_string_literal

%type<ast> symbol_literal_ref
%type<ast> type_literal_ref

/*!endif*/

/*!if GRAMMAR_RULES*/

nodecl_literal_expr : NODECL_LITERAL_EXPR '(' nodecl_literal_attribute_seq  ')'
{
    $$ = ASTMake1(AST_NODECL_LITERAL, $3, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);
}
;

/*!ifnot FORTRAN2003*/
nodecl_literal_stmt : NODECL_LITERAL_STMT '(' nodecl_literal_attribute_seq ')'
/*!endif*/
/*!if FORTRAN2003*/
nodecl_literal_stmt : NODECL_LITERAL_STMT '(' nodecl_literal_attribute_seq ')' eos
/*!endif*/
{
    $$ = ASTMake1(AST_NODECL_LITERAL, $3, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);
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

symbol_literal_ref : SYMBOL_LITERAL_REF '(' nodecl_string_literal ')'
{
    $$ = ASTMake1(AST_SYMBOL_LITERAL_REF, $3, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);
}
;

type_literal_ref : TYPE_LITERAL_REF '(' nodecl_string_literal ')'
{
    $$ = ASTMake1(AST_TYPE_LITERAL_REF, $3, make_locus(@1.first_filename, @1.first_line, @1.first_column), NULL);
}
;

/*!if FORTRAN2003*/
nodecl_string_literal : CHAR_LITERAL
/*!endif*/
/*!ifnot FORTRAN2003*/
nodecl_string_literal : STRING_LITERAL
/*!endif*/
{
    $$ = ASTLeaf(AST_STRING_LITERAL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;

/* Entry points in the grammars */
/*!ifnot FORTRAN2003*/
primary_expression : nodecl_literal_expr
{
    $$ = $1;
}
;

unqualified_id : symbol_literal_ref
{
    $$ = $1;
}
;

/*!if CPLUSPLUS*/
unqualified_id_no_destructor : symbol_literal_ref
{
    $$ = $1;
}
;
/*!endif*/

nondeclarating_statement : nodecl_literal_stmt
{
    $$ = $1;
}
;

/*!if C99*/
simple_type_specifier : type_literal_ref
/*!endif*/
/*!if CPLUSPLUS*/
named_simple_type_specifier : type_literal_ref
/*!endif*/
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
declaration_type_spec : type_literal_ref
;
name : symbol_literal_ref
;
/*!endif*/

/*!endif*/
