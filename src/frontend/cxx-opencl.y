/*!if GRAMMAR_PROLOGUE */
%token<token_atrib> OPENCL_GLOBAL "__global" 
%token<token_atrib> OPENCL_KERNEL "__kernel"
%token<token_atrib> OPENCL_CONSTANT "__constant"
%token<token_atrib> OPENCL_LOCAL "__local"

%type<ast> opencl_specifiers

/*!endif*/
/*!if GRAMMAR_RULES*/
nontype_specifier_without_attribute : opencl_specifiers
{
    $$ = $1;
}
;

opencl_specifiers : OPENCL_GLOBAL
{
    $$ = ASTLeaf(AST_OPENCL_GLOBAL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| OPENCL_KERNEL
{
    $$ = ASTLeaf(AST_OPENCL_KERNEL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| OPENCL_CONSTANT
{
    $$ = ASTLeaf(AST_OPENCL_CONSTANT, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
| OPENCL_LOCAL
{
    $$ = ASTLeaf(AST_OPENCL_LOCAL, make_locus(@1.first_filename, @1.first_line, @1.first_column), $1.token_text);
}
;
/*!endif*/
