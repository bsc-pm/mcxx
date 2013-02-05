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
    $$ = ASTLeaf(AST_OPENCL_GLOBAL, $1.token_file, $1.token_line, $1.token_text);
}
| OPENCL_KERNEL
{
    $$ = ASTLeaf(AST_OPENCL_KERNEL, $1.token_file, $1.token_line, $1.token_text);
}
| OPENCL_CONSTANT
{
    $$ = ASTLeaf(AST_OPENCL_CONSTANT, $1.token_file, $1.token_line, $1.token_text);
}
| OPENCL_LOCAL
{
    $$ = ASTLeaf(AST_OPENCL_LOCAL, $1.token_file, $1.token_line, $1.token_text);
}
;
/*!endif*/
