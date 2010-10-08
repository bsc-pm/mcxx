/*!if GRAMMAR_PROLOGUE*/
%token<token_atrib> XL_BUILTIN_SPEC "_Builtin"

/*!endif*/
/*!if GRAMMAR_RULES*/

nontype_specifier_without_attribute : XL_BUILTIN_SPEC
{
    $$ = ASTLeaf(AST_XL_BUILTIN_SPEC, $1.token_file, $1.token_line, $1.token_text);
}
;

/*!endif*/
