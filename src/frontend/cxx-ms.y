/*!if GRAMMAR_PROLOGUE */

%token<token_atrib> TOK_DECLSPEC "__declspec"

%type<ast> declspec_specifier
%type<ast> extended_decl_modifier_list
%type<ast> extended_decl_modifier_list0
%type<ast> extended_decl_modifier

/*!endif*/
/*!if GRAMMAR_RULES*/
nontype_specifier : declspec_specifier
{
    $$ = $1;
}
;

declspec_specifier : TOK_DECLSPEC '(' extended_decl_modifier_list ')'
{
    $$ = ASTMake1(AST_DECLSPEC, $3, $1.token_file, $1.token_line, $1.token_text);
}
;

extended_decl_modifier_list : /* empty */
{
    $$ = NULL;
}
| extended_decl_modifier_list0
{
    $$ = $1;
}
;

extended_decl_modifier_list0 : extended_decl_modifier
{
    $$ = ASTListLeaf($1);
}
| extended_decl_modifier_list0 ',' extended_decl_modifier
{
    $$ = ASTList($1, $3);
}
;

extended_decl_modifier : IDENTIFIER
{
    $$ = ASTMake1(AST_DECLSPEC_ITEM, NULL, $1.token_file, $1.token_line, $1.token_text);
}
| IDENTIFIER '(' expression_list ')'
{
    $$ = ASTMake1(AST_DECLSPEC_ITEM, $3, $1.token_file, $1.token_line, $1.token_text);
}
;

/*!endif*/
