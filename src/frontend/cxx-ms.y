/*!if GRAMMAR_PROLOGUE */

%token<token_atrib> TOKEN_DECLSPEC "__declspec"

%token<token_atrib> MS_INT8 "__int8"
%token<token_atrib> MS_INT16 "__int16"
%token<token_atrib> MS_INT32 "__int32"
%token<token_atrib> MS_INT64 "__int64"

%type<ast> declspec_specifier
%type<ast> declspec_specifier_seq
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

declspec_specifier : TOKEN_DECLSPEC '(' extended_decl_modifier_list ')'
{
    $$ = ASTMake1(AST_MS_DECLSPEC, $3, $1.token_file, $1.token_line, $1.token_text);
}
;

declspec_specifier_seq : declspec_specifier
{
    $$ = ASTListLeaf($1);
}
| declspec_specifier_seq declspec_specifier
{
    $$ = ASTList($1, $2);
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
    $$ = ASTMake1(AST_MS_DECLSPEC_ITEM, NULL, $1.token_file, $1.token_line, $1.token_text);
}
| IDENTIFIER '(' expression_list ')'
{
    $$ = ASTMake1(AST_MS_DECLSPEC_ITEM, $3, $1.token_file, $1.token_line, $1.token_text);
}
;

/*!if CPLUSPLUS*/
class_head : class_key declspec_specifier_seq
{
	$$ = ASTMake4(AST_MS_CLASS_HEAD_SPEC, $1, NULL, NULL, $2, ASTFileName($1), ASTLine($1), NULL);
}
| class_key declspec_specifier_seq id_type_expr
{
	$$ = ASTMake4(AST_MS_CLASS_HEAD_SPEC, $1, $3, NULL, $2, ASTFileName($1), ASTLine($1), NULL);
}
| class_key declspec_specifier_seq base_clause
{
	$$ = ASTMake4(AST_MS_CLASS_HEAD_SPEC, $1, NULL, $3, $2, ASTFileName($1), ASTLine($1), NULL);
}
| class_key declspec_specifier_seq id_type_expr base_clause
{
	$$ = ASTMake4(AST_MS_CLASS_HEAD_SPEC, $1, $3, $4, $2, ASTFileName($1), ASTLine($1), NULL);
}
;

elaborated_type_specifier: class_key declspec_specifier_seq id_type_expr
{
	$$ = ASTMake3(AST_MS_ELABORATED_TYPE_CLASS_SPEC, $1, $3, $2, ASTFileName($1), ASTLine($1), NULL);
}
| ENUM declspec_specifier_seq id_type_expr
{
	$$ = ASTMake2(AST_MS_ELABORATED_TYPE_ENUM_SPEC, $3, $2, $1.token_file, $1.token_line, NULL);
}
;
/*!endif*/

/*!if C99*/
class_head : class_key declspec_specifier_seq
{
	$$ = ASTMake4(AST_MS_CLASS_HEAD_SPEC, $1, NULL, NULL, $2, ASTFileName($1), ASTLine($1), NULL);
}
| class_key declspec_specifier_seq IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_file, $3.token_line, $3.token_text);

	$$ = ASTMake4(AST_MS_CLASS_HEAD_SPEC, $1, identifier, NULL, $2, ASTFileName($1), ASTLine($1), NULL);
}
;

elaborated_type_specifier : class_key attributes IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_file, $3.token_line, $3.token_text);

	$$ = ASTMake3(AST_MS_ELABORATED_TYPE_CLASS_SPEC, $1, identifier, $2, ASTFileName($1), ASTLine($1), NULL);
}
| ENUM attributes IDENTIFIER
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_file, $3.token_line, $3.token_text);

	$$ = ASTMake2(AST_MS_ELABORATED_TYPE_ENUM_SPEC, identifier, $2, $1.token_file, $1.token_line, NULL);
}
;
/*!endif*/

enum_specifier : ENUM declspec_specifier_seq IDENTIFIER '{' enumeration_list '}'
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_file, $3.token_line, $3.token_text);

	$$ = ASTMake3(AST_MS_ENUM_SPECIFIER, identifier, $5, $2, $1.token_file, $1.token_line, NULL);
}
| ENUM declspec_specifier_seq '{' enumeration_list '}'
{
	$$ = ASTMake3(AST_MS_ENUM_SPECIFIER, NULL, $4, $2, $1.token_file, $1.token_line, NULL);
}
| ENUM declspec_specifier_seq IDENTIFIER '{' '}'
{
	AST identifier = ASTLeaf(AST_SYMBOL, $3.token_file, $3.token_line, $3.token_text);

	$$ = ASTMake3(AST_MS_ENUM_SPECIFIER, identifier, NULL, $2, $1.token_file, $1.token_line, NULL);
}
| ENUM declspec_specifier_seq '{' '}'
{
	$$ = ASTMake3(AST_MS_ENUM_SPECIFIER, NULL, NULL, $2, $1.token_file, $1.token_line, NULL);
}
;

builtin_types : MS_INT8
{
    $$ = ASTLeaf(AST_MS_INT8, $1.token_file, $1.token_line, NULL);
}
| MS_INT16
{
    $$ = ASTLeaf(AST_MS_INT16, $1.token_file, $1.token_line, NULL);
}
| MS_INT32
{
    $$ = ASTLeaf(AST_MS_INT32, $1.token_file, $1.token_line, NULL);
}
| MS_INT64
{
    $$ = ASTLeaf(AST_MS_INT64, $1.token_file, $1.token_line, NULL);
}
;

/*!endif*/
