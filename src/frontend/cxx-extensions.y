/*!if GRAMMAR_PROLOGUE*/

%token<token_atrib> SUBPARSE_SHAPING_EXPRESSION "<subparse-shaping-expression>"

%type<ast> shaping_expression
%type<ast> shape_seq
%type<ast> shape

/*!endif*/
/*!if GRAMMAR_RULES*/

shaping_expression : shape_seq assignment_expression
{
    $$ = ASTMake2(AST_SHAPING_EXPRESSION, $1, $2, ASTFileName($1), ASTLine($1), NULL);
}
| assignment_expression
{
    $$ = $1;
}
;

shape_seq : shape_seq shape
{
    $$ = ASTList($1, $2);
}
| shape
{
    $$ = ASTListLeaf($1);
}
;

shape: '[' expression ']'
{
    $$ = $2;
}
;

subparsing : SUBPARSE_SHAPING_EXPRESSION shaping_expression
{
    $$ = $2;
}
;

/*!endif*/
