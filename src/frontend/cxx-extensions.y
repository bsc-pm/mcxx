/*!if GRAMMAR_PROLOGUE*/

%type<ast> shape_seq
%type<ast> shape

/*!endif*/
/*!if GRAMMAR_RULES*/

cast_expression : shape_seq cast_expression
{
    $$ = ASTMake2(AST_SHAPING_EXPRESSION, $1, $2, ASTFileName($1), ASTLine($1), NULL);
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
/*!endif*/
