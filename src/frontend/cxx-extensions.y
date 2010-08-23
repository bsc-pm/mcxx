/*!if GRAMMAR_PROLOGUE*/

%type<ast> shape_seq
%type<ast> shape

/*!endif*/
/*!if GRAMMAR_RULES*/

cast_expression : shape_seq cast_expression %merge<ambiguityHandler>
{
    $$ = ASTMake2(AST_SHAPING_EXPRESSION, $1, $2, ASTFileName($1), ASTLine($1), NULL);
}
;

shape_seq : shape_seq shape %dprec 2
{
    $$ = ASTList($1, $2);
}
| shape %dprec 1
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
