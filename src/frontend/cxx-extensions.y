/*!if GRAMMAR_PROLOGUE*/

%type<ast> shape_seq
%type<ast> shape

/*!endif*/
/*!if GRAMMAR_RULES*/

postfix_expression : postfix_expression '[' logical_or_expression ':' logical_or_expression ']'
{
    $$ = ASTMake3(AST_ARRAY_SECTION, $1, $3, $5, ASTFileName($1), ASTLine($1), NULL);
}
| postfix_expression '[' logical_or_expression ';' logical_or_expression ']'
{
    $$ = ASTMake3(AST_ARRAY_SECTION_SIZE, $1, $3, $5, ASTFileName($1), ASTLine($1), NULL);
}

;

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
