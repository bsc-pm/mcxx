/*!if GRAMMAR_PROLOGUE*/
%type<ast> custom_construct_statement
%type<ast> custom_construct_header
%type<ast> custom_construct_parameters_seq
%type<ast> custom_construct_parameter
%token<token_atrib> CONSTRUCT
/*!endif*/
/*!if GRAMMAR_RULES*/

// Grammar entry point
statement : custom_construct_statement
{
    $$ = $1;
}
;

// Custom code construct

custom_construct_statement : custom_construct_header statement
{
    $$ = ASTMake2(AST_CUSTOM_CONSTRUCT_STATEMENT, $1, $2, ASTLine($1), NULL);
};

custom_construct_header : CONSTRUCT IDENTIFIER custom_construct_parameters_seq
{
    $$ = ASTMake1(AST_CUSTOM_CONSTRUCT_HEADER, $3, $1.token_line, $2.token_text);
}
| CONSTRUCT IDENTIFIER 
{
    $$ = ASTMake1(AST_CUSTOM_CONSTRUCT_HEADER, NULL, $1.token_line, $2.token_text);
}
;

custom_construct_parameters_seq : custom_construct_parameter
{
    $$ = ASTListLeaf($1);
}
| custom_construct_parameters_seq ',' custom_construct_parameter
{
    $$ = ASTList($1, $3);
}
;

custom_construct_parameter : IDENTIFIER ':' expression
{
    $$ = ASTMake2(AST_CUSTOM_CONSTRUCT_PARAMETER,
            ASTLeaf(AST_SYMBOL, $1.token_line, $1.token_text), 
            $3, 
            $1.token_line, NULL);
}
;

/*!endif*/
