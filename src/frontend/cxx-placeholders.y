/*!if GRAMMAR_PROLOGUE*/

%token<token_atrib> STATEMENT_PLACEHOLDER

%{
    static AST* decode_placeholder(const char *);
%}

/*!endif*/
/*!if GRAMMAR_RULES*/

statement : STATEMENT_PLACEHOLDER
{
    // The lexer ensures this has the following form
    // @STATEMENT-PH::0x1234abcd@, where the pointer coded
    // is an 'AST*'
    AST *tree = decode_placeholder($1.token_text);

    // This is an empty statement
    $$ = *tree = ASTMake1(AST_DECLARATION_STATEMENT,
            ASTLeaf(AST_EMPTY_DECL, $1.token_line, $1.token_text), 
            $1.token_line, NULL);
};

/*!endif*/

// This is code

/*!if GRAMMAR_CODE*/
#define TOK_SEPARATOR "::"
static AST* decode_placeholder(const char *c)
{
    const char * colons = strstr(c, TOK_SEPARATOR);

    if (colons == NULL)
    {
        internal_error("Invalid placeholder token", 0);
    }

    colons += strlen(TOK_SEPARATOR);

    AST *tree = NULL;
    sscanf(colons, "%p", &tree);

    if (tree == NULL)
    {
        internal_error("Invalid AST* reference", 0);
    }

    return tree;
}
/*!endif*/
