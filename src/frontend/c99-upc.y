/*!if GRAMMAR_PROLOGUE */
%token<token_atrib> UPC_MYTHREAD "MYTHREAD (UPC)" 
%token<token_atrib> UPC_RELAXED "relaxed (UPC)"
%token<token_atrib> UPC_SHARED "shared (UPC)"
%token<token_atrib> UPC_STRICT "strict (UPC)"
%token<token_atrib> UPC_THREADS "THREADS (UPC)"
%token<token_atrib> UPC_BARRIER "upc_barrier"
%token<token_atrib> UPC_BLOCKSIZEOF "upc_blocksizeof"
%token<token_atrib> UPC_ELEMSIZEOF "upc_elemsizeof"
%token<token_atrib> UPC_FENCE "upc_fence"
%token<token_atrib> UPC_FORALL "upc_forall"
%token<token_atrib> UPC_LOCALSIZEOF "upc_localsizeof"
%token<token_atrib> UPC_MAX_BLOCKSIZE "UPC_MAX_BLOCKSIZE"
%token<token_atrib> UPC_NOTIFY "upc_notify"
%token<token_atrib> UPC_WAIT "upc_wait"

%type<ast> upc_shared_type_qualifier
%type<ast> upc_reference_type_qualifier
%type<ast> upc_layout_qualifier
%type<ast> upc_layout_qualifier_element
%type<ast> upc_synchronization_statement
%type<ast> upc_expression_opt
%type<ast> upc_affinity_opt
%type<ast> upc_affinity
/*!endif*/
/*!if GRAMMAR_RULES*/
unary_expression : UPC_LOCALSIZEOF unary_expression
{
    $$ = ASTMake1(AST_UPC_LOCALSIZEOF, $2, $1.token_file, $1.token_line, NULL);
}
| UPC_LOCALSIZEOF '(' type_id ')'
{
    $$ = ASTMake1(AST_UPC_LOCALSIZEOF_TYPEID, $3, $1.token_file, $1.token_line, NULL);
}
| UPC_BLOCKSIZEOF unary_expression
{
    $$ = ASTMake1(AST_UPC_BLOCKSIZEOF, $2, $1.token_file, $1.token_line, NULL);
}
| UPC_BLOCKSIZEOF '(' type_id ')'
{
    $$ = ASTMake1(AST_UPC_BLOCKSIZEOF_TYPEID, $3, $1.token_file, $1.token_line, NULL);
}
| UPC_ELEMSIZEOF unary_expression
{
    $$ = ASTMake1(AST_UPC_ELEMSIZEOF, $2, $1.token_file, $1.token_line, NULL);
}
| UPC_ELEMSIZEOF '(' type_id ')'
{
    $$ = ASTMake1(AST_UPC_ELEMSIZEOF_TYPEID, $3, $1.token_file, $1.token_line, NULL);
}
;

cv_qualifier : upc_shared_type_qualifier
{
    $$ = $1;
}
| upc_reference_type_qualifier
{
    $$ = $1;
}
;

upc_shared_type_qualifier : UPC_SHARED
{
    $$ = ASTMake1(AST_UPC_SHARED, NULL, $1.token_file, $1.token_line, NULL);
}
| UPC_SHARED upc_layout_qualifier
{
    $$ = ASTMake1(AST_UPC_SHARED, $2, $1.token_file, $1.token_line, NULL);
}
;

upc_reference_type_qualifier : UPC_RELAXED
{
    $$ = ASTLeaf(AST_UPC_RELAXED, $1.token_file, $1.token_line, NULL);
}
| UPC_STRICT
{
    $$ = ASTLeaf(AST_UPC_STRICT, $1.token_file, $1.token_line, NULL);
}
;

// UPC only allows one of these qualifiers but as an extension we allow a list
upc_layout_qualifier: upc_layout_qualifier_element
{
    $$ = ASTListLeaf($1);
}
| upc_layout_qualifier upc_layout_qualifier_element
{
    $$ = ASTList($1, $2);
}
;

upc_layout_qualifier_element : '[' ']'
{
    $$ = ASTMake1(AST_UPC_LAYOUT_QUALIFIER, NULL, $1.token_file, $1.token_line, NULL);
}
| '[' constant_expression ']'
{
    $$ = ASTMake1(AST_UPC_LAYOUT_QUALIFIER, $2, $1.token_file, $1.token_line, NULL);
}
| '[' '*' ']'
{
    $$ = ASTMake1(AST_UPC_LAYOUT_QUALIFIER, 
            ASTLeaf(AST_UPC_LAYOUT_UNDEF, $2.token_file, $2.token_line, NULL), 
            $1.token_file, $1.token_line, NULL);
}
;

no_if_statement : upc_synchronization_statement
{
    $$ = $1;
}
;

upc_synchronization_statement : UPC_NOTIFY upc_expression_opt ';'
{
    $$ = ASTMake1(AST_UPC_NOTIFY, $2, $1.token_file, $1.token_line, NULL);
}
| UPC_WAIT upc_expression_opt ';'
{
    $$ = ASTMake1(AST_UPC_WAIT, $2, $1.token_file, $1.token_line, NULL);
}
| UPC_BARRIER upc_expression_opt ';'
{
    $$ = ASTMake1(AST_UPC_BARRIER, $2, $1.token_file, $1.token_line, NULL);
}
| UPC_FENCE ';'
{
    $$ = ASTLeaf(AST_UPC_FENCE, $1.token_file, $1.token_line, NULL);
}
;

upc_expression_opt : expression
{
    $$ = $1;
}
|
{
    $$ = NULL;
}
;

iteration_statement : UPC_FORALL '(' for_init_statement upc_expression_opt ';' upc_expression_opt ';' upc_affinity_opt ')' statement
{
    AST upc_forall_header =
        ASTMake4(AST_UPC_FORALL_HEADER, $3, $4, $6, $8, $1.token_file, $1.token_line, NULL);

    $$ = ASTMake2(AST_UPC_FORALL, upc_forall_header, $10, $1.token_file, $1.token_line, NULL);
}
;

upc_affinity_opt : upc_affinity
{
    $$ = $1;
}
| 
{
    $$ = NULL;
};

upc_affinity : expression
{
    $$ = $1;
}
| CONTINUE
{
    $$ = ASTLeaf(AST_UPC_CONTINUE, $1.token_file, $1.token_line, NULL);
}
;

/*!endif*/
