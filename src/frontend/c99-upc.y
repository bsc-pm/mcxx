/*!if GRAMMAR_PROLOGUE */
%token<token_atrib> UPC_MYTHREAD
%token<token_atrib> UPC_RELAXED
%token<token_atrib> UPC_SHARED
%token<token_atrib> UPC_STRICT
%token<token_atrib> UPC_THREADS
%token<token_atrib> UPC_BARRIER
%token<token_atrib> UPC_BLOCKSIZEOF
%token<token_atrib> UPC_ELEMSIZEOF
%token<token_atrib> UPC_FENCE
%token<token_atrib> UPC_FORALL
%token<token_atrib> UPC_LOCALSIZEOF
%token<token_atrib> UPC_MAX_BLOCKSIZE
%token<token_atrib> UPC_NOTIFY
%token<token_atrib> UPC_WAIT

%type<ast> upc_shared_type_qualifier
%type<ast> upc_reference_type_qualifier
%type<ast> upc_layout_qualifier
%type<ast> upc_synchronization_statement
%type<ast> upc_expression_opt
%type<ast> upc_affinity_opt
%type<ast> upc_affinity
/*!endif*/
/*!if GRAMMAR_RULES*/
unary_expression : UPC_LOCALSIZEOF unary_expression
{
}
| UPC_LOCALSIZEOF '(' type_id ')'
{
}
| UPC_BLOCKSIZEOF unary_expression
{
}
| UPC_BLOCKSIZEOF '(' type_id ')'
{
}
| UPC_ELEMSIZEOF unary_expression
{
}
| UPC_ELEMSIZEOF '(' type_id ')'
{
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
}
| UPC_SHARED upc_layout_qualifier
{
}
;

upc_reference_type_qualifier : UPC_RELAXED
{
}
| UPC_STRICT
{
}
;

upc_layout_qualifier : '[' ']'
{
}
| '[' constant_expression ']'
{
}
| '[' '*' ']'
{
}
;

no_if_statement : upc_synchronization_statement
{
}
;

upc_synchronization_statement : UPC_NOTIFY upc_expression_opt ';'
{
}
| UPC_WAIT upc_expression_opt ';'
{
}
| UPC_BARRIER upc_expression_opt ';'
{
}
| UPC_FENCE ';'
{
}
;

upc_expression_opt : expression
{
}
|
{
}
;

iteration_statement : UPC_FORALL '(' upc_expression_opt ';' upc_expression_opt ';' upc_expression_opt ';' upc_affinity_opt ')' statement
{
}
| UPC_FORALL '(' simple_declaration upc_expression_opt ';' upc_expression_opt ';' upc_affinity_opt ')' statement
{
}
;

upc_affinity_opt : upc_affinity
{
}
| 
{
};

upc_affinity : expression
{
}
| CONTINUE
{
}
;

/*!endif*/
