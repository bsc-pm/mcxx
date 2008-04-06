/*!if GRAMMAR_PROLOGUE*/
// OpenMP 2.5 tokens
%token<token_atrib> OMP_ATOMIC
%token<token_atrib> OMP_BARRIER
%token<token_atrib> OMP_COPYIN
%token<token_atrib> OMP_COPYPRIVATE
%token<token_atrib> OMP_CRITICAL
%token<token_atrib> OMP_DEFAULT
%token<token_atrib> OMP_DEFAULT_CUSTOM
%token<token_atrib> OMP_DYNAMIC
%token<token_atrib> OMP_FIRSTPRIVATE
%token<token_atrib> OMP_FLUSH
%token<token_atrib> OMP_FOR
%token<token_atrib> OMP_GUIDED
%token<token_atrib> OMP_IF
%token<token_atrib> OMP_LASTPRIVATE
%token<token_atrib> OMP_MASTER
%token<token_atrib> OMP_NEWLINE
%token<token_atrib> OMP_NONE
%token<token_atrib> OMP_NOWAIT
%token<token_atrib> OMP_NUM_THREADS
%token<token_atrib> OMP_ORDERED
%token<token_atrib> OMP_PARALLEL
%token<token_atrib> OMP_PARALLEL_FOR
%token<token_atrib> OMP_PARALLEL_SECTIONS
%token<token_atrib> OMP_PARALLEL_SINGLE
%token<token_atrib> OMP_PRAGMA
%token<token_atrib> OMP_PRIVATE
%token<token_atrib> OMP_REDUCTION
%token<token_atrib> OMP_RUNTIME
%token<token_atrib> OMP_SCHEDULE
%token<token_atrib> OMP_SCHEDULE_CUSTOM
%token<token_atrib> OMP_SECTION
%token<token_atrib> OMP_SECTIONS
%token<token_atrib> OMP_SHARED
%token<token_atrib> OMP_SINGLE
%token<token_atrib> OMP_STATIC
%token<token_atrib> OMP_THREADPRIVATE
%token<token_atrib> OMP_CUSTOM_CLAUSE
%token<token_atrib> OMP_CUSTOM_DIRECTIVE
%token<token_atrib> OMP_CUSTOM_CONSTRUCT

// OpenMP 2.5 semantic values
%type<ast> openmp_construct
%type<ast> openmp_directive
%type<ast> parallel_construct
%type<ast> for_construct
%type<ast> sections_construct
%type<ast> single_construct
%type<ast> parallel_for_construct
%type<ast> parallel_sections_construct
%type<ast> master_construct
%type<ast> critical_construct
%type<ast> atomic_construct
%type<ast> ordered_construct
%type<ast> barrier_directive
%type<ast> flush_directive
%type<ast> structured_block

%type<ast> parallel_directive
%type<ast> parallel_clause_seq_opt
%type<ast> parallel_clause_seq
%type<ast> parallel_clause
%type<ast> unique_parallel_clause

%type<ast> for_directive
%type<ast> for_clause_opt_seq
%type<ast> for_clause_seq
%type<ast> for_clause
%type<ast> unique_for_clause

%type<ast> sections_directive
%type<ast> sections_clause_opt_seq
%type<ast> sections_clause_seq
%type<ast> sections_clause

%type<ast> section_scope
%type<ast> section_sequence
%type<ast> section_directive

%type<ast> single_directive
%type<ast> single_clause_opt_seq
%type<ast> single_clause_seq
%type<ast> single_clause

%type<ast> parallel_for_directive
%type<ast> parallel_for_clause_opt_seq
%type<ast> parallel_for_clause_seq
%type<ast> parallel_for_clause

%type<ast> parallel_sections_directive
%type<ast> parallel_sections_clause_opt_seq
%type<ast> parallel_sections_clause_seq
%type<ast> parallel_sections_clause

%type<ast> atomic_directive

%type<ast> master_directive

%type<ast> critical_directive
%type<ast> region_phrase_opt
%type<ast> region_phrase

%type<ast> flush_vars_opt
%type<ast> flush_vars

%type<ast> ordered_directive

%type<ast> threadprivate_directive

%type<ast> schedule_kind

%type<ast> data_clause

%type<ast> nowait_clause

%type<ast> variable_list

%type<ast> reduction_operator

%type<ast> user_defined_reduction

%type<ast> omp_custom_construct_statement
%type<ast> omp_custom_construct_declaration
%type<ast> omp_custom_directive
%type<ast> omp_custom_clause_opt_seq
%type<ast> omp_custom_clause_seq
%type<ast> omp_custom_parameter_clause
%type<ast> omp_custom_clause
%type<ast> omp_custom_construct_line
/*!endif*/
/*!if GRAMMAR_RULES*/
/* OpenMP 2.5 grammar rules */

// Grammar entry point
no_if_statement : openmp_construct
{
    $$ = $1;
}
;

statement_seq : openmp_directive
{
	$$ = ASTListLeaf($1);
}
| statement_seq openmp_directive
{
	$$ = ASTList($1, $2);
}
;

declaration : threadprivate_directive
{
	$$ = $1;
}
| omp_custom_directive
{
	$$ = $1;
}
| omp_custom_construct_declaration
{
    $$ = $1;
}
;

// OpenMP
openmp_construct : parallel_construct
{
	$$ = $1;
}
| for_construct
{
	$$ = $1;
}
| sections_construct
{
	$$ = $1;
}
| single_construct
{
	$$ = $1;
}
| parallel_for_construct
{
	$$ = $1;
}
| parallel_sections_construct
{
	$$ = $1;
}
| master_construct
{
	$$ = $1;
}
| critical_construct
{
	$$ = $1;
}
| atomic_construct
{
	$$ = $1;
}
| ordered_construct
{
	$$ = $1;
}
// There is a bug in the specification of OpenMP 2.5, a threadprivate directive
// really CAN appear here even if in the grammar spec does not
| threadprivate_directive
{
	$$ = $1;
}
| omp_custom_directive
{
	$$ = $1;
}
| omp_custom_construct_statement
{
	$$ = $1;
}
;

openmp_directive : barrier_directive
{
	$$ = $1;
}
| flush_directive
{
	$$ = $1;
}
;

// Custom OpenMP support
omp_custom_directive : OMP_PRAGMA OMP_CUSTOM_DIRECTIVE omp_custom_clause_opt_seq OMP_NEWLINE
{
 	$$ = ASTMake1(AST_OMP_CUSTOM_DIRECTIVE, $3, $1.token_line, $2.token_text);
}
;

omp_custom_construct_line : OMP_PRAGMA OMP_CUSTOM_CONSTRUCT omp_custom_clause_opt_seq OMP_NEWLINE
{
 	$$ = ASTMake1(AST_OMP_CUSTOM_CONSTRUCT_DIRECTIVE, $3, $1.token_line, $2.token_text);
}
;

omp_custom_construct_statement : omp_custom_construct_line structured_block
{
    $$ = ASTMake2(AST_OMP_CUSTOM_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

omp_custom_construct_declaration : omp_custom_construct_line declaration
{
    $$ = ASTMake2(AST_OMP_CUSTOM_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

omp_custom_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| omp_custom_clause_seq
{
	$$ = $1;
}
;

// I think this is the more general
omp_custom_clause_seq : parallel_for_clause
{
	$$ = ASTListLeaf($1);
}
| omp_custom_parameter_clause
{
	$$ = ASTListLeaf($1);
}
| omp_custom_clause_seq parallel_for_clause
{
	$$ = ASTList($1, $2);
}
;

omp_custom_parameter_clause : '(' expression_list ')'
{
	$$ = ASTMake1(AST_OMP_CUSTOM_PARAMETER_CLAUSE, $2, $1.token_line, NULL);
}
;

omp_custom_clause : OMP_CUSTOM_CLAUSE '(' expression_list ')'
{
	$$ = ASTMake1(AST_OMP_CUSTOM_CLAUSE, $3, $1.token_line, $1.token_text);
}
| OMP_CUSTOM_CLAUSE '(' ')'
{
	$$ = ASTMake1(AST_OMP_CUSTOM_CLAUSE, NULL, $1.token_line, $1.token_text);
}
| OMP_CUSTOM_CLAUSE
{
	$$ = ASTMake1(AST_OMP_CUSTOM_CLAUSE, NULL, $1.token_line, $1.token_text);
}
;

// End of custom support

structured_block : statement
{
	$$ = $1;
}
;

parallel_construct : parallel_directive structured_block
{
	$$ = ASTMake2(AST_OMP_PARALLEL_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

parallel_directive : OMP_PRAGMA OMP_PARALLEL parallel_clause_seq_opt OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_PARALLEL_DIRECTIVE, $3, $1.token_line, NULL);
}
;

parallel_clause_seq_opt : /* empty */
{
	$$ = NULL;
}
| parallel_clause_seq
{
	$$ = $1;
}
;

parallel_clause_seq : parallel_clause
{
	$$ = ASTListLeaf($1);
}
| parallel_clause_seq parallel_clause
{
	$$ = ASTList($1, $2);
}
| parallel_clause_seq ',' parallel_clause
{
	$$ = ASTList($1, $3);
}
;

parallel_clause : unique_parallel_clause
{
	$$ = $1;
}
| data_clause
{
	$$ = $1;
}
| omp_custom_clause
{
	$$ = $1;
}
;

unique_parallel_clause : OMP_IF '(' expression ')' 
{
	$$ = ASTMake1(AST_OMP_IF_CLAUSE, $3, $1.token_line, NULL);
}
| OMP_NUM_THREADS '(' expression ')'
{
	$$ = ASTMake1(AST_OMP_NUM_THREADS_CLAUSE, $3, $1.token_line, NULL);
}
;

for_construct : for_directive iteration_statement
{
	$$ = ASTMake2(AST_OMP_FOR_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

for_directive : OMP_PRAGMA OMP_FOR for_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_FOR_DIRECTIVE, $3, $1.token_line, NULL);
}
;

for_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| for_clause_seq
{
	$$ = $1;
}
;

for_clause_seq : for_clause
{
	$$ = ASTListLeaf($1);
}
| for_clause_seq ',' for_clause
{
	$$ = ASTList($1, $3);
}
| for_clause_seq for_clause
{
	$$ = ASTList($1, $2);
}
;

for_clause : unique_for_clause 
{
	$$ = $1;
}
| data_clause
{
	$$ = $1
}
| nowait_clause
{
	$$ = $1;
}
| omp_custom_clause
{
	$$ = $1;
}
;

unique_for_clause : OMP_ORDERED
{
	$$ = ASTLeaf(AST_OMP_ORDERED_CLAUSE, $1.token_line, NULL);
}
| OMP_SCHEDULE '(' schedule_kind ')'
{
	$$ = ASTMake2(AST_OMP_SCHEDULE_CLAUSE, $3, NULL, $1.token_line, NULL);
}
| OMP_SCHEDULE '(' schedule_kind ',' expression ')'
{
	$$ = ASTMake2(AST_OMP_SCHEDULE_CLAUSE, $3, $5, $1.token_line, NULL);
}
;

schedule_kind : OMP_STATIC
{
	$$ = ASTLeaf(AST_OMP_STATIC_SCHEDULE, $1.token_line, NULL);
}
| OMP_DYNAMIC
{
	$$ = ASTLeaf(AST_OMP_DYNAMIC_SCHEDULE, $1.token_line, NULL);
}
| OMP_GUIDED
{
	$$ = ASTLeaf(AST_OMP_GUIDED_SCHEDULE, $1.token_line, NULL);
}
| OMP_RUNTIME
{
	$$ = ASTLeaf(AST_OMP_RUNTIME_SCHEDULE, $1.token_line, NULL);
}
| OMP_SCHEDULE_CUSTOM
{
    $$ = ASTLeaf(AST_OMP_CUSTOM_SCHEDULE, $1.token_line, $1.token_text);
}
;

sections_construct : sections_directive section_scope
{
	$$ = ASTMake2(AST_OMP_SECTIONS_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

sections_directive : OMP_PRAGMA OMP_SECTIONS sections_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_SECTIONS_DIRECTIVE, $3, $1.token_line, NULL);
}
;

sections_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| sections_clause_seq
{
	$$ = $1;
}
;

sections_clause_seq : sections_clause
{
	$$ = ASTListLeaf($1);
}
| sections_clause_seq sections_clause
{
	$$ = ASTList($1, $2);
}
| sections_clause_seq ',' sections_clause
{
	$$ = ASTList($1, $3);
}
;

sections_clause : data_clause
{
	$$ = $1;
}
| nowait_clause
{
	$$ = $1;
}
| omp_custom_clause
{
}
;

nowait_clause :  OMP_NOWAIT
{
	$$ = ASTLeaf(AST_OMP_NOWAIT_CLAUSE, $1.token_line, NULL);
}
;

section_scope : '{' section_sequence '}'
{
	$$ = $2;
}
;

section_sequence : section_directive structured_block
{
    AST section_holder = ASTMake2(AST_OMP_SECTION, $1, $2, ASTLine($1), NULL);
	$$ = ASTListLeaf(section_holder);
}
| structured_block
{
    AST section_holder = ASTMake2(AST_OMP_SECTION, NULL, $1, ASTLine($1), NULL);
	$$ = ASTListLeaf(section_holder);
}
| section_sequence section_directive structured_block
{
    AST section_holder = ASTMake2(AST_OMP_SECTION, $2, $3, ASTLine($2), NULL);
	$$ = ASTList($1, section_holder);
}
;

section_directive : OMP_PRAGMA OMP_SECTION OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_SECTION_DIRECTIVE, $1.token_line, NULL);
}
;

single_construct : single_directive structured_block
{
	$$ = ASTMake2(AST_OMP_SINGLE_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

single_directive : OMP_PRAGMA OMP_SINGLE single_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_SINGLE_DIRECTIVE, $3, $1.token_line, NULL);
}
;

single_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| single_clause_seq
{
	$$ = $1;
}
;

single_clause_seq : single_clause
{
	$$ = ASTListLeaf($1);
}
| single_clause_seq ',' single_clause
{
	$$ = ASTList($1, $3);
}
| single_clause_seq single_clause
{
	$$ = ASTList($1, $2);
}
;

single_clause : data_clause
{
	$$ = $1;
}
| nowait_clause
{
	$$ = $1;
}
| omp_custom_clause
{
	$$ = $1;
}
;

parallel_for_construct : parallel_for_directive iteration_statement
{
	$$ = ASTMake2(AST_OMP_PARALLEL_FOR_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

parallel_for_directive : OMP_PRAGMA OMP_PARALLEL_FOR parallel_for_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_PARALLEL_FOR_DIRECTIVE, $3, $1.token_line, NULL);
}
;

parallel_for_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| parallel_for_clause_seq 
{
	$$ = $1;
}
;

parallel_for_clause_seq : parallel_for_clause
{
	$$ = ASTListLeaf($1);
}
| parallel_for_clause_seq ',' parallel_for_clause
{
	$$ = ASTList($1, $3);
}
| parallel_for_clause_seq parallel_for_clause
{
	$$ = ASTList($1, $2);
}
;

parallel_for_clause : unique_parallel_clause
{
	$$ = $1;
}
| unique_for_clause
{
	$$ = $1;
}
| data_clause
{
	$$ = $1;
}
| omp_custom_clause
{
	$$ = $1;
}
;

parallel_sections_construct : parallel_sections_directive section_scope
{
	$$ = ASTMake2(AST_OMP_PARALLEL_SECTIONS_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

parallel_sections_directive : OMP_PRAGMA OMP_PARALLEL_SECTIONS parallel_sections_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_PARALLEL_SECTIONS_DIRECTIVE, $3, $1.token_line, NULL);
}
;

parallel_sections_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| parallel_sections_clause_seq
{
	$$ = $1;
}
;

parallel_sections_clause_seq : parallel_sections_clause
{
	$$ = ASTListLeaf($1);
}
| parallel_sections_clause_seq ',' parallel_sections_clause
{
	$$ = ASTList($1, $3);
}
| parallel_sections_clause_seq parallel_sections_clause
{
	$$ = ASTList($1, $2);
}
;

parallel_sections_clause : unique_parallel_clause
{
	$$ = $1;
}
| data_clause
{
	$$ = $1;
}
| omp_custom_clause
{
	$$ = $1;
}
;

master_construct : master_directive structured_block
{
	$$ = ASTMake2(AST_OMP_MASTER_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

master_directive : OMP_PRAGMA OMP_MASTER OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_MASTER_DIRECTIVE, $1.token_line, NULL);
}
;

critical_construct : critical_directive structured_block
{
	$$ = ASTMake2(AST_OMP_CRITICAL_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

critical_directive : OMP_PRAGMA OMP_CRITICAL region_phrase_opt OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_CRITICAL_DIRECTIVE, $3, $1.token_line, NULL);
}
;

region_phrase_opt : /* empty */
{
	$$ = NULL;
}
| region_phrase
{
	$$ = $1;
}
;

region_phrase : '(' IDENTIFIER ')'
{
	// Cast it into an expression, makes things a lot easier
	AST critical_region_phrase = ASTLeaf(AST_SYMBOL, $2.token_line, $2.token_text);

	$$ = ASTMake1(AST_EXPRESSION, critical_region_phrase, $1.token_line, NULL);
}
;

barrier_directive : OMP_PRAGMA OMP_BARRIER OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_BARRIER_DIRECTIVE, $1.token_line, NULL);
}
;

atomic_construct : atomic_directive expression_statement
{
	$$ = ASTMake2(AST_OMP_ATOMIC_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

atomic_directive : OMP_PRAGMA OMP_ATOMIC OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_ATOMIC_DIRECTIVE, $1.token_line, NULL);
}
;

flush_directive : OMP_PRAGMA OMP_FLUSH flush_vars_opt OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_FLUSH_DIRECTIVE, $3, $1.token_line, NULL);
}
;

flush_vars_opt : /* empty */
{
	$$ = NULL;
}
| flush_vars
{
	$$ = $1;
}
;

flush_vars : '(' variable_list ')'
{
	$$ = $2;
}
;

ordered_construct : ordered_directive structured_block
{
	$$ = ASTMake2(AST_OMP_ORDERED_CONSTRUCT, $1, $2, ASTLine($1), NULL);
}
;

ordered_directive : OMP_PRAGMA OMP_ORDERED OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_ORDERED_DIRECTIVE, $1.token_line, NULL);
}
;

threadprivate_directive : OMP_PRAGMA OMP_THREADPRIVATE '(' variable_list ')' OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_THREADPRIVATE_DIRECTIVE, $4, $1.token_line, NULL);
}
;

data_clause : OMP_PRIVATE '(' variable_list ')'
{
	$$ = ASTMake1(AST_OMP_PRIVATE_CLAUSE, $3, $1.token_line, NULL);
}
| OMP_COPYPRIVATE '(' variable_list ')'
{
	$$ = ASTMake1(AST_OMP_COPYPRIVATE_CLAUSE, $3, $1.token_line, NULL);
}
| OMP_FIRSTPRIVATE '(' variable_list ')'
{
	$$ = ASTMake1(AST_OMP_FIRSTPRIVATE_CLAUSE, $3, $1.token_line, NULL);
}
| OMP_LASTPRIVATE '(' variable_list ')'
{
	$$ = ASTMake1(AST_OMP_LASTPRIVATE_CLAUSE, $3, $1.token_line, NULL);
}
| OMP_SHARED '(' variable_list ')'
{
	$$ = ASTMake1(AST_OMP_SHARED_CLAUSE, $3, $1.token_line, NULL);
}
| OMP_DEFAULT '(' OMP_SHARED ')'
{
	$$ = ASTLeaf(AST_OMP_DEFAULT_SHARED_CLAUSE, $1.token_line, NULL);
}
| OMP_DEFAULT '(' OMP_NONE ')'
{
	$$ = ASTLeaf(AST_OMP_DEFAULT_NONE_CLAUSE, $1.token_line, NULL);
}
| OMP_DEFAULT '(' OMP_DEFAULT_CUSTOM ')'
{
    $$ = ASTLeaf(AST_OMP_DEFAULT_CUSTOM_CLAUSE, $1.token_line, $3.token_text);
}
| OMP_REDUCTION '(' reduction_operator ':' variable_list ')'
{
	$$ = ASTMake2(AST_OMP_REDUCTION_CLAUSE, 
			$3, $5, $1.token_line, NULL);
}
| user_defined_reduction
{
	$$ = $1;
}
| OMP_COPYIN '(' variable_list ')'
{
	$$ = ASTMake1(AST_OMP_COPYIN_CLAUSE, $3, $1.token_line, NULL);
}
;

reduction_operator : '+' 
{
	$$ = ASTLeaf(AST_ADD_OPERATOR, $1.token_line, NULL);
}
| '*'
{
	$$ = ASTLeaf(AST_MULT_OPERATOR, $1.token_line, NULL);
}
| '-'
{
	$$ = ASTLeaf(AST_MINUS_OPERATOR, $1.token_line, NULL);
}
| '&'
{
	$$ = ASTLeaf(AST_BITWISE_AND_OPERATOR, $1.token_line, NULL);
}
| '^'
{
	$$ = ASTLeaf(AST_BITWISE_XOR_OPERATOR, $1.token_line, NULL);
}
| '|'
{
	$$ = ASTLeaf(AST_BITWISE_OR_OPERATOR, $1.token_line, NULL);
}
| ANDAND
{
	$$ = ASTLeaf(AST_LOGICAL_AND_OPERATOR, $1.token_line, NULL);
}
| OROR
{
	$$ = ASTLeaf(AST_LOGICAL_OR_OPERATOR, $1.token_line, NULL);
}
;

variable_list : id_expression
{
	$$ = ASTListLeaf($1);
}
| variable_list ',' id_expression
{
	$$ = ASTList($1, $3);
}
;

user_defined_reduction : OMP_REDUCTION '(' id_expression ',' constant_expression ':' variable_list ')'
{
	$$ = ASTMake3(AST_OMP_USER_DEFINED_REDUCTION_CLAUSE, 
			$3, $5, $7, $1.token_line, NULL);
}
| OMP_REDUCTION '(' reduction_operator ',' constant_expression ':' variable_list ')'
{
	$$ = ASTMake3(AST_OMP_USER_DEFINED_REDUCTION_CLAUSE, 
			$3, $5, $7, $1.token_line, NULL);
}
;

/*!endif*/
