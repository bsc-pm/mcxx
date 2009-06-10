/*!if GRAMMAR_PROLOGUE*/
// OpenMP 3.0 tokens
%token<token_atrib> OMP_ATOMIC "atomic (OpenMP)"
%token<token_atrib> OMP_BARRIER "barrier (OpenMP)"
%token<token_atrib> OMP_COPYIN "copyin (OpenMP)"
%token<token_atrib> OMP_COPYPRIVATE "copyprivate (OpenMP)"
%token<token_atrib> OMP_CRITICAL "critical (OpenMP)"
%token<token_atrib> OMP_DEFAULT "default (OpenMP)"
%token<token_atrib> OMP_DEFAULT_CUSTOM "default_custom (OpenMP)"
%token<token_atrib> OMP_DYNAMIC "dynamic (OpenMP)"
%token<token_atrib> OMP_FIRSTPRIVATE "firstprivate (OpenMP)"
%token<token_atrib> OMP_FLUSH "flush (OpenMP)"
%token<token_atrib> OMP_FOR "for (OpenMP)"
%token<token_atrib> OMP_GUIDED "guided (OpenMP)"
%token<token_atrib> OMP_IF "if (OpenMP)"
%token<token_atrib> OMP_LASTPRIVATE "lastprivate (OpenMP)"
%token<token_atrib> OMP_MASTER "master (OpenMP)"
%token<token_atrib> OMP_NEWLINE "newline (OpenMP)"
%token<token_atrib> OMP_NONE "none (OpenMP)"
%token<token_atrib> OMP_NOWAIT "nowait (OpenMP)"
%token<token_atrib> OMP_NUM_THREADS "num_threads (OpenMP)"
%token<token_atrib> OMP_ORDERED "ordered (OpenMP)"
%token<token_atrib> OMP_PARALLEL "parallel (OpenMP)"
%token<token_atrib> OMP_PARALLEL_FOR "parallel_for (OpenMP)"
%token<token_atrib> OMP_PARALLEL_SECTIONS "parallel_sections (OpenMP)"
%token<token_atrib> OMP_PARALLEL_SINGLE "parallel_single (OpenMP)"
%token<token_atrib> OMP_PRAGMA "pragma (OpenMP)"
%token<token_atrib> OMP_PRIVATE "private (OpenMP)"
%token<token_atrib> OMP_REDUCTION "reduction (OpenMP)"
%token<token_atrib> OMP_RUNTIME "runtime (OpenMP)"
%token<token_atrib> OMP_SCHEDULE "schedule (OpenMP)"
%token<token_atrib> OMP_SCHEDULE_CUSTOM "schedule_custom (OpenMP)"
%token<token_atrib> OMP_SECTION "section (OpenMP)"
%token<token_atrib> OMP_SECTIONS "sections (OpenMP)"
%token<token_atrib> OMP_SHARED "shared (OpenMP)"
%token<token_atrib> OMP_SINGLE "single (OpenMP)"
%token<token_atrib> OMP_STATIC "static (OpenMP)"
%token<token_atrib> OMP_THREADPRIVATE "threadprivate (OpenMP)"
%token<token_atrib> OMP_CUSTOM_CLAUSE "<custom-clause> (OpenMP)"
%token<token_atrib> OMP_CUSTOM_DIRECTIVE "<custom-directive> (OpenMP)"
%token<token_atrib> OMP_CUSTOM_CONSTRUCT "<custom-construct> (OpenMP)"
%token<token_atrib> OMP_TASK "task (OpenMP)"
%token<token_atrib> OMP_TASKWAIT "taskwait (OpenMP)"

%token<token_atrib> OMP_DECLARE "declare (OpenMP)"
%token<token_atrib> OMP_IDENTITY "identity (OpenMP)"
%token<token_atrib> OMP_CONSTRUCTOR "constructor (OpenMP)"
%token<token_atrib> OMP_ORDER "order (OpenMP)"
%token<token_atrib> OMP_COMMUTATIVE "commutative (OpenMP)"
%token<token_atrib> OMP_TYPE "type (OpenMP)"
%token<token_atrib> OMP_OPERATOR "operator (OpenMP)"
%token<token_atrib> OMP_REDUCTION_RIGHT "right (OpenMP)"
%token<token_atrib> OMP_REDUCTION_LEFT "left (OpenMP)"

// OpenMP 3.0 semantic values
%type<ast> omp_construct
%type<ast> omp_directive
%type<ast> omp_parallel_construct
%type<ast> omp_for_construct
%type<ast> omp_sections_construct
%type<ast> omp_single_construct
%type<ast> omp_parallel_for_construct
%type<ast> omp_parallel_sections_construct
%type<ast> omp_master_construct
%type<ast> omp_critical_construct
%type<ast> omp_atomic_construct
%type<ast> omp_ordered_construct
%type<ast> omp_barrier_directive
%type<ast> omp_flush_directive
%type<ast> omp_structured_block

%type<ast> omp_parallel_directive
%type<ast> omp_parallel_clause_seq_opt
%type<ast> omp_parallel_clause_seq
%type<ast> omp_parallel_clause
%type<ast> omp_unique_parallel_clause

%type<ast> omp_for_directive
%type<ast> omp_for_clause_opt_seq
%type<ast> omp_for_clause_seq
%type<ast> omp_for_clause
%type<ast> omp_unique_for_clause

%type<ast> omp_sections_directive
%type<ast> omp_sections_clause_opt_seq
%type<ast> omp_sections_clause_seq
%type<ast> omp_sections_clause

%type<ast> omp_task_construct
%type<ast> omp_task_directive
%type<ast> omp_task_clause_seq_opt
%type<ast> omp_task_clause_seq
%type<ast> omp_task_clause
%type<ast> omp_unique_task_clause

%type<ast> omp_taskwait_directive

%type<ast> omp_section_scope
%type<ast> omp_section_sequence
%type<ast> omp_section_directive

%type<ast> omp_single_directive
%type<ast> omp_single_clause_opt_seq
%type<ast> omp_single_clause_seq
%type<ast> omp_single_clause

%type<ast> omp_parallel_for_directive
%type<ast> omp_parallel_for_clause_opt_seq
%type<ast> omp_parallel_for_clause_seq
%type<ast> omp_parallel_for_clause

%type<ast> omp_parallel_sections_directive
%type<ast> omp_parallel_sections_clause_opt_seq
%type<ast> omp_parallel_sections_clause_seq
%type<ast> omp_parallel_sections_clause

%type<ast> omp_atomic_directive

%type<ast> omp_master_directive

%type<ast> omp_critical_directive
%type<ast> omp_region_phrase_opt
%type<ast> omp_region_phrase

%type<ast> omp_flush_vars_opt
%type<ast> omp_flush_vars

%type<ast> omp_ordered_directive

%type<ast> omp_threadprivate_directive

%type<ast> omp_schedule_kind

%type<ast> omp_data_clause

%type<ast> omp_nowait_clause

%type<ast> omp_variable_list

%type<ast> omp_reduction_operator

%type<ast> omp_custom_construct_statement
%type<ast> omp_custom_construct_declaration
%type<ast> omp_custom_directive
%type<ast> omp_custom_clause_opt_seq
%type<ast> omp_custom_clause_seq
%type<ast> omp_custom_parameter_clause
%type<ast> omp_custom_clause
%type<ast> omp_custom_construct_line

%type<ast> omp_declare_directive
%type<ast> omp_declare_reduction
%type<ast> omp_declare_reduction_clause
%type<ast> omp_declare_reduction_clauses
%type<ast> omp_identity_expression
%type<ast> omp_reduction_order
%type<ast> omp_user_defined_operator

/*!endif*/
/*!if GRAMMAR_RULES*/
/* OpenMP 3.0 grammar rules */

// Grammar entry point
no_if_statement : omp_construct
{
    $$ = $1;
}
;

statement_seq : omp_directive
{
	$$ = ASTListLeaf($1);
}
| statement_seq omp_directive
{
	$$ = ASTList($1, $2);
}
;

declaration : omp_threadprivate_directive
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
| omp_declare_directive
{
    $$ = $1;
}
;

// OpenMP
omp_construct : omp_parallel_construct
{
	$$ = $1;
}
| omp_task_construct
{
    $$ = $1;
}
| omp_for_construct
{
	$$ = $1;
}
| omp_sections_construct
{
	$$ = $1;
}
| omp_single_construct
{
	$$ = $1;
}
| omp_parallel_for_construct
{
	$$ = $1;
}
| omp_parallel_sections_construct
{
	$$ = $1;
}
| omp_master_construct
{
	$$ = $1;
}
| omp_critical_construct
{
	$$ = $1;
}
| omp_atomic_construct
{
	$$ = $1;
}
| omp_ordered_construct
{
	$$ = $1;
}
| omp_taskwait_directive
{
    $$ = $1;
}
// There is a bug in the specification of OpenMP 2.5, a threadprivate directive
// really CAN appear here even if in the grammar spec does not say so
| omp_threadprivate_directive
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

omp_directive : omp_barrier_directive
{
	$$ = $1;
}
| omp_flush_directive
{
	$$ = $1;
}
;

// Custom OpenMP support
omp_custom_directive : OMP_PRAGMA OMP_CUSTOM_DIRECTIVE omp_custom_clause_opt_seq OMP_NEWLINE
{
 	$$ = ASTMake1(AST_OMP_CUSTOM_DIRECTIVE, $3, $1.token_file, $1.token_line, $2.token_text);
}
;

omp_custom_construct_line : OMP_PRAGMA OMP_CUSTOM_CONSTRUCT omp_custom_clause_opt_seq OMP_NEWLINE
{
 	$$ = ASTMake1(AST_OMP_CUSTOM_CONSTRUCT_DIRECTIVE, $3, $1.token_file, $1.token_line, $2.token_text);
}
;

omp_custom_construct_statement : omp_custom_construct_line omp_structured_block
{
    $$ = ASTMake2(AST_OMP_CUSTOM_CONSTRUCT, $1, $2, ASTFileName($1), ASTLine($1), NULL);
}
;

omp_custom_construct_declaration : omp_custom_construct_line declaration
{
    $$ = ASTMake2(AST_OMP_CUSTOM_CONSTRUCT, $1, $2, ASTFileName($1), ASTLine($1), NULL);
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
omp_custom_clause_seq : omp_parallel_for_clause
{
	$$ = ASTListLeaf($1);
}
| omp_custom_parameter_clause
{
	$$ = ASTListLeaf($1);
}
| omp_custom_clause_seq omp_parallel_for_clause
{
	$$ = ASTList($1, $2);
}
;

omp_custom_parameter_clause : '(' expression_list ')'
{
	$$ = ASTMake1(AST_OMP_CUSTOM_PARAMETER_CLAUSE, $2, $1.token_file, $1.token_line, NULL);
}
;

omp_custom_clause : OMP_CUSTOM_CLAUSE '(' expression_list ')'
{
	$$ = ASTMake1(AST_OMP_CUSTOM_CLAUSE, $3, $1.token_file, $1.token_line, $1.token_text);
}
| OMP_CUSTOM_CLAUSE '(' ')'
{
	$$ = ASTMake1(AST_OMP_CUSTOM_CLAUSE, NULL, $1.token_file, $1.token_line, $1.token_text);
}
| OMP_CUSTOM_CLAUSE
{
	$$ = ASTMake1(AST_OMP_CUSTOM_CLAUSE, NULL, $1.token_file, $1.token_line, $1.token_text);
}
;

// End of custom support

omp_structured_block : statement
{
	$$ = $1;
}
;

omp_parallel_construct : omp_parallel_directive omp_structured_block
{
	$$ = ASTMake2(AST_OMP_PARALLEL_CONSTRUCT, $1, $2, ASTFileName($1), ASTLine($1), NULL);
}
;

omp_parallel_directive : OMP_PRAGMA OMP_PARALLEL omp_parallel_clause_seq_opt OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_PARALLEL_DIRECTIVE, $3, $1.token_file, $1.token_line, NULL);
}
;

omp_parallel_clause_seq_opt : /* empty */
{
	$$ = NULL;
}
| omp_parallel_clause_seq
{
	$$ = $1;
}
;

omp_parallel_clause_seq : omp_parallel_clause
{
	$$ = ASTListLeaf($1);
}
| omp_parallel_clause_seq omp_parallel_clause
{
	$$ = ASTList($1, $2);
}
| omp_parallel_clause_seq ',' omp_parallel_clause
{
	$$ = ASTList($1, $3);
}
;

omp_parallel_clause : omp_unique_parallel_clause
{
	$$ = $1;
}
| omp_data_clause
{
	$$ = $1;
}
| omp_custom_clause
{
	$$ = $1;
}
;

omp_unique_parallel_clause : OMP_IF '(' expression ')' 
{
	$$ = ASTMake1(AST_OMP_IF_CLAUSE, $3, $1.token_file, $1.token_line, NULL);
}
| OMP_NUM_THREADS '(' expression ')'
{
	$$ = ASTMake1(AST_OMP_NUM_THREADS_CLAUSE, $3, $1.token_file, $1.token_line, NULL);
}
;

omp_task_construct : omp_task_directive omp_structured_block
{
	$$ = ASTMake2(AST_OMP_TASK_CONSTRUCT, $1, $2, ASTFileName($1), ASTLine($1), NULL);
};

omp_task_directive : OMP_PRAGMA OMP_TASK omp_task_clause_seq_opt OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_TASK_DIRECTIVE, $3, $1.token_file, $1.token_line, NULL);
};

omp_task_clause_seq_opt : /* empty */
{
	$$ = NULL;
}
| omp_task_clause_seq
{
	$$ = $1;
}
;

omp_task_clause_seq : omp_task_clause
{
	$$ = ASTListLeaf($1);
}
| omp_task_clause_seq omp_task_clause
{
	$$ = ASTList($1, $2);
}
| omp_task_clause_seq ',' omp_task_clause
{
	$$ = ASTList($1, $3);
}
;

omp_task_clause : omp_unique_task_clause
{
	$$ = $1;
}
| omp_data_clause
{
	$$ = $1;
}
| omp_custom_clause
{
	$$ = $1;
}
;

omp_unique_task_clause : OMP_IF '(' expression ')' 
{
	$$ = ASTMake1(AST_OMP_IF_CLAUSE, $3, $1.token_file, $1.token_line, NULL);
}
;

omp_for_construct : omp_for_directive iteration_statement
{
	$$ = ASTMake2(AST_OMP_FOR_CONSTRUCT, $1, $2, ASTFileName($1), ASTLine($1), NULL);
}
;

omp_for_directive : OMP_PRAGMA OMP_FOR omp_for_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_FOR_DIRECTIVE, $3, $1.token_file, $1.token_line, NULL);
}
;

omp_for_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| omp_for_clause_seq
{
	$$ = $1;
}
;

omp_for_clause_seq : omp_for_clause
{
	$$ = ASTListLeaf($1);
}
| omp_for_clause_seq ',' omp_for_clause
{
	$$ = ASTList($1, $3);
}
| omp_for_clause_seq omp_for_clause
{
	$$ = ASTList($1, $2);
}
;

omp_for_clause : omp_unique_for_clause 
{
	$$ = $1;
}
| omp_data_clause
{
	$$ = $1
}
| omp_nowait_clause
{
	$$ = $1;
}
| omp_custom_clause
{
	$$ = $1;
}
;

omp_unique_for_clause : OMP_ORDERED
{
	$$ = ASTLeaf(AST_OMP_ORDERED_CLAUSE, $1.token_file, $1.token_line, NULL);
}
| OMP_SCHEDULE '(' omp_schedule_kind ')'
{
	$$ = ASTMake2(AST_OMP_SCHEDULE_CLAUSE, $3, NULL, $1.token_file, $1.token_line, NULL);
}
| OMP_SCHEDULE '(' omp_schedule_kind ',' expression ')'
{
	$$ = ASTMake2(AST_OMP_SCHEDULE_CLAUSE, $3, $5, $1.token_file, $1.token_line, NULL);
}
;

omp_schedule_kind : OMP_STATIC
{
	$$ = ASTLeaf(AST_OMP_STATIC_SCHEDULE, $1.token_file, $1.token_line, NULL);
}
| OMP_DYNAMIC
{
	$$ = ASTLeaf(AST_OMP_DYNAMIC_SCHEDULE, $1.token_file, $1.token_line, NULL);
}
| OMP_GUIDED
{
	$$ = ASTLeaf(AST_OMP_GUIDED_SCHEDULE, $1.token_file, $1.token_line, NULL);
}
| OMP_RUNTIME
{
	$$ = ASTLeaf(AST_OMP_RUNTIME_SCHEDULE, $1.token_file, $1.token_line, NULL);
}
| OMP_SCHEDULE_CUSTOM
{
    $$ = ASTLeaf(AST_OMP_CUSTOM_SCHEDULE, $1.token_file, $1.token_line, $1.token_text);
}
;

omp_sections_construct : omp_sections_directive omp_section_scope
{
	$$ = ASTMake2(AST_OMP_SECTIONS_CONSTRUCT, $1, $2, ASTFileName($1), ASTLine($1), NULL);
}
;

omp_sections_directive : OMP_PRAGMA OMP_SECTIONS omp_sections_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_SECTIONS_DIRECTIVE, $3, $1.token_file, $1.token_line, NULL);
}
;

omp_sections_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| omp_sections_clause_seq
{
	$$ = $1;
}
;

omp_sections_clause_seq : omp_sections_clause
{
	$$ = ASTListLeaf($1);
}
| omp_sections_clause_seq omp_sections_clause
{
	$$ = ASTList($1, $2);
}
| omp_sections_clause_seq ',' omp_sections_clause
{
	$$ = ASTList($1, $3);
}
;

omp_sections_clause : omp_data_clause
{
	$$ = $1;
}
| omp_nowait_clause
{
	$$ = $1;
}
| omp_custom_clause
{
}
;

omp_nowait_clause :  OMP_NOWAIT
{
	$$ = ASTLeaf(AST_OMP_NOWAIT_CLAUSE, $1.token_file, $1.token_line, NULL);
}
;

omp_section_scope : '{' omp_section_sequence '}'
{
	$$ = $2;
}
;

omp_section_sequence : omp_section_directive omp_structured_block
{
    AST section_holder = ASTMake2(AST_OMP_SECTION, $1, $2, ASTFileName($1), ASTLine($1), NULL);
	$$ = ASTListLeaf(section_holder);
}
| omp_structured_block
{
    AST section_holder = ASTMake2(AST_OMP_SECTION, NULL, $1, ASTFileName($1), ASTLine($1), NULL);
	$$ = ASTListLeaf(section_holder);
}
| omp_section_sequence omp_section_directive omp_structured_block
{
    AST section_holder = ASTMake2(AST_OMP_SECTION, $2, $3, ASTFileName($2), ASTLine($2), NULL);
	$$ = ASTList($1, section_holder);
}
;

omp_section_directive : OMP_PRAGMA OMP_SECTION OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_SECTION_DIRECTIVE, $1.token_file, $1.token_line, NULL);
}
;

omp_single_construct : omp_single_directive omp_structured_block
{
	$$ = ASTMake2(AST_OMP_SINGLE_CONSTRUCT, $1, $2, ASTFileName($1), ASTLine($1), NULL);
}
;

omp_single_directive : OMP_PRAGMA OMP_SINGLE omp_single_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_SINGLE_DIRECTIVE, $3, $1.token_file, $1.token_line, NULL);
}
;

omp_single_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| omp_single_clause_seq
{
	$$ = $1;
}
;

omp_single_clause_seq : omp_single_clause
{
	$$ = ASTListLeaf($1);
}
| omp_single_clause_seq ',' omp_single_clause
{
	$$ = ASTList($1, $3);
}
| omp_single_clause_seq omp_single_clause
{
	$$ = ASTList($1, $2);
}
;

omp_single_clause : omp_data_clause
{
	$$ = $1;
}
| omp_nowait_clause
{
	$$ = $1;
}
| omp_custom_clause
{
	$$ = $1;
}
;

omp_parallel_for_construct : omp_parallel_for_directive iteration_statement
{
	$$ = ASTMake2(AST_OMP_PARALLEL_FOR_CONSTRUCT, $1, $2, ASTFileName($1), ASTLine($1), NULL);
}
;

omp_parallel_for_directive : OMP_PRAGMA OMP_PARALLEL_FOR omp_parallel_for_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_PARALLEL_FOR_DIRECTIVE, $3, $1.token_file, $1.token_line, NULL);
}
;

omp_parallel_for_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| omp_parallel_for_clause_seq 
{
	$$ = $1;
}
;

omp_parallel_for_clause_seq : omp_parallel_for_clause
{
	$$ = ASTListLeaf($1);
}
| omp_parallel_for_clause_seq ',' omp_parallel_for_clause
{
	$$ = ASTList($1, $3);
}
| omp_parallel_for_clause_seq omp_parallel_for_clause
{
	$$ = ASTList($1, $2);
}
;

omp_parallel_for_clause : omp_unique_parallel_clause
{
	$$ = $1;
}
| omp_unique_for_clause
{
	$$ = $1;
}
| omp_data_clause
{
	$$ = $1;
}
| omp_custom_clause
{
	$$ = $1;
}
;

omp_parallel_sections_construct : omp_parallel_sections_directive omp_section_scope
{
	$$ = ASTMake2(AST_OMP_PARALLEL_SECTIONS_CONSTRUCT, $1, $2, ASTFileName($1), ASTLine($1), NULL);
}
;

omp_parallel_sections_directive : OMP_PRAGMA OMP_PARALLEL_SECTIONS omp_parallel_sections_clause_opt_seq OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_PARALLEL_SECTIONS_DIRECTIVE, $3, $1.token_file, $1.token_line, NULL);
}
;

omp_parallel_sections_clause_opt_seq : /* empty */
{
	$$ = NULL;
}
| omp_parallel_sections_clause_seq
{
	$$ = $1;
}
;

omp_parallel_sections_clause_seq : omp_parallel_sections_clause
{
	$$ = ASTListLeaf($1);
}
| omp_parallel_sections_clause_seq ',' omp_parallel_sections_clause
{
	$$ = ASTList($1, $3);
}
| omp_parallel_sections_clause_seq omp_parallel_sections_clause
{
	$$ = ASTList($1, $2);
}
;

omp_parallel_sections_clause : omp_unique_parallel_clause
{
	$$ = $1;
}
| omp_data_clause
{
	$$ = $1;
}
| omp_custom_clause
{
	$$ = $1;
}
;

omp_master_construct : omp_master_directive omp_structured_block
{
	$$ = ASTMake2(AST_OMP_MASTER_CONSTRUCT, $1, $2, ASTFileName($1), ASTLine($1), NULL);
}
;

omp_master_directive : OMP_PRAGMA OMP_MASTER OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_MASTER_DIRECTIVE, $1.token_file, $1.token_line, NULL);
}
;

omp_critical_construct : omp_critical_directive omp_structured_block
{
	$$ = ASTMake2(AST_OMP_CRITICAL_CONSTRUCT, $1, $2, ASTFileName($1), ASTLine($1), NULL);
}
;

omp_critical_directive : OMP_PRAGMA OMP_CRITICAL omp_region_phrase_opt OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_CRITICAL_DIRECTIVE, $3, $1.token_file, $1.token_line, NULL);
}
;

omp_region_phrase_opt : /* empty */
{
	$$ = NULL;
}
| omp_region_phrase
{
	$$ = $1;
}
;

omp_region_phrase : '(' IDENTIFIER ')'
{
	// Cast it into an expression, makes things a lot easier
	AST critical_region_phrase = ASTLeaf(AST_SYMBOL, $2.token_file, $2.token_line, $2.token_text);

	$$ = ASTMake1(AST_EXPRESSION, critical_region_phrase, $1.token_file, $1.token_line, NULL);
}
;

omp_barrier_directive : OMP_PRAGMA OMP_BARRIER OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_BARRIER_DIRECTIVE, $1.token_file, $1.token_line, NULL);
}
;

omp_atomic_construct : omp_atomic_directive expression_statement
{
	$$ = ASTMake2(AST_OMP_ATOMIC_CONSTRUCT, $1, $2, ASTFileName($1), ASTLine($1), NULL);
}
;

omp_atomic_directive : OMP_PRAGMA OMP_ATOMIC OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_ATOMIC_DIRECTIVE, $1.token_file, $1.token_line, NULL);
}
;

omp_flush_directive : OMP_PRAGMA OMP_FLUSH omp_flush_vars_opt OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_FLUSH_DIRECTIVE, $3, $1.token_file, $1.token_line, NULL);
}
;

omp_flush_vars_opt : /* empty */
{
	$$ = NULL;
}
| omp_flush_vars
{
	$$ = $1;
}
;

omp_flush_vars : '(' omp_variable_list ')'
{
	$$ = $2;
}
;

omp_ordered_construct : omp_ordered_directive omp_structured_block
{
	$$ = ASTMake2(AST_OMP_ORDERED_CONSTRUCT, $1, $2, ASTFileName($1), ASTLine($1), NULL);
}
;

omp_ordered_directive : OMP_PRAGMA OMP_ORDERED OMP_NEWLINE
{
	$$ = ASTLeaf(AST_OMP_ORDERED_DIRECTIVE, $1.token_file, $1.token_line, NULL);
}
;

omp_taskwait_directive : OMP_PRAGMA OMP_TASKWAIT OMP_NEWLINE
{
    $$ = ASTLeaf(AST_OMP_TASKWAIT_DIRECTIVE, $1.token_file, $1.token_line, NULL);
}
;

omp_threadprivate_directive : OMP_PRAGMA OMP_THREADPRIVATE '(' omp_variable_list ')' OMP_NEWLINE
{
	$$ = ASTMake1(AST_OMP_THREADPRIVATE_DIRECTIVE, $4, $1.token_file, $1.token_line, NULL);
}
;

omp_declare_directive : omp_declare_reduction
{
    $$ = $1;
}
;

omp_declare_reduction : OMP_PRAGMA OMP_DECLARE OMP_REDUCTION omp_declare_reduction_clauses OMP_NEWLINE
{
    $$ = ASTMake1(AST_OMP_DECLARE_REDUCTION_DIRECTIVE, $4, $1.token_file, $1.token_line, NULL);
};

omp_declare_reduction_clauses : omp_declare_reduction_clause
{
    $$ = ASTListLeaf($1);
}
| omp_declare_reduction_clauses omp_declare_reduction_clause
{
    $$ = ASTList($1, $2);
}
;

omp_declare_reduction_clause : OMP_TYPE '(' type_id ')'
{
    $$ = ASTMake1(AST_OMP_TYPE_CLAUSE, $3, $1.token_file, $1.token_line, NULL);
}
| OMP_IDENTITY '(' omp_identity_expression ')'
{
    $$ = ASTMake1(AST_OMP_IDENTITY_CLAUSE, $3, $1.token_file, $1.token_line, NULL);
}
| OMP_OPERATOR '(' omp_user_defined_operator ')'
{
    $$ = ASTMake1(AST_OMP_OPERATOR_CLAUSE, $3, $1.token_file, $1.token_line, NULL);
}
| OMP_ORDER '(' omp_reduction_order ')'
{
    $$ = ASTMake1(AST_OMP_ORDER_CLAUSE, $3, $1.token_file, $1.token_line, NULL);
}
| OMP_COMMUTATIVE
{
    $$ = ASTLeaf(AST_OMP_COMMUTATIVE_CLAUSE, $1.token_file, $1.token_line, NULL);
}
;

omp_user_defined_operator : omp_reduction_operator
{
    $$ = ASTMake1(AST_OMP_REDUCTION_OPERATOR_BUILTIN, $1, ASTFileName($1), ASTLine($1), NULL);
}
| id_expression
{
    $$ = ASTMake1(AST_OMP_REDUCTION_OPERATOR_FUNCTION, $1, ASTFileName($1), ASTLine($1), NULL);
}
| '.' id_expression
{
    $$ = ASTMake1(AST_OMP_REDUCTION_OPERATOR_MEMBER_FUNCTION, $2, ASTFileName($2), ASTLine($2), NULL);
}
;

omp_identity_expression : initializer_clause
{
    $$ = ASTMake1(AST_OMP_IDENTITY_INITIALIZER, $1, ASTFileName($1), ASTLine($1), NULL);
}
 // FIXME This is only for C++ (but tpp has a bug)
| OMP_CONSTRUCTOR '(' expression_list ')'
{
    $$ = ASTMake1(AST_OMP_IDENTITY_CONSTRUCTOR, $3, $1.token_file, $1.token_line, NULL);
}
;

omp_reduction_order : OMP_REDUCTION_LEFT
{
    $$ = ASTLeaf(AST_OMP_REDUCTION_LEFT, $1.token_file, $1.token_line, NULL);
}
| OMP_REDUCTION_RIGHT
{
    $$ = ASTLeaf(AST_OMP_REDUCTION_RIGHT, $1.token_file, $1.token_line, NULL);
}
;

omp_data_clause : OMP_PRIVATE '(' omp_variable_list ')'
{
	$$ = ASTMake1(AST_OMP_PRIVATE_CLAUSE, $3, $1.token_file, $1.token_line, NULL);
}
| OMP_COPYPRIVATE '(' omp_variable_list ')'
{
	$$ = ASTMake1(AST_OMP_COPYPRIVATE_CLAUSE, $3, $1.token_file, $1.token_line, NULL);
}
| OMP_FIRSTPRIVATE '(' omp_variable_list ')'
{
	$$ = ASTMake1(AST_OMP_FIRSTPRIVATE_CLAUSE, $3, $1.token_file, $1.token_line, NULL);
}
| OMP_LASTPRIVATE '(' omp_variable_list ')'
{
	$$ = ASTMake1(AST_OMP_LASTPRIVATE_CLAUSE, $3, $1.token_file, $1.token_line, NULL);
}
| OMP_SHARED '(' omp_variable_list ')'
{
	$$ = ASTMake1(AST_OMP_SHARED_CLAUSE, $3, $1.token_file, $1.token_line, NULL);
}
| OMP_DEFAULT '(' OMP_SHARED ')'
{
	$$ = ASTLeaf(AST_OMP_DEFAULT_SHARED_CLAUSE, $1.token_file, $1.token_line, NULL);
}
| OMP_DEFAULT '(' OMP_NONE ')'
{
	$$ = ASTLeaf(AST_OMP_DEFAULT_NONE_CLAUSE, $1.token_file, $1.token_line, NULL);
}
| OMP_DEFAULT '(' OMP_DEFAULT_CUSTOM ')'
{
    $$ = ASTLeaf(AST_OMP_DEFAULT_CUSTOM_CLAUSE, $1.token_file, $1.token_line, $3.token_text);
}
| OMP_REDUCTION '(' omp_reduction_operator ':' omp_variable_list ')'
{
	$$ = ASTMake2(AST_OMP_REDUCTION_CLAUSE, 
			$3, $5, $1.token_file, $1.token_line, NULL);
}
| OMP_COPYIN '(' omp_variable_list ')'
{
	$$ = ASTMake1(AST_OMP_COPYIN_CLAUSE, $3, $1.token_file, $1.token_line, NULL);
}
;

omp_reduction_operator : '+' 
{
	$$ = ASTLeaf(AST_ADD_OPERATOR, $1.token_file, $1.token_line, NULL);
}
| '*'
{
	$$ = ASTLeaf(AST_MULT_OPERATOR, $1.token_file, $1.token_line, NULL);
}
| '-'
{
	$$ = ASTLeaf(AST_MINUS_OPERATOR, $1.token_file, $1.token_line, NULL);
}
| '&'
{
	$$ = ASTLeaf(AST_BITWISE_AND_OPERATOR, $1.token_file, $1.token_line, NULL);
}
| '^'
{
	$$ = ASTLeaf(AST_BITWISE_XOR_OPERATOR, $1.token_file, $1.token_line, NULL);
}
| '|'
{
	$$ = ASTLeaf(AST_BITWISE_OR_OPERATOR, $1.token_file, $1.token_line, NULL);
}
| ANDAND
{
	$$ = ASTLeaf(AST_LOGICAL_AND_OPERATOR, $1.token_file, $1.token_line, NULL);
}
| OROR
{
	$$ = ASTLeaf(AST_LOGICAL_OR_OPERATOR, $1.token_file, $1.token_line, NULL);
}
;

omp_variable_list : id_expression
{
	$$ = ASTListLeaf($1);
}
| omp_variable_list ',' id_expression
{
	$$ = ASTList($1, $3);
}
;

/*!endif*/
