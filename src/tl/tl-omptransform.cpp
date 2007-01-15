#include "tl-omp.hpp"
#include "tl-omptransform.hpp"
#include "tl-predicateutils.hpp"
#include "tl-source.hpp"
#include <iostream>
#include <utility>
#include <stack>
#include <set>


namespace TL
{
    class OpenMPTransform : public OpenMP::OpenMPPhase
    {
        private:
            // Here we declare those "persistent" variables

            // The number of parallel regions seen so far
            // (technically not, since it is updated in the postorder of these)
            int num_parallels;

            // The nesting for parallel, updated both in preorder and postorder
            int parallel_nesting;

            // A stack to save the number of "section" within a "sections".  If
            // just a scalar was used, we would only be able to handle one
            // level of sections
            std::stack<int> num_sections_stack;

			// Stores the innermost induction variable of a parallel for or for construct
			std::stack<IdExpression> induction_var_stack;

            // A set to save what critical names have been defined in
            // translation unit level
            std::set<std::string> criticals_defined;
        public:
            OpenMPTransform()
            {
            }

            virtual ~OpenMPTransform()
            {
                // This is needed since "init" is a virtual method
            }

            virtual void init()
            {
                // This function is called in OpenMPPhase::run. The user
                // can register here the handlers that will be called for
                // every construction (in preorder and postorder)
                //
                // Register the handlers (callbacks) for every construction

                // #pragma omp parallel
                on_parallel_pre.connect(&OpenMPTransform::parallel_preorder, *this);
                on_parallel_post.connect(&OpenMPTransform::parallel_postorder, *this);

                // #pragma omp parallel for
                on_parallel_for_pre.connect(&OpenMPTransform::parallel_for_preorder, *this);
                on_parallel_for_post.connect(&OpenMPTransform::parallel_for_postorder, *this);

                // #pragma omp for
				on_for_pre.connect(&OpenMPTransform::for_preorder, *this);
                on_for_post.connect(&OpenMPTransform::for_postorder, *this);

                // #pragma omp parallel sections 
                on_parallel_sections_pre.connect(&OpenMPTransform::parallel_sections_preorder, *this);
                on_parallel_sections_post.connect(&OpenMPTransform::parallel_sections_postorder, *this);
                
                // #pragma omp sections
                on_sections_pre.connect(&OpenMPTransform::sections_preorder, *this);
                on_sections_pre.connect(&OpenMPTransform::sections_postorder, *this);
                
                // #pragma omp section
                on_section_post.connect(&OpenMPTransform::section_postorder, *this);

                // #pragma omp barrier
                on_barrier_post.connect(&OpenMPTransform::barrier_postorder, *this);
                
                // #pragma omp atomic
                on_atomic_post.connect(&OpenMPTransform::atomic_postorder, *this);

				// #pragma omp ordered
				on_ordered_post.connect(&OpenMPTransform::ordered_postorder, *this);

				// #pragma omp master
				on_master_post.connect(&OpenMPTransform::master_postorder, *this);

                // #pragma omp single
                on_single_post.connect(&OpenMPTransform::single_postorder, *this);
				
                // #pragma omp single
                on_parallel_single_post.connect(&OpenMPTransform::parallel_single_postorder, *this);
                
                // #pragma omp critical
                on_critical_post.connect(&OpenMPTransform::critical_postorder, *this);
                
                // #pragma omp flush
                on_flush_post.connect(&OpenMPTransform::flush_postorder, *this);

                // #pragma omp threadprivate
                on_threadprivate_post.connect(&OpenMPTransform::threadprivate_postorder, *this);

                // #pragma omp task
                on_custom_construct_post["task"].connect(&OpenMPTransform::task_postorder, *this);

                // #pragma omp directive taskwait
                on_custom_construct_post["taskwait"].connect(&OpenMPTransform::taskwait_postorder, *this);
                
                // #pragma omp directive taskgroup
                on_custom_construct_post["taskgroup"].connect(&OpenMPTransform::taskgroup_postorder, *this);
            }

            void threadprivate_postorder(OpenMP::ThreadPrivateDirective threadprivate_directive)
            {
                OpenMP::Directive directive = threadprivate_directive.directive();

                OpenMP::Clause clause = directive.parameter_clause();

                ObjectList<IdExpression> threadprivate_references = clause.id_expressions();

                for (ObjectList<IdExpression>::iterator it = threadprivate_references.begin();
                        it != threadprivate_references.end();
                        it++)
                {
                    Declaration decl = it->get_declaration();

                    DeclarationSpec decl_spec = decl.get_declaration_specifiers();
                    ObjectList<DeclaredEntity> declared_entities = decl.get_declared_entities();

                    Source remade_declaration;
                    Source not_modified_entities;

                    if (declared_entities.size() > 1)
                    {
                        for (ObjectList<DeclaredEntity>::iterator it2 = declared_entities.begin();
                                it2 != declared_entities.end();
                                it2++)
                        {
                            if (it2->get_declared_entity().get_symbol() != it->get_symbol())
                            {
                                not_modified_entities.append_with_separator(it2->prettyprint(), ",");
                            }
                        }

                        remade_declaration
                            << decl_spec.prettyprint() << " " << not_modified_entities << ";"
                            ;
                    }
                    remade_declaration
                        << decl_spec.prettyprint() << " __thread " << it->prettyprint() << ";"
                        ;

                    AST_t redeclaration_tree = remade_declaration.parse_declaration(decl.get_scope(),
                            scope_link, Source::ALLOW_REDECLARATION);
                    decl.get_ast().replace(redeclaration_tree);

                    // std::cerr << "-- Remade declaration --" << std::endl;
                    // std::cerr << remade_declaration.get_source(true) << std::endl;
                    // std::cerr << "-- End remade declaration --" << std::endl;
                }

				threadprivate_directive.get_ast().remove_in_list();
            }

            void task_postorder(OpenMP::CustomConstruct task_construct)
            {
                // One more parallel seen
                num_parallels++;

                OpenMP::Directive directive = task_construct.directive();
                Statement construct_body = task_construct.body();
				
                // Get the enclosing function
                FunctionDefinition function_definition = task_construct.get_enclosing_function();
                // its scope
                Scope function_scope = function_definition.get_scope();
                // and the id-expression of the function name
                IdExpression function_name = function_definition.get_function_name();
                // create the outlined function name
                Source outlined_function_name = get_outlined_function_name(function_name);

                // Get references in local clause
                OpenMP::CustomClause local_clause = directive.custom_clause("local");
                ObjectList<IdExpression> local_references = local_clause.id_expressions();

                // Get references in captureaddress clause
                OpenMP::CustomClause captureaddress_clause = directive.custom_clause("captureaddress");
				
				ObjectList<IdExpression> captureaddress_references_all = captureaddress_clause.id_expressions();
				ObjectList<IdExpression> captureaddress_references;
				{
					for (ObjectList<IdExpression>::iterator it = captureaddress_references_all.begin();
							it != captureaddress_references_all.end();
							it++)
					{
						Symbol global_sym = function_scope.get_symbol_from_id_expr(it->get_ast());

						if (!global_sym.is_valid() ||
								global_sym != it->get_symbol())
						{
							captureaddress_references.append(*it);
						}
					}
				}

                OpenMP::CustomClause capturevalue_clause = directive.custom_clause("capturevalue");
                ObjectList<IdExpression> capturevalue_references = capturevalue_clause.id_expressions();


				ObjectList<IdExpression> capturevalue_references_body;
				// Fix this with a better ObjectList<T>::insert(Functor<S, T>, T);
				{
					ObjectList<IdExpression> capturevalue_references_body_all
						= construct_body.non_local_symbol_occurrences(Statement::ONLY_VARIABLES);

					for (ObjectList<IdExpression>::iterator it = capturevalue_references_body_all.begin();
							it != capturevalue_references_body_all.end();
							it++)
					{
						if (!capturevalue_references_body.contains(functor(&IdExpression::get_symbol), it->get_symbol()))
						{
								capturevalue_references_body.append(*it);
						}
					}
				}

                // Filter those symbols in local and capturevalue
                capturevalue_references_body = 
                    capturevalue_references_body.filter(not_in_set(local_references, functor(&IdExpression::get_symbol)));
                capturevalue_references_body = 
                    capturevalue_references_body.filter(not_in_set(captureaddress_references_all, functor(&IdExpression::get_symbol)));
                capturevalue_references_body = 
                    capturevalue_references_body.filter(not_in_set(capturevalue_references, functor(&IdExpression::get_symbol)));

                capturevalue_references.append(capturevalue_references_body);

                // This list will hold everything that must be passed by pointer
                ObjectList<IdExpression> pass_by_pointer;
                // This list will hold everything that has been privatized
                ObjectList<IdExpression> privatized_entities;
                // Create the replacement map and fill the privatized
                // entities and pass by pointer lists.
                ObjectList<IdExpression> empty;
                ObjectList<OpenMP::ReductionIdExpression> reduction_empty;

                ReplaceIdExpression replace_references = 
                    set_replacements(function_definition,
                            directive,
                            construct_body,
                            captureaddress_references,
                            local_references,
                            empty,
                            empty,
                            reduction_empty,
                            pass_by_pointer,
                            privatized_entities);

                // Get the code of the outline
                AST_t outline_code = get_outline_task(
                        function_definition,
                        outlined_function_name, 
                        construct_body,
                        replace_references,
                        local_references,
                        pass_by_pointer,
                        capturevalue_references);
                
                // Now prepend the outline
                function_definition.get_ast().prepend_sibling_function(outline_code);

                Source task_queueing;
                Source size_params;
                Source task_parameters;
				Source task_parameter_list;

				// FIXME - This is for IA32 only
                for (ObjectList<IdExpression>::iterator it = captureaddress_references.begin();
                        it != captureaddress_references.end();
                        it++)
                {
                    task_parameter_list.append_with_separator("&" + it->prettyprint(), ",");

                    size_params << "num_params += "
                        << "((sizeof(&" << it->prettyprint() << ") % 4) == 0) "
                        <<   "? sizeof(&" << it->prettyprint() << ")"
                        <<   ": (sizeof(&" << it->prettyprint() << ") - (4 - (sizeof(&" << it->prettyprint() << ") % 4)));"
                        ;
                }

                for (ObjectList<IdExpression>::iterator it = capturevalue_references.begin();
                        it != capturevalue_references.end();
                        it++)
                {
                    task_parameter_list.append_with_separator(it->prettyprint(), ",");

                    size_params << "num_params += "
                        << "((sizeof(" << it->prettyprint() << ") % 4) == 0) "
                        <<   "? sizeof(" << it->prettyprint() << ")"
                        <<   ": (sizeof(" << it->prettyprint() << ") - (4 - (sizeof(" << it->prettyprint() << ") % 4)));"
                        ;
                }

				if (!task_parameter_list.empty())
				{
					task_parameters << ", " << task_parameter_list;
				}

				size_params 
					<< "num_params /= 4;";

				Source threadswitch;

				if (directive.custom_clause("switch").is_defined())
				{
					threadswitch << "1";
				}
				else
				{
					threadswitch << "0";
				}

                task_queueing
                    << "{"
                    <<    "nth_desc * nth;"
                    <<    "int arg;"
                    <<    "int set_threadswitch;"
                    <<    "int num_params;"
//                    <<    "extern struct nth_desc *nthf_create_task_(void (*)(), unsigned long long*, int*, ...);"

                    <<    "set_threadswitch = " << threadswitch << ";" 
                    <<    "num_params = 0;"
                    <<     size_params

                    <<    "nth = nthf_create_task_((void (*)())(" << outlined_function_name << "), "
                    <<             "&set_threadswitch, &num_params " << task_parameters << ");"
					<<    "if (nth == NTH_CANNOT_ALLOCATE_TASK)"
					<<    "{"
					<<       outlined_function_name << "(" << task_parameter_list << ");"
					<<    "}"
                    << "}"
                ;

                AST_t task_code = task_queueing.parse_statement(task_construct.get_scope(),
                        task_construct.get_scope_link());

                task_construct.get_ast().replace(task_code);
            }

            void taskwait_postorder(OpenMP::CustomConstruct taskwait_construct)
            {
                Source taskwait_source;
                Statement taskwait_body = taskwait_construct.body();

                taskwait_source
                    << "{"
//                    <<    "extern void nthf_task_block_(void);"
                    <<    "nthf_task_block_();"
					<<    taskwait_body.prettyprint()
                    << "}"
                    ;

                AST_t taskwait_code = taskwait_source.parse_statement(taskwait_construct.get_scope(),
                        taskwait_construct.get_scope_link());

                taskwait_construct.get_ast().replace(taskwait_code);
            }

            void taskgroup_postorder(OpenMP::CustomConstruct taskgroup_construct)
            {
                Source taskgroup_source;
                Statement taskgroup_body = taskgroup_construct.body();

                taskgroup_source
                    << "{"
//                    <<    "extern void nthf_task_block_(void);"
//                    <<    "extern int nthf_push_taskgroup_scope_(void);"
//                    <<    "extern int nthf_pop_taskgroup_scope_(void);"
                    <<    "nthf_push_taskgroup_scope_();"
                    <<    taskgroup_body.prettyprint()
                    <<    "nthf_task_block_();"
                    <<    "nthf_pop_taskgroup_scope_();"
                    << "}"
                    ;

                AST_t taskgroup_code = taskgroup_source.parse_statement(taskgroup_construct.get_scope(),
                        taskgroup_construct.get_scope_link());

                taskgroup_construct.get_ast().replace(taskgroup_code);
            }

            void section_postorder(OpenMP::SectionConstruct section_construct)
            {
                int &num_sections = num_sections_stack.top();

                Source section_source;
                Statement construct_body = section_construct.body();

                section_source
                    << "case " << num_sections << ":"
                    << "{"
                    <<    construct_body.prettyprint()
                    <<    "break;"
                    << "}"
                    ;

                AST_t section_tree = section_source.parse_statement(section_construct.get_scope(),
                        section_construct.get_scope_link());

                // One more section
                num_sections++;

                section_construct.get_ast().replace(section_tree);
            }

            void critical_postorder(OpenMP::CriticalConstruct critical_construct)
            {
                Source critical_source;

                OpenMP::Directive directive = critical_construct.directive();
                Statement critical_body = critical_construct.body();
                ScopeLink scope_link = critical_construct.get_scope_link();
                
                OpenMP::Clause region_name = directive.parameter_clause();

                std::string mutex_variable;

                if (!region_name.is_defined())
                {
                    mutex_variable = "_nthf_unspecified_critical";
                }
                else
                {
                    ObjectList<IdExpression> id_expressions = region_name.id_expressions(OpenMP::ALL_SYMBOLS);
                    IdExpression head = id_expressions[0];

                    mutex_variable = "_nthf_"  + head.prettyprint();
                }

                critical_source
                    << "{"
//                    <<   "extern void nthf_spin_lock_(void*);"
//                    <<   "extern void nthf_spin_unlock_(void*);"
                    <<   "nthf_spin_lock_(&" << mutex_variable << ");"
                    <<   critical_body.prettyprint()
                    <<   "nthf_spin_unlock_(&" << mutex_variable << ");"
                    << "}"
                    ;

                if (criticals_defined.find(mutex_variable) == criticals_defined.end())
                {
                    // Now declare, if not done before
                    Source critical_mutex_def_src;

                    critical_mutex_def_src <<
                        "nth_word_t " << mutex_variable << ";"
                        ;

                    // AST_t translation_unit = critical_construct.get_ast().get_translation_unit();
                    // Scope scope_translation_unit = scope_link.get_scope(translation_unit);

                    AST_t critical_mutex_def_tree = critical_mutex_def_src.parse_global(critical_construct.get_scope(),
                            critical_construct.get_scope_link());

					critical_construct.get_ast().prepend_sibling_function(critical_mutex_def_tree);

                    criticals_defined.insert(mutex_variable);
                }

                AST_t critical_tree = critical_source.parse_statement(critical_construct.get_scope(),
                        critical_construct.get_scope_link());

                critical_construct.get_ast().replace(critical_tree);
            }

            void atomic_postorder(OpenMP::AtomicConstruct atomic_construct)
            {
                // TODO - An atomic can be implemented better
                Source critical_source;

                Statement critical_body = atomic_construct.body();

                critical_source
                    << "{"
                    <<   "static nth_word_t default_mutex_var;"
//                    <<   "extern void nthf_spin_lock_(void*);"
//                    <<   "extern void nthf_spin_unlock_(void*);"
                    <<   "nthf_spin_lock_(&default_mutex_var);"
                    <<   critical_body.prettyprint()
                    <<   "nthf_spin_unlock_(&default_mutex_var);"
                    << "}"
                    ;

                AST_t atomic_tree = critical_source.parse_statement(atomic_construct.get_scope(),
                        atomic_construct.get_scope_link());

                atomic_construct.get_ast().replace(atomic_tree);
            }

            void barrier_postorder(OpenMP::BarrierDirective barrier_directive)
            {
                Source barrier_source;

                barrier_source
                    << "{"
//                    <<    "extern void in__tone_barrier_();"
                    <<    "in__tone_barrier_();"
                    << "}"
                    ;

                AST_t barrier_tree = barrier_source.parse_statement(barrier_directive.get_scope(),
                        barrier_directive.get_scope_link());

                barrier_directive.get_ast().replace(barrier_tree);
            }

            void flush_postorder(OpenMP::FlushDirective flush_directive)
            {
                Source flush_source;

                flush_source
                    << "{"
//                    <<    "extern void synchronize();"
                    <<    "synchronize();"
                    << "}"
                    ;

                AST_t flush_tree = flush_source.parse_statement(flush_directive.get_scope(),
                        flush_directive.get_scope_link());

                flush_directive.get_ast().replace(flush_tree);
            }

            void parallel_single_preorder(OpenMP::ParallelSingleConstruct parallel_single_construct)
            {
                // Increase the parallel nesting value
                parallel_nesting++;
            }

            void parallel_single_postorder(OpenMP::ParallelSingleConstruct parallel_single_construct)
            {
                // One more parallel seen
                num_parallels++;

                // Decrease the parallel nesting
                parallel_nesting--;
                
                // Get the directive
                OpenMP::Directive directive = parallel_single_construct.directive();
                
                // Get the enclosing function definition
                FunctionDefinition function_definition = parallel_single_construct.get_enclosing_function();
                // its scope
                Scope function_scope = function_definition.get_scope();
                // and the id-expression of the function name
                IdExpression function_name = function_definition.get_function_name();

                // They will hold the entities as they appear in the clauses
                ObjectList<IdExpression> shared_references;
                ObjectList<IdExpression> private_references;
                ObjectList<IdExpression> firstprivate_references;
                ObjectList<IdExpression> lastprivate_references;
                ObjectList<OpenMP::ReductionIdExpression> reduction_references;
                
                // Get the construct_body of the statement
                Statement construct_body = parallel_single_construct.body();

                // Get the data attributes for every entity
                get_data_attributes(function_scope,
                        directive,
                        construct_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);

                // This list will hold everything that must be passed by pointer
                ObjectList<IdExpression> pass_by_pointer;
                // This list will hold everything that has been privatized
                ObjectList<IdExpression> privatized_entities;
                // Create the replacement map and fill the privatized
                // entities and pass by pointer lists.
                ReplaceIdExpression replace_references = 
                    set_replacements(function_definition,
                            directive,
                            construct_body,
                            shared_references,
                            private_references,
                            firstprivate_references,
                            lastprivate_references,
                            reduction_references,
                            pass_by_pointer,
                            privatized_entities);

                // Get the outline function name
                Source outlined_function_name = get_outlined_function_name(function_name);

                // Create the outline for parallel for using 
                // the privatized entities and pass by pointer
                // lists.
                // Additionally {first|last}private and reduction
                // entities are needed for proper initializations
                // and assignments.
                AST_t outline_code = get_outline_parallel_single(
                        function_definition,
                        outlined_function_name, 
                        construct_body,
                        replace_references,
                        pass_by_pointer,
                        privatized_entities,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);

                // In the AST of the function definition, prepend outline_code
                // as a sibling (at the same level)
                function_definition.get_ast().prepend_sibling_function(outline_code);

                // Now create the spawning code. Pass by pointer list and
                // reductions are needed for proper pass of data and reduction
                // vectors declaration
                
                OpenMP::Clause num_threads = directive.num_threads_clause();
                OpenMP::CustomClause groups_clause = directive.custom_clause("groups");
                
                AST_t spawn_code = get_parallel_spawn_code(
						function_definition,
						parallel_single_construct.get_scope(),
                        parallel_single_construct.get_scope_link(),
                        outlined_function_name,
                        pass_by_pointer,
                        reduction_references,
                        num_threads,
                        groups_clause
                        );

                // Now replace the whole construct with spawn_code
                parallel_single_construct.get_ast().replace(spawn_code);
			}

            void single_postorder(OpenMP::SingleConstruct single_construct)
            {
                Source single_source;
                Source barrier_code;

                Statement body_construct = single_construct.body();
                OpenMP::Directive directive = single_construct.directive();

                single_source
                    << "{"
                    <<   "int nth_low;"
                    <<   "int nth_upper;"
                    <<   "int nth_step;"
                    <<   "int nth_chunk;"
                    <<   "int nth_schedule;"
                    <<   "int nth_dummy1;"
                    <<   "int nth_dummy2;"
                    <<   "int nth_dummy3;"
                    <<   "int nth_barrier; "

                    <<   "nth_low = 0;"
                    <<   "nth_upper = 0;"
                    <<   "nth_step = 1;"
                    <<   "nth_schedule = 1;"
                    <<   "nth_chunk = 1;"

//                    <<   "extern void in__tone_begin_for_(int*, int*, int*, int*, int*);"
//                    <<   "extern int in__tone_next_iters_(int*, int*, int*);"
//                    <<   "extern void in__tone_end_for_(int*);"

                    <<   "in__tone_begin_for_ (&nth_low, &nth_upper, &nth_step, &nth_chunk, &nth_schedule);"
                    <<   "while (in__tone_next_iters_ (&nth_dummy1, &nth_dummy2, &nth_dummy3) != 0)"
                    <<   "{"
                    <<       body_construct.prettyprint()
                    <<   "}"
                    <<   barrier_code
                    << "}"
                    ;

                OpenMP::Clause nowait_clause = directive.nowait_clause();
                barrier_code << "nth_barrier = " << (nowait_clause.is_defined() ? "0" : "1") << ";";
                barrier_code << "in__tone_end_for_(&nth_barrier);";

                AST_t single_tree = single_source.parse_statement(single_construct.get_scope(), 
                        single_construct.get_scope_link());

                single_construct.get_ast().replace(single_tree);
            }

            // Parallel in preorder
            void parallel_preorder(OpenMP::ParallelConstruct parallel_construct)
            {
                // Increase the parallel nesting value
                parallel_nesting++;
            }

            // Parallel in postorder
            void parallel_postorder(OpenMP::ParallelConstruct parallel_construct)
            {
                // One more parallel seen
                num_parallels++;

                // Decrease the parallel nesting
                parallel_nesting--;
                
                // Get the directive
                OpenMP::Directive directive = parallel_construct.directive();
                
                // Get the enclosing function definition
                FunctionDefinition function_definition = parallel_construct.get_enclosing_function();
                // its scope
                Scope function_scope = function_definition.get_scope();
                // and the id-expression of the function name
                IdExpression function_name = function_definition.get_function_name();

                // They will hold the entities as they appear in the clauses
                ObjectList<IdExpression> shared_references;
                ObjectList<IdExpression> private_references;
                ObjectList<IdExpression> firstprivate_references;
                ObjectList<IdExpression> lastprivate_references;
                ObjectList<OpenMP::ReductionIdExpression> reduction_references;
                
                // Get the construct_body of the statement
                Statement construct_body = parallel_construct.body();

                // Get the data attributes for every entity
                get_data_attributes(function_scope,
                        directive,
                        construct_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);

                // This list will hold everything that must be passed by pointer
                ObjectList<IdExpression> pass_by_pointer;
                // This list will hold everything that has been privatized
                ObjectList<IdExpression> privatized_entities;
                // Create the replacement map and fill the privatized
                // entities and pass by pointer lists.
                ReplaceIdExpression replace_references = 
                    set_replacements(function_definition,
                            directive,
                            construct_body,
                            shared_references,
                            private_references,
                            firstprivate_references,
                            lastprivate_references,
                            reduction_references,
                            pass_by_pointer,
                            privatized_entities);

                // Get the outline function name
                Source outlined_function_name = get_outlined_function_name(function_name);

                // Create the outline for parallel for using 
                // the privatized entities and pass by pointer
                // lists.
                // Additionally {first|last}private and reduction
                // entities are needed for proper initializations
                // and assignments.
                AST_t outline_code = get_outline_parallel(
                        function_definition,
                        outlined_function_name, 
                        construct_body,
                        replace_references,
                        pass_by_pointer,
                        privatized_entities,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);

                // In the AST of the function definition, prepend outline_code
                // as a sibling (at the same level)
                function_definition.get_ast().prepend_sibling_function(outline_code);

                // Now create the spawning code. Pass by pointer list and
                // reductions are needed for proper pass of data and reduction
                // vectors declaration
                
                OpenMP::Clause num_threads = directive.num_threads_clause();
                OpenMP::CustomClause groups_clause = directive.custom_clause("groups");
                
                AST_t spawn_code = get_parallel_spawn_code(
						function_definition,
						parallel_construct.get_scope(),
                        parallel_construct.get_scope_link(),
                        outlined_function_name,
                        pass_by_pointer,
                        reduction_references,
                        num_threads,
                        groups_clause
                        );

                // Now replace the whole construct with spawn_code
                parallel_construct.get_ast().replace(spawn_code);
            }

            void parallel_for_preorder(OpenMP::ParallelForConstruct parallel_for_construct)
            {
                // Increase the parallel nesting value
                parallel_nesting++;

                Statement construct_body = parallel_for_construct.body();
                // The construct is in fact a ForStatement in a #pragma omp parallel do
                ForStatement for_statement(construct_body);
				
                IdExpression induction_var = for_statement.get_induction_variable();

				// Save this induction var in the stack
				induction_var_stack.push(induction_var);
            }

            void parallel_for_postorder(OpenMP::ParallelForConstruct parallel_for_construct)
            {
                // One more parallel seen
                num_parallels++;

				// Remove the induction var from the stack
				induction_var_stack.pop();

                // Decrease the parallel nesting level 
                parallel_nesting--;

                // Get the directive
                OpenMP::Directive directive = parallel_for_construct.directive();
                
                // Get the enclosing function definition
                FunctionDefinition function_definition = parallel_for_construct.get_enclosing_function();
                Scope function_scope = function_definition.get_scope();
                IdExpression function_name = function_definition.get_function_name();

                // They will hold the entities as they appear in the clauses
                ObjectList<IdExpression> shared_references;
                ObjectList<IdExpression> private_references;
                ObjectList<IdExpression> firstprivate_references;
                ObjectList<IdExpression> lastprivate_references;
                ObjectList<OpenMP::ReductionIdExpression> reduction_references;
                
                // Get the construct_body of the statement
                Statement construct_body = parallel_for_construct.body();
                // The construct is in fact a ForStatement in a #pragma omp parallel do
                ForStatement for_statement(construct_body);
                Statement loop_body = for_statement.get_loop_body();

                // Get the data attributes for every entity
                get_data_attributes(function_scope,
                        directive,
                        loop_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);

                // The induction variable deserves special treatment
                IdExpression induction_var = for_statement.get_induction_variable();
                // If private_references does not contain an IdExpression whose
                // related symbol is the induction variable symbol, then
                // privatize here (FIXME: I've seen codes where the induction
                // variable appears in lastprivate)
                if (!private_references.contains(functor(&IdExpression::get_symbol), induction_var.get_symbol()))
                {
                    // Add the induction variale onto the private references
                    private_references.append(induction_var);
                    // And now remove, if it was already there, any reference
                    // to the induction variable symbol from the set of shared
                    // references
                    shared_references = shared_references.not_find(functor(&IdExpression::get_symbol), 
                            induction_var.get_symbol());
                }

                // The lists of entities passes by pointer and entities
                // privatized in the outline
                ObjectList<IdExpression> pass_by_pointer;
                ObjectList<IdExpression> privatized_entities;
                // Create the replacement map and the pass_by_pointer set
                ReplaceIdExpression replace_references = 
                    set_replacements(function_definition,
                            directive,
                            loop_body,
                            shared_references,
                            private_references,
                            firstprivate_references,
                            lastprivate_references,
                            reduction_references,
                            pass_by_pointer,
                            privatized_entities);

                // Get the outline function name
                Source outlined_function_name = get_outlined_function_name(function_name);

                // Create the outline for parallel for
                AST_t outline_code = get_outline_parallel_for(
                        function_definition,
                        outlined_function_name, 
                        for_statement,
                        loop_body,
                        replace_references,
                        pass_by_pointer,
                        privatized_entities,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);

                // Now prepend the outline
                function_definition.get_ast().prepend_sibling_function(outline_code);

                OpenMP::Clause num_threads = directive.num_threads_clause();
                OpenMP::CustomClause groups_clause = directive.custom_clause("groups");

                AST_t spawn_code = get_parallel_spawn_code(
						function_definition,
						parallel_for_construct.get_scope(),
                        parallel_for_construct.get_scope_link(),
                        outlined_function_name,
                        pass_by_pointer,
                        reduction_references,
                        num_threads,
                        groups_clause
                        );

                // Replace all the whole construct with spawn_code
                parallel_for_construct.get_ast().replace(spawn_code);
            }

            void parallel_sections_preorder(OpenMP::ParallelSectionsConstruct parallel_sections_construct)
            {
                parallel_nesting++;

                // We push a new level of sections with zero "section" counted
                // so far
                num_sections_stack.push(0);
            }

            void parallel_sections_postorder(OpenMP::ParallelSectionsConstruct parallel_sections_construct)
            {
                // One more parallel seen
                num_parallels++;

                // Decrease the parallel nesting
                parallel_nesting--;
                
                // Get the directive
                OpenMP::Directive directive = parallel_sections_construct.directive();
                
                // Get the enclosing function definition
                FunctionDefinition function_definition = parallel_sections_construct.get_enclosing_function();
                // its scope
                Scope function_scope = function_definition.get_scope();
                // and the id-expression of the function name
                IdExpression function_name = function_definition.get_function_name();

                // They will hold the entities as they appear in the clauses
                ObjectList<IdExpression> shared_references;
                ObjectList<IdExpression> private_references;
                ObjectList<IdExpression> firstprivate_references;
                ObjectList<IdExpression> lastprivate_references;
                ObjectList<OpenMP::ReductionIdExpression> reduction_references;
                
                // Get the construct_body of the statement
                Statement construct_body = parallel_sections_construct.body();

                // Get the data attributes for every entity
                get_data_attributes(function_scope,
                        directive,
                        construct_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);

                // This list will hold everything that must be passed by pointer
                ObjectList<IdExpression> pass_by_pointer;
                // This list will hold everything that has been privatized
                ObjectList<IdExpression> privatized_entities;
                // Create the replacement map and fill the privatized
                // entities and pass by pointer lists.
                ReplaceIdExpression replace_references = 
                    set_replacements(function_definition,
                            directive,
                            construct_body,
                            shared_references,
                            private_references,
                            firstprivate_references,
                            lastprivate_references,
                            reduction_references,
                            pass_by_pointer,
                            privatized_entities);

                // Get the outline function name
                Source outlined_function_name = get_outlined_function_name(function_name);

                // Create the outline for parallel sections using 
                // the privatized entities and pass by pointer
                // lists.
                // Additionally {first|last}private and reduction
                // entities are needed for proper initializations
                // and assignments.
                AST_t outline_code = get_outline_parallel_sections(
                        function_definition,
                        outlined_function_name, 
                        construct_body,
                        replace_references,
                        pass_by_pointer,
                        privatized_entities,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);

                // In the AST of the function definition, prepend outline_code
                // as a sibling (at the same level)
                function_definition.get_ast().prepend_sibling_function(outline_code);

                OpenMP::Clause num_threads = directive.num_threads_clause();
                OpenMP::CustomClause groups_clause = directive.custom_clause("groups");
                
                // Now create the spawning code. Pass by pointer list and
                // reductions are needed for proper pass of data and reduction
                // vectors declaration
                AST_t spawn_code = get_parallel_spawn_code(
						function_definition,
						parallel_sections_construct.get_scope(),
                        parallel_sections_construct.get_scope_link(),
                        outlined_function_name,
                        pass_by_pointer,
                        reduction_references,
                        num_threads,
                        groups_clause
                        );

                // One less level of sections
                num_sections_stack.pop();

                // Now replace the whole construct with spawn_code
                parallel_sections_construct.get_ast().replace(spawn_code);
            }

            void sections_preorder(OpenMP::SectionsConstruct sections_construct)
            {
                // We push a new level of sections with zero "section" counted
                // so far
                num_sections_stack.push(0);
            }

            void sections_postorder(OpenMP::SectionsConstruct sections_construct)
            {
                // They will hold the entities as they appear in the clauses
                ObjectList<IdExpression> shared_references;
                ObjectList<IdExpression> private_references;
                ObjectList<IdExpression> firstprivate_references;
                ObjectList<IdExpression> lastprivate_references;
                ObjectList<OpenMP::ReductionIdExpression> reduction_references;
                
                // Get the construct_body of the statement
                OpenMP::Directive directive = sections_construct.directive();
                Statement construct_body = sections_construct.body();
                
                // Get the enclosing function definition
                FunctionDefinition function_definition = sections_construct.get_enclosing_function();
                // its scope
                Scope function_scope = function_definition.get_scope();

                // Get the data attributes for every entity
                get_data_attributes(function_scope,
                        directive,
                        construct_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);

                // This list will hold everything that must be passed by pointer
                ObjectList<IdExpression> pass_by_pointer;
                // This list will hold everything that has been privatized
                ObjectList<IdExpression> privatized_entities;
                // Create the replacement map and fill the privatized
                // entities and pass by pointer lists.
                ReplaceIdExpression replace_references = 
                    set_replacements(function_definition,
                            directive,
                            construct_body,
                            shared_references,
                            private_references,
                            firstprivate_references,
                            lastprivate_references,
                            reduction_references,
                            pass_by_pointer,
                            privatized_entities);

                int num_sections = num_sections_stack.top();

                Source loop_distribution_code;

                loop_distribution_code = get_loop_distribution_in_sections(num_sections,
                        construct_body,
                        replace_references);

                Source private_declarations;
                private_declarations = get_privatized_declarations(privatized_entities,
                        pass_by_pointer,
                        firstprivate_references,
                        reduction_references);

                Source lastprivate_code;

                if (!lastprivate_references.empty())
                {
                    Source lastprivate_assignments;
                    lastprivate_assignments = get_lastprivate_assignments(lastprivate_references,
                            pass_by_pointer);

                    lastprivate_code 
                        << "if (intone_last != 0)"
                        << "{"
                        <<    lastprivate_assignments
                        << "}"
                        ;
                }

                OpenMP::Clause nowait_clause = directive.nowait_clause();
                Source loop_finalization = get_loop_finalization(/*do_barrier=*/!(nowait_clause.is_defined()));

                Source reduction_code;

                bool orphaned = (parallel_nesting == 0);
                if (orphaned)
                {
                    reduction_code = get_critical_reduction_code(reduction_references);
                }
                else
                {
                    reduction_code = get_noncritical_inlined_reduction_code(reduction_references,
                            private_declarations);
                }

                Source sections_source;
                sections_source 
                    << "{"
                    <<    private_declarations
                    <<    loop_distribution_code
                    <<    lastprivate_code
                    <<    loop_finalization
                    <<    reduction_code
                    << "}"
                    ;

                num_sections_stack.pop();

                AST_t sections_tree = sections_source.parse_statement(sections_construct.get_scope(),
                        sections_construct.get_scope_link());

                sections_construct.get_ast().replace(sections_tree);
            }

			void master_postorder(OpenMP::MasterConstruct master_construct)
			{
				Source master_source;

				Statement statement = master_construct.body();

				master_source
					<< "if (in__tone_is_master_())"
					<< "{"
					<<    statement.prettyprint()
					<< "}"
					;

				AST_t master_tree = master_source.parse_statement(master_construct.get_scope(),
						master_construct.get_scope_link());

				master_construct.get_ast().replace(master_tree);
			}

			void ordered_postorder(OpenMP::OrderedConstruct ordered_construct)
			{
				IdExpression induction_var = induction_var_stack.top();

				Statement construct_body = ordered_construct.body();
				Source ordered_source;

				ordered_source
					<< "{"
					<<   "in__tone_enter_ordered_ (& "<< induction_var.prettyprint() << ");"
					<<   construct_body.prettyprint()
					<<   "in__tone_leave_ordered_ (&" << induction_var.prettyprint() << ");"
					<< "}"
					;

                AST_t ordered_code = ordered_source.parse_statement(ordered_construct.get_scope(),
                        ordered_construct.get_scope_link());

                ordered_construct.get_ast().replace(ordered_code);
			}

            void for_preorder(OpenMP::ForConstruct for_construct)
			{
                Statement construct_body = for_construct.body();
                // The construct is in fact a ForStatement in a #pragma omp parallel do
                ForStatement for_statement(construct_body);
				
                IdExpression induction_var = for_statement.get_induction_variable();

				// Save this induction var in the stack
				induction_var_stack.push(induction_var);
			}
            
            void for_postorder(OpenMP::ForConstruct for_construct)
            {
                OpenMP::Directive directive = for_construct.directive();
                ForStatement for_statement = for_construct.body();
                Statement loop_body = for_statement.get_loop_body();

				// Remove the induction var from the stack
				induction_var_stack.pop();

                // They will hold the entities as they appear in the clauses
                ObjectList<IdExpression> shared_references;
                ObjectList<IdExpression> private_references;
                ObjectList<IdExpression> firstprivate_references;
                ObjectList<IdExpression> lastprivate_references;
                ObjectList<OpenMP::ReductionIdExpression> reduction_references;
                
                // Get the enclosing function definition
                FunctionDefinition function_definition = for_construct.get_enclosing_function();
                // its scope
                Scope function_scope = function_definition.get_scope();
                
                // Get the data attributes for every entity
                get_data_explicit_attributes(function_scope,
                        directive,
                        loop_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);
                
                // The induction variable deserves special treatment
                IdExpression induction_var = for_statement.get_induction_variable();
                // If private_references does not contain an IdExpression whose
                // related symbol is the induction variable symbol, then
                // privatize here (FIXME: I've seen codes where the induction
                // variable appears in lastprivate)
                if (!private_references.contains(functor(&IdExpression::get_symbol), induction_var.get_symbol()))
                {
                    // Add the induction variale onto the private references
                    private_references.append(induction_var);
                    // And now remove, if it was already there, any reference
                    // to the induction variable symbol from the set of shared
                    // references
                    shared_references = shared_references.not_find(functor(&IdExpression::get_symbol), 
                            induction_var.get_symbol());
                }

                // The lists of entities passed by pointer and entities
                // privatized in the outline
                ObjectList<IdExpression> pass_by_pointer;
                ObjectList<IdExpression> privatized_entities;
                // Create the replacement map and the pass_by_pointer set
                ReplaceIdExpression replace_references = 
                    set_replacements(function_definition,
                            directive,
                            loop_body,
                            shared_references,
                            private_references,
                            firstprivate_references,
                            lastprivate_references,
                            reduction_references,
                            pass_by_pointer,
                            privatized_entities);

                Source parallel_for_body;
                
                Source private_declarations = get_privatized_declarations(privatized_entities, pass_by_pointer, 
                        firstprivate_references, reduction_references);

                Source loop_distribution_code = get_loop_distribution_code(for_statement,
                        replace_references);

                Source lastprivate_code;

                if (!lastprivate_references.empty())
                {
                    Source lastprivate_assignments = get_lastprivate_assignments(lastprivate_references, 
                            pass_by_pointer);

                    lastprivate_code
                        << "if (intone_last != 0)"
                        << "{"
                        <<    lastprivate_assignments
                        << "}"
                        ;
                }

                OpenMP::Clause nowait_clause = directive.nowait_clause();

                Source loop_finalization = get_loop_finalization(/*do_barrier=*/!(nowait_clause.is_defined()));

                Source reduction_code;

                parallel_for_body
                    << "{"
                    <<    private_declarations
                    <<    loop_distribution_code
                    <<    lastprivate_code
                    <<    loop_finalization
                    <<    reduction_code
                    << "}"
                    ;

                bool orphaned = (parallel_nesting == 0);
                if (orphaned)
                {
                    reduction_code = get_critical_reduction_code(reduction_references);
                }
                else
                {
                    reduction_code = get_noncritical_inlined_reduction_code(reduction_references,
                            private_declarations);
                }

                AST_t result;
                result = parallel_for_body.parse_statement(loop_body.get_scope(), 
                         loop_body.get_scope_link());

                for_construct.get_ast().replace(result);
            }

            AST_t get_parallel_spawn_code(
					FunctionDefinition function_definition,
                    Scope scope,
                    ScopeLink scope_link,
                    Source outlined_function_name,
                    ObjectList<IdExpression> pass_by_pointer,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references,
                    OpenMP::Clause num_threads_clause,
                    OpenMP::CustomClause groups_clause)
            {
                Source spawn_code;
                Source reduction_vectors;
                Source groups_definition;
                Source source_num_parameters;
                Source referenced_parameters;

                Source reduction_code;

                // The skeleton of the spawn code will be this one
                spawn_code
                    << "{"
                    << "  int nth_nprocs;"
                    << "  nth_desc *nth_selfv;"
                    << "  int nth_arg;"
                    << "  nth_argdesc_t nth_mask;"
                    << "  int nth_num_params;"
                    << "  int nth_p;"
//                    << "  extern struct nth_desc *nthf_self_();"
//                    << "  extern void nthf_team_set_nplayers_(int *);"
//                    << "  extern void nthf_create_1s_vp_(void (*)(), int *, int *, struct nth_desc **, unsigned long long *, int *, ...);"
//                    << "  extern void nthf_block_();"
                    <<    reduction_vectors
                    <<    groups_definition
                    << "  nth_selfv = nthf_self_();"
                    << "  nthf_team_set_nplayers_ (&nth_nprocs);"
                    << "  nth_arg = 0;"
                    << "  nth_mask = (nth_argdesc_t)(~0);"
                    << "  nth_num_params = 0;"
                    <<    source_num_parameters
                    << "  for (nth_p = 0; nth_p < nth_nprocs; nth_p++)"
                    << "  {"
                    << "     nthf_create_1s_vp_((void(*)())(" << outlined_function_name << "), &nth_arg, &nth_p, &nth_selfv, "
                    << "        &nth_mask, &nth_num_params " << referenced_parameters << ");"
                    << "  }"
                    << "  nthf_block_();"
                    <<    reduction_code
                    << "}"
                    ;

				// FIXME - This is IA32 specific
                // Reduction vectors
                //
                // For every entity in the reduction_references list
                for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                        it != reduction_references.end();
                        it++)
                {
                    // create a reduction vector after the name of the mangled entity
                    std::string reduction_vector_name = "rdv_" + it->get_id_expression().mangle_id_expression();

                    // get its type
                    Symbol reduction_symbol = it->get_symbol();
                    Type reduction_type = reduction_symbol.get_type();

                    // create a tree of expression 64
                    // FIXME: hardcoded to 64 processors
                    Source array_length;
                    array_length << "64";
                    AST_t array_length_tree = array_length.parse_expression(it->get_id_expression().get_scope());

                    // and get an array of 64 elements
                    Type reduction_vector_type = reduction_type.get_array_to(array_length_tree, 
                            it->get_id_expression().get_scope());

                    // now get the code that declares this reduction vector
                    reduction_vectors
                        << reduction_vector_type.get_declaration(it->get_id_expression().get_scope(), 
								reduction_vector_name) << ";";

                    // And add to the list of referenced parameters
                    referenced_parameters << ", " << reduction_vector_name;

                    source_num_parameters << "nth_num_params += "
                        << "((sizeof(" << reduction_vector_name << ") % 4) == 0) "
                        <<   "? sizeof(" << reduction_vector_name << ")"
                        <<   ": (sizeof(" << reduction_vector_name << ") + (4 - (sizeof(" << reduction_vector_name << ") % 4)));"
                        ;
                }
                
                // Referenced parameters
                //
				// "this" if needed
				if (is_nonstatic_member_function(function_definition))
				{
                    referenced_parameters << ", this";

                    source_num_parameters << "nth_num_params += "
                        << "((sizeof(this) % 4) == 0) "
                        <<   "? sizeof(this)"
                        <<   ": (sizeof(this) - (4 - (sizeof(this) % 4)));"
                        ;
				}

                // For every entity in list "pass_by_pointer"
                for (ObjectList<IdExpression>::iterator it = pass_by_pointer.begin();
                        it != pass_by_pointer.end();
                        it++)
                {
                    // Simply pass its reference (its address)
                    referenced_parameters << ", &" << it->prettyprint();

                    source_num_parameters << "nth_num_params += "
                        << "((sizeof(&" << it->prettyprint() << ") % 4) == 0) "
                        <<   "? sizeof(&" << it->prettyprint() << ")"
                        <<   ": (sizeof(&" << it->prettyprint() << ") - (4 - (sizeof(&" << it->prettyprint() << ") % 4)));"
                        ;
                }

				source_num_parameters
					<< "nth_num_params /= 4;"
					;
                
                // Groups definition
                if (!groups_clause.is_defined() && !num_threads_clause.is_defined())
                {
                    groups_definition 
//                        << "extern int nthf_cpus_actual_();"

                        << "nth_nprocs =  nthf_cpus_actual_();"
                        ;
                }
                else if (num_threads_clause.is_defined())
                {
                    ObjectList<Expression> clause_exprs = num_threads_clause.get_expression_list();

                    std::string num_threads_value = clause_exprs[0].prettyprint();
                    groups_definition 
//                        << "extern void nthf_compute_uniform_groups_(int*);"
                        // << "int nth_num_threads = " << num_threads_value << ";"

                        // << "nthf_compute_uniform_groups_(&nth_num_threads);"
                        // << "nth_nprocs = nth_num_threads;"
                        << "nth_nprocs =" << num_threads_value << ";"
                        ;
                }
                else /* groups is defined */
                {
                    groups_definition << "int nth_groups_num;"
                        ;

                    ObjectList<Expression> groups_expressions = groups_clause.get_expression_list();

                    switch (groups_expressions.size())
                    {
                        case 1 :
                            {
                                std::string num_groups = groups_expressions[0].prettyprint();

                                groups_definition 
//                                    << "extern void nthf_compute_uniform_groups_(int*);"

                                    << "nth_groups_num = " << num_groups << ";"
                                    << "nthf_compute_uniform_groups_(&nthf_groups_num);"
                                    ;
                                break;
                            }
                        case 2 :
                            {
                                std::string num_groups = groups_expressions[0].prettyprint();
                                std::string howmany_groups = groups_expressions[1].prettyprint();

                                groups_definition
//                                    << "extern void nthf_compute_groups_vec_(int*, int*);"

                                    << "nth_groups_num = " << num_groups << ";"
                                    << "nthf_compute_groups_vec_(&nthf_groups_num, " << howmany_groups << ");"
                                    ;
                        
                                break;
                            }
                        case 3 :
                            {
                                std::string num_groups = groups_expressions[0].prettyprint();
                                std::string who_groups = groups_expressions[1].prettyprint();
                                std::string howmany_groups = groups_expressions[2].prettyprint();

                                groups_definition
//                                    << "extern void nthf_define_groups_(int*, int*, int*);"

                                    << "nth_groups_num = " << num_groups << ";"
                                    << "nthf_define_groups_(&nthf_groups_num, " << who_groups << ", " << howmany_groups << ");"
                                    ;

                                break;
                            }
                        default:
                            break;
                    }

                    groups_definition
                        << "nth_nprocs = nth_groups_num;"
                        ;
                }
                
                // Reduction code
                //
                // If there is any reduction reference
                reduction_code = get_noncritical_reduction_code(reduction_references);
                
                // std::cerr << "CODI SPAWN" << std::endl;
                // std::cerr << spawn_code.get_source(true) << std::endl;
                // std::cerr << "End CODI SPAWN" << std::endl;
                
                // Parse the spawn code and return it
                AST_t result = spawn_code.parse_statement(scope, scope_link);
                return result;
            }

            Source get_critical_reduction_code(ObjectList<OpenMP::ReductionIdExpression> reduction_references)
            {
                Source reduction_code;

                if (reduction_references.empty())
                {
                    // Nothing to do if the reduction set is empty
                    return reduction_code;
                }

                Source reduction_gathering;

                reduction_code
                    << "{"
                    <<    "static nth_word_t default_mutex;"
//                    <<    "extern nthf_spin_lock_(void*);"
//                    <<    "extern nthf_spin_unlock_(void*);"
                    <<    "int rdv_i;"

                    <<    "nthf_spin_lock_(&default_mutex);"
                    <<    "for (rdv_i = 0; rdv_i < nth_nprocs; rdv_i++)"
                    <<    "{"
                    <<       reduction_gathering
                    <<    "}"
                    <<    "nthf_spin_unlock_(&default_mutex);"
                    << "}"
                    ; 

                for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                        it != reduction_references.end();
                        it++)
                {
                    // get the operator involved
                    std::string reduced_var_name = it->get_id_expression().mangle_id_expression();
                    std::string reduction_var_name = "p_" + it->get_id_expression().mangle_id_expression();

                    std::string op = it->get_operation().prettyprint();

                    reduction_gathering 
                        << reduced_var_name << " = " << reduced_var_name << op << reduction_var_name << ";"
                        ;
                }

                return reduction_code;
            }


            Source get_noncritical_reduction_code(ObjectList<OpenMP::ReductionIdExpression> reduction_references)
            {
                Source reduction_code;

                if (reduction_references.empty())
                {
                    return reduction_code;
                }

                // Create the source code that gathers the values computed by every thread
                Source reduction_gathering;

                reduction_code
					<< "int rdv_i;"
                    << "for (rdv_i = 0; rdv_i < nth_nprocs; rdv_i++)"
                    << "{"
                    <<    reduction_gathering
                    << "}"
                    ;

                Source reduction_gethering;

                reduction_gathering = get_reduction_gathering(reduction_references);

                return reduction_code;
            }

            Source get_noncritical_inlined_reduction_code(
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references,
                    Source& private_declarations)
            {
                Source reduction_code;

				if (reduction_references.empty())
				{
					return reduction_code;
				}

                for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                        it != reduction_references.end();
                        it++)
                {
                    // create a reduction vector after the name of the mangled entity
                    std::string reduction_vector_name = "rdv_" + it->get_id_expression().mangle_id_expression();

                    // get its type
                    Symbol reduction_symbol = it->get_symbol();
                    Type reduction_type = reduction_symbol.get_type();

                    // create a tree of expression 64
                    // FIXME: hardcoded to 64 processors
                    Source array_length;
                    array_length << "64";
                    AST_t array_length_tree = array_length.parse_expression(it->get_id_expression().get_scope());

                    // and get an array of 64 elements
                    Type reduction_vector_type = reduction_type.get_array_to(array_length_tree, 
                            it->get_id_expression().get_scope());

                    // now get the code that declares this reduction vector, and add it to the private_declarations
                    private_declarations
                        << reduction_vector_type.get_declaration(
								it->get_id_expression().get_scope(), reduction_vector_name) 
						<< ";";
                }

                Source reduction_update;
                Source reduction_gathering;

                reduction_code
                    << reduction_update
//                    << "extern void in__tone_barrier_();"
//                    << "extern char in__tone_is_master_();"

                    << "in__tone_barrier_();"
                    << "if (in__tone_is_master_())"
                    << "{"
                    <<    "int rdv_i;"
//                    <<    "extern int nthf_cpus_actual_();"

                    <<    "int nth_nprocs = nthf_cpus_actual_();"
                    <<    "for (rdv_i = 0; rdv_i < nth_nprocs; rdv_i++)"
                    <<    "{"
                    <<       reduction_gathering
                    <<    "}"
                    << "}"
                    ;

                reduction_update = get_reduction_update(reduction_references);
                reduction_gathering = get_reduction_gathering(reduction_references);

                return reduction_code;
            }

            Source get_reduction_update(ObjectList<OpenMP::ReductionIdExpression> reduction_references)
			{
				Source reduction_update;

				if (reduction_references.empty())
				{
					return reduction_update;
				}

				reduction_update 
					<< "{"
					//                        <<    "extern int in__tone_thread_id_ ();"
					<<    "int nth_thread_id = in__tone_thread_id_();"
					;

				for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
						it != reduction_references.end();
						it++)
				{
					reduction_update
						<< "rdv_" << it->get_id_expression().mangle_id_expression() << "[nth_thread_id] = "
						<< "p_" << it->get_id_expression().mangle_id_expression() << ";";
				}

				reduction_update
					<< "}"
					;


				return reduction_update;
			}

            Source get_reduction_gathering(ObjectList<OpenMP::ReductionIdExpression> reduction_references)
            {
                Source reduction_gathering;

                // For every entity being reduced
                for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                        it != reduction_references.end();
                        it++)
                {
                    // And reduce for this element of the reduction vector
					if (!it->is_user_defined())
					{
						// If it is not a user defined one it is easy

						// Construct the name of its related reduction vector
						std::string reduced_var_name = it->get_id_expression().prettyprint();
						std::string reduction_vector_name = "rdv_" + it->get_id_expression().mangle_id_expression();

						// get the operator involved
						std::string op = it->get_operation().prettyprint();
						reduction_gathering
							<< reduced_var_name << " = " << reduced_var_name << op << reduction_vector_name << "[rdv_i]" << ";";
					}
					else
					{
						Source one_urd_reduction = get_one_user_defined_gathering(*it);
						reduction_gathering << one_urd_reduction;
					}
                }

                return reduction_gathering;
            }

			Source get_one_user_defined_gathering(OpenMP::ReductionIdExpression reduction_id_expr)
			{
				IdExpression reductor = reduction_id_expr.get_user_defined_reductor();
				Symbol reductor_symbol = reductor.get_symbol();

				Type reductor_type = reductor_symbol.get_type();

				if (!reductor_type.is_function())
				{
					std::cerr << "User defined reduction in " 
						<< reductor.get_ast().get_locus() << " does not refer a function. Ignoring" << std::endl;
					return Source("");
				}
				
				// Construct the name of its related reduction vector
				std::string reduced_var_name = reduction_id_expr.get_id_expression().prettyprint();
				std::string reduction_vector_name = "rdv_" + reduction_id_expr.get_id_expression().mangle_id_expression();

				Source reduction_gathering;

				// FIXME - For C++ this is more difficult. Currently not implemented
				// Extract the unqualified part of the id-expression
				// and if it is a member construct a member-access with function call
				//
				// The id-expression
				//
				// Lets "happily" assume that if the reductor returns void is of the form
				//
				//    void f(T*, T);
				//    void f(T&, T);
				//
				// otherwise we will assume it is of type 
				//
				//    T f(T, T);
				//
				if (reductor_type.returns().is_void())
				{
					// If the first parameter is a pointer we will assume that the reductor is of this form
					//
					//    void f(T*, t);
					//
					// otherwise it will be assumed to be
					//
					//    void f(T&, t);
					//
					ObjectList<Type> parameters = reductor_type.parameters();

					if (parameters[0].is_pointer())
					{
						reduction_gathering
							<< reductor.prettyprint() << "(&" << reduced_var_name << "," << reduction_vector_name << "[rdv_i]" << ");";
					}
					else
					{
						reduction_gathering
							<< reductor.prettyprint() << "(" << reduced_var_name << "," << reduction_vector_name << "[rdv_i]" << ");";
					}
				}
				else
				{
					reduction_gathering
						<< reduced_var_name << " = " << reductor.prettyprint() << "(" << reduced_var_name << "," << reduction_vector_name << "[rdv_i]" << ");";
				}

				return reduction_gathering;
			}

			Source get_member_function_declaration(
					FunctionDefinition function_definition,
                    Source outlined_function_name,
                    ObjectList<IdExpression> pass_by_pointer,
                    ObjectList<IdExpression> pass_by_value,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references
					)
			{
				Source result;

				Source formal_parameters;

				result
					<< "static void " << outlined_function_name << "(" << formal_parameters << ");"
					;

				formal_parameters = get_formal_parameters(function_definition, 
						pass_by_pointer, pass_by_value, reduction_references);

				return result;
			}

            Source get_outline_common(
					FunctionDefinition function_definition,
                    Source& specific_body,
                    Source outlined_function_name,
                    ObjectList<IdExpression> pass_by_pointer,
                    ObjectList<IdExpression> pass_by_value,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references
                    )
            {
                Source formal_parameters;
                Source reduction_code;

				Source static_qualifier;

				Source forward_declaration;

                Source result;
                result
					<< forward_declaration
					<< static_qualifier
                    << "void " << outlined_function_name << "(" << formal_parameters << ")"
                    << "{"
                    <<    specific_body
                    << "}"
                    ;

				IdExpression function_name = function_definition.get_function_name();
				Symbol function_symbol = function_name.get_symbol();

				// If the function is a member and is not qualified we need an additional
				// static here
				if (function_symbol.is_member() 
						&& !function_name.is_qualified())
				{
					static_qualifier << "static ";
				}

				formal_parameters = get_formal_parameters(
						function_definition, 
						pass_by_pointer, 
						pass_by_value, 
						reduction_references);

				// We want to forward the declaration
				if (!function_symbol.is_member())
				{
					Declaration point_of_decl = function_name.get_declaration();
					DeclarationSpec decl_specs = point_of_decl.get_declaration_specifiers();
					ObjectList<DeclaredEntity> declared_entities = point_of_decl.get_declared_entities();
					DeclaredEntity declared_entity = *(declared_entities.begin());

					forward_declaration 
						<< decl_specs.prettyprint()
						<< " "
						<< declared_entity.prettyprint()
						<< ";";
				}

                return result;
            }

			Source get_formal_parameters(
					FunctionDefinition function_definition,
					ObjectList<IdExpression> pass_by_pointer,
					ObjectList<IdExpression> pass_by_value,
					ObjectList<OpenMP::ReductionIdExpression> reduction_references)
			{
				Source formal_parameters;

				// Add _this if needed

				if (is_nonstatic_member_function(function_definition))
				{
					IdExpression function_name = function_definition.get_function_name();
					Symbol function_symbol = function_name.get_symbol();

					Type class_type = function_symbol.get_class_type();
					Type pointer_to_class = class_type.get_pointer_to();

					formal_parameters.append_with_separator(
							// Fix this scope
							pointer_to_class.get_declaration(function_name.get_scope(), "_this"), 
							",");
				}

                // Reduction vectors are passed first by "value" (there is no
                // need to pass by pointer something that was already passed as
                // a pointer)
                for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                        it != reduction_references.end();
                        it++)
                {
                    std::string reduction_vector_name = "rdv_" + it->get_id_expression().mangle_id_expression();

                    Symbol sym = it->get_symbol();
                    Type type = sym.get_type();
                    Type pointer_type = type.get_pointer_to();
                    
                    formal_parameters.append_with_separator(
							pointer_type.get_declaration(it->get_id_expression().get_scope(), reduction_vector_name), 
							",");
                }

                // Formal parameters, basically pass_by_pointer things
                // (and sometimes something else)
                for (ObjectList<IdExpression>::iterator it = pass_by_pointer.begin();
                        it != pass_by_pointer.end();
                        it++)
                {
                    Symbol sym = it->get_symbol();
                    Type type = sym.get_type();
                    Type pointer_type = type.get_pointer_to();

                    // Get a declaration of the mangled name of the id-expression
                    formal_parameters.append_with_separator(
							pointer_type.get_declaration(it->get_scope(), it->mangle_id_expression())
							, ",");
                }

                for (ObjectList<IdExpression>::iterator it = pass_by_value.begin();
                        it != pass_by_value.end();
                        it++)
                {
                    Symbol sym = it->get_symbol();
                    Type type = sym.get_type();

                    // Get a declaration of the mangled name of the id-expression
                    formal_parameters.append_with_separator(
							type.get_declaration(it->get_scope(), it->mangle_id_expression()), 
							",");
                }

				return formal_parameters;
			}

            AST_t get_outline_parallel(
                    FunctionDefinition function_definition,
                    Source outlined_function_name,
                    Statement construct_body,
                    ReplaceIdExpression replace_references,
                    ObjectList<IdExpression> pass_by_pointer,
                    ObjectList<IdExpression> privatized_entities,
                    ObjectList<IdExpression> firstprivate_references,
                    ObjectList<IdExpression> lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references
                    )
            {
                ObjectList<IdExpression> pass_by_value;

                Source outline_parallel;
                Source parallel_body;
                Source empty;

                outline_parallel = get_outline_common(
						function_definition,
                        parallel_body, // The body of the outline
                        outlined_function_name,
                        pass_by_pointer,
                        pass_by_value,
                        reduction_references);
                
                // Replace references using set "replace_references" over construct body
                Statement modified_parallel_body_stmt = replace_references.replace(construct_body);

                Source private_declarations = get_privatized_declarations(privatized_entities, pass_by_pointer, 
                        firstprivate_references, reduction_references);

                Source reduction_update = get_reduction_update(reduction_references);
                
                parallel_body 
                    << private_declarations
                    << modified_parallel_body_stmt.prettyprint()
                    << reduction_update
//                    << "extern void nthf_task_block_(void);"
                    << "nthf_task_block_();"
                    ;

                // std::cerr << "CODI OUTLINE" << std::endl;
                // std::cerr << outline_parallel.get_source(true) << std::endl;
                // std::cerr << "End CODI OUTLINE" << std::endl;

				IdExpression function_name = function_definition.get_function_name();
				Symbol function_symbol = function_name.get_symbol();

				// If the function is a member and is qualified (therefore the
				// function definition is outside the class) we have to create
				// an additional declaration for the new member
				if (function_symbol.is_member() 
						&& function_name.is_qualified())
				{
					Source outline_function_decl = get_outlined_function_name(function_name, /*qualified=*/false);

					Source member_declaration = get_member_function_declaration(
							function_definition,
							outline_function_decl,
							pass_by_pointer,
							pass_by_value,
							reduction_references);

					Declaration decl = function_name.get_declaration();

					Scope class_scope = decl.get_scope();
					Type class_type = function_symbol.get_class_type();
					AST_t member_decl_tree = member_declaration.parse_member(decl.get_scope(), decl.get_scope_link(), class_type);

					decl.get_ast().append(member_decl_tree);
				}

                AST_t result;

                result = outline_parallel.parse_global(function_definition.get_scope(), 
                         function_definition.get_scope_link());

                return result;
            }

            AST_t get_outline_task(
                    FunctionDefinition function_definition,
                    Source outlined_function_name,
                    Statement construct_body,
                    ReplaceIdExpression replace_references,
                    ObjectList<IdExpression> local_entities,
                    ObjectList<IdExpression> pass_by_pointer,
                    ObjectList<IdExpression> pass_by_value
                    )
            {
                ObjectList<OpenMP::ReductionIdExpression> reduction_references;
                ObjectList<IdExpression> firstprivate_references;

                Source outline_parallel;
                Source parallel_body;
                Source empty;

                outline_parallel = get_outline_common(
						function_definition,
                        parallel_body, // The body of the outline
                        outlined_function_name,
                        pass_by_pointer,
                        pass_by_value,
                        reduction_references);
                
                // Replace references using set "replace_references" over construct body
                Statement modified_parallel_body_stmt = replace_references.replace(construct_body);

                Source private_declarations = get_privatized_declarations(local_entities, pass_by_pointer, 
                        firstprivate_references, reduction_references);

                parallel_body 
                    << private_declarations
                    << modified_parallel_body_stmt.prettyprint()
                    ;

                AST_t result;

                result = outline_parallel.parse_global(function_definition.get_scope(), 
                         function_definition.get_scope_link());

                return result;
            }

            // Create outline for parallel sections
            AST_t get_outline_parallel_sections(
                    FunctionDefinition function_definition,
                    Source outlined_function_name, 
                    Statement construct_body,
                    ReplaceIdExpression replace_references,
                    ObjectList<IdExpression> pass_by_pointer,
                    ObjectList<IdExpression> privatized_entities,
                    ObjectList<IdExpression> firstprivate_references,
                    ObjectList<IdExpression> lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references)
            {
                ObjectList<IdExpression> pass_by_value;

                Source outline_parallel_sections;
                Source parallel_sections_body;
                
                // Get the source of the common parallel X outline
                outline_parallel_sections = get_outline_common(
						function_definition,
                        parallel_sections_body,
                        outlined_function_name,
                        pass_by_pointer,
                        pass_by_value,
                        reduction_references);

                Source private_declarations = get_privatized_declarations(privatized_entities, pass_by_pointer,
                        firstprivate_references, reduction_references);

                Source loop_distribution;

                int num_sections = num_sections_stack.top();

                loop_distribution = get_loop_distribution_in_sections(num_sections,
                        construct_body,
                        replace_references
                        );

                Source lastprivate_code;

                if (!lastprivate_references.empty())
                {
                    Source lastprivate_assignments = get_lastprivate_assignments(lastprivate_references, 
                            pass_by_pointer);

                    lastprivate_code
                        << "if (intone_last != 0)"
                        << "{"
                        <<    lastprivate_assignments
                        << "}"
                        ;
                }
                
                // Barrier is already done at parallel level
                Source loop_finalization = get_loop_finalization(/* do_barrier = */ false);

                Source reduction_update = get_reduction_update(reduction_references);

                parallel_sections_body 
                    << private_declarations
                    << loop_distribution
                    << lastprivate_code
                    << reduction_update
                    << loop_finalization
//                    << "extern void nthf_task_block_(void);"
                    << "nthf_task_block_();"
                    ;

                // std::cerr << "CODI OUTLINE PARALLEL SECTIONS" << std::endl;
                // std::cerr << outline_parallel_sections.get_source(true) << std::endl;
                // std::cerr << "End CODI OUTLINE PARALLEL SECTIONS" << std::endl;

                AST_t result;

                result = outline_parallel_sections.parse_global(function_definition.get_scope(), 
                        function_definition.get_scope_link());

                return result;
            }

            AST_t get_outline_parallel_single(
                    FunctionDefinition function_definition,
                    Source outlined_function_name,
                    Statement construct_body,
                    ReplaceIdExpression replace_references,
                    ObjectList<IdExpression> pass_by_pointer,
                    ObjectList<IdExpression> privatized_entities,
                    ObjectList<IdExpression> firstprivate_references,
                    ObjectList<IdExpression> lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references
                    )
            {
                ObjectList<IdExpression> pass_by_value;

                Source outline_parallel;
                Source parallel_body;
                Source empty;

                outline_parallel = get_outline_common(
						function_definition,
                        parallel_body, // The body of the outline
                        outlined_function_name,
                        pass_by_pointer,
                        pass_by_value,
                        reduction_references);
                
                // Replace references using set "replace_references" over construct body
                Statement modified_parallel_body_stmt = replace_references.replace(construct_body);

                Source private_declarations = get_privatized_declarations(privatized_entities, pass_by_pointer, 
                        firstprivate_references, reduction_references);

                Source reduction_update = get_reduction_update(reduction_references);

				Source single_source;

                single_source
                    << "{"
                    <<   "int nth_low;"
                    <<   "int nth_upper;"
                    <<   "int nth_step;"
                    <<   "int nth_chunk;"
                    <<   "int nth_schedule;"
                    <<   "int nth_dummy1;"
                    <<   "int nth_dummy2;"
                    <<   "int nth_dummy3;"
                    <<   "int nth_barrier; "

                    <<   "nth_low = 0;"
                    <<   "nth_upper = 0;"
                    <<   "nth_step = 1;"
                    <<   "nth_schedule = 1;"
                    <<   "nth_chunk = 1;"

//                    <<   "extern void in__tone_begin_for_(int*, int*, int*, int*, int*);"
//                    <<   "extern int in__tone_next_iters_(int*, int*, int*);"
//                    <<   "extern void in__tone_end_for_(int*);"

                    <<   "in__tone_begin_for_ (&nth_low, &nth_upper, &nth_step, &nth_chunk, &nth_schedule);"
                    <<   "while (in__tone_next_iters_ (&nth_dummy1, &nth_dummy2, &nth_dummy3) != 0)"
                    <<   "{"
                    <<       modified_parallel_body_stmt.prettyprint()
                    <<   "}"
                    << "}"
                    ;
                
                parallel_body 
                    << private_declarations
                    << single_source
                    << "nthf_task_block_();"
                    ;

                // std::cerr << "CODI OUTLINE" << std::endl;
                // std::cerr << outline_parallel.get_source(true) << std::endl;
                // std::cerr << "End CODI OUTLINE" << std::endl;

                AST_t result;

                result = outline_parallel.parse_global(function_definition.get_scope(), 
                         function_definition.get_scope_link());

                return result;
            }


            // Create outline for parallel for
            AST_t get_outline_parallel_for(
                    FunctionDefinition function_definition,
                    Source outlined_function_name,
                    ForStatement for_statement,
                    Statement loop_body,
                    ReplaceIdExpression replace_references,
                    ObjectList<IdExpression> pass_by_pointer,
                    ObjectList<IdExpression> privatized_entities,
                    ObjectList<IdExpression> firstprivate_references,
                    ObjectList<IdExpression> lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references
                    )
            {
                // empty
                ObjectList<IdExpression> pass_by_value;

                Source empty;
                Source outline_parallel_for;
                Source parallel_for_body;

                // Get the source of the common parallel X outline
                outline_parallel_for = get_outline_common(
						function_definition,
                        parallel_for_body,
                        outlined_function_name,
                        pass_by_pointer,
                        pass_by_value,
                        reduction_references);

                Source private_declarations = get_privatized_declarations(privatized_entities, pass_by_pointer, 
                        firstprivate_references, reduction_references);

                Source loop_distribution = get_loop_distribution_code(for_statement, replace_references);

                Source lastprivate_code;

                if (!lastprivate_references.empty())
                {
                    Source lastprivate_assignments = get_lastprivate_assignments(lastprivate_references, 
                            pass_by_pointer);

                    lastprivate_code
                        << "if (intone_last != 0)"
                        << "{"
                        <<    lastprivate_assignments
                        << "}"
                        ;
                }

                // Barrier is already done at parallel level
                Source loop_finalization = get_loop_finalization(/* do_barrier = */ false);

                Source reduction_update = get_reduction_update(reduction_references);

                parallel_for_body 
                    << private_declarations
                    << loop_distribution
                    << lastprivate_code
                    << reduction_update
                    << loop_finalization
//                    << "extern void nthf_task_block_(void);"
                    << "nthf_task_block_();"
                    ;

                // std::cerr << "CODI OUTLINE PARALLEL FOR" << std::endl;
                // std::cerr << outline_parallel_for.get_source(true) << std::endl;
                // std::cerr << "End CODI OUTLINE PARALLEL FOR" << std::endl;

                AST_t result;

                result = outline_parallel_for.parse_global(function_definition.get_scope(), 
                        function_definition.get_scope_link());

                return result;
            }

            void get_data_explicit_attributes(
                    Scope function_scope,
                    OpenMP::Directive directive,
                    Statement construct_body,
                    ObjectList<IdExpression>& shared_references,
                    ObjectList<IdExpression>& private_references,
                    ObjectList<IdExpression>& firstprivate_references,
                    ObjectList<IdExpression>& lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression>& reduction_references)
            {
                // Get references in shared clause
                OpenMP::Clause shared_clause = directive.shared_clause();
                shared_references = shared_clause.id_expressions();

                // Get references in private_clause
                OpenMP::Clause private_clause = directive.private_clause();
                private_references = private_clause.id_expressions();

                // Get references in firstprivate clause
                OpenMP::Clause firstprivate_clause = directive.firstprivate_clause();
                firstprivate_references = firstprivate_clause.id_expressions();

                // Get references in lastprivate clause
                OpenMP::Clause lastprivate_clause = directive.lastprivate_clause();
                lastprivate_references = lastprivate_clause.id_expressions();

                // Get references in reduction clause
                OpenMP::ReductionClause reduction_clause = directive.reduction_clause();
                reduction_references = reduction_clause.id_expressions();
            }


            void get_data_attributes(
                    Scope function_scope,
                    OpenMP::Directive directive,
                    Statement construct_body,
                    ObjectList<IdExpression>& shared_references,
                    ObjectList<IdExpression>& private_references,
                    ObjectList<IdExpression>& firstprivate_references,
                    ObjectList<IdExpression>& lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression>& reduction_references)
            {
                get_data_explicit_attributes(
                        function_scope,
                        directive,
                        construct_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references);

                OpenMP::DefaultClause default_clause = directive.default_clause();

                // Everything should have been tagged by the user
                if (default_clause.is_none())
                    return;

                // Get every non local reference: this is, not defined in the
                // construct itself, but visible at the point where the
                // construct is defined
                ObjectList<IdExpression> non_local_symbols = construct_body.non_local_symbol_occurrences(Statement::ONLY_VARIABLES);

                // Filter in any of the private sets. We don't want any
                // id-expression whose related symbol appears in any
                // id-expression of shared, private, firstprivate lastprivate
                // or reduction
                non_local_symbols = non_local_symbols.filter(not_in_set(shared_references, functor(&IdExpression::get_symbol)));
                non_local_symbols = non_local_symbols.filter(not_in_set(private_references, functor(&IdExpression::get_symbol)));
                non_local_symbols = non_local_symbols.filter(not_in_set(firstprivate_references, functor(&IdExpression::get_symbol)));
                non_local_symbols = non_local_symbols.filter(not_in_set(lastprivate_references, functor(&IdExpression::get_symbol)));

                // Get every id-expression related to the ReductionIdExpression list
                ObjectList<IdExpression> reduction_id_expressions = 
                    reduction_references.map(functor(&OpenMP::ReductionIdExpression::get_id_expression));
                non_local_symbols = non_local_symbols.filter(not_in_set(reduction_id_expressions, functor(&IdExpression::get_symbol)));

                shared_references.append(non_local_symbols);
            }

            ReplaceIdExpression set_replacements(FunctionDefinition function_definition,
                    OpenMP::Directive directive,
                    Statement construct_body,
                    ObjectList<IdExpression>& shared_references,
                    ObjectList<IdExpression>& private_references,
                    ObjectList<IdExpression>& firstprivate_references,
                    ObjectList<IdExpression>& lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression>& reduction_references,
                    ObjectList<IdExpression>& pass_by_pointer,
                    ObjectList<IdExpression>& privatized_entities)
            {
				Symbol function_symbol = function_definition.get_function_name().get_symbol();
				Scope function_scope = function_definition.get_scope();
                ReplaceIdExpression result;

                // First mix every symbol that might be shareable
                // 
                // "shareable" means that it can be passed by pointer
                // because of it being an entity not accessible from the
                // sibling function

                ObjectList<IdExpression> shareable_references;

                // There will be no repeated here
                shareable_references.append(shared_references);
                shareable_references.append(firstprivate_references);
                shareable_references.append(lastprivate_references);

                for (ObjectList<IdExpression>::iterator it = shareable_references.begin();
                        it != shareable_references.end();
                        it++)
                {
                    Symbol current_sym = it->get_symbol();

                    // This symbol already appears in "pass_by_pointer" id-expressions,
                    // ignore it
                    if (pass_by_pointer.contains(functor(&IdExpression::get_symbol), current_sym))
                        continue;

                    // From the scope of the enclosing function definition, try to get
                    // a symbol
                    Symbol global_sym = function_scope.get_symbol_from_id_expr(it->get_ast());

                    if (!global_sym.is_valid() ||
                            (global_sym != current_sym))
                    {
                        // The symbol is either not accessible or it is not the same
                        // Since it is shared it must be referenced via a pointer

                        // This must be passed by pointer
                        pass_by_pointer.append(*it);

                        // The mapping is needed only if the entity appears in
                        // shared set because every reference will have to be
                        // replaced
                        if (shared_references.contains(functor(&IdExpression::get_symbol), current_sym))
                        {
                            // First get a nice name by mangling it
                            //
                            // FIXME: There are cases we could see an unqualified identifier
                            // that due to scoping issues clashes with another unqualified identifier
                            // (e.g. different using namespaces in different block scopes)
                            std::string mangled_name = it->mangle_id_expression();

                            Source derref_name;
                            derref_name << "(*" << mangled_name << ")";

                            result.add_replacement(current_sym, derref_name);
                        }
                    }
					else if (it->is_unqualified()
							&& current_sym.is_member()
							&& (current_sym.get_class_type() == function_symbol.get_class_type()))
					{
						if (is_nonstatic_member_function(function_definition))
						{
							Source member_access;
							member_access << "(_this->" << it->prettyprint() << ")";

							result.add_replacement(current_sym, member_access);
						}
					}
                }
                
                // First mix all things that will be made private
                ObjectList<IdExpression> privatized_references;
                privatized_references.append(private_references);
                privatized_references.append(firstprivate_references);
                privatized_references.append(lastprivate_references);
                privatized_references.append(reduction_references.map(functor(&OpenMP::ReductionIdExpression::get_id_expression)));

                for (ObjectList<IdExpression>::iterator it = privatized_references.begin();
                        it != privatized_references.end();
                        it++)
                {
                    Symbol current_sym = it->get_symbol();

                    // If already has replacement, ignore
                    if (result.has_replacement(current_sym))
                        continue;

                    std::string private_mangled_name = std::string("p_") + it->mangle_id_expression();

                    // This entity is privatized
                    privatized_entities.append(*it);

                    // Create a replacement for it
                    result.add_replacement(current_sym, private_mangled_name);
                }

                return result;
            }

            Source get_loop_distribution_in_sections(
                    int num_sections,
                    Statement construct_body,
                    ReplaceIdExpression replace_references)
            {
                Source loop_distribution;
                
                // Replace references using set "replace_references" over construct body
                Statement modified_parallel_body_stmt = replace_references.replace(construct_body);

                loop_distribution 
                    << "int nth_low;"
                    << "int nth_upper;"
                    << "int nth_step;"
                    << "int nth_chunk;"
                    << "int nth_schedule;"
                    << "int intone_start;"
                    << "int intone_end;"
                    << "int intone_last;"
                    << "int nth_barrier;"
                    << "int nth_i;"

                    << "nth_low = 0;"
                    << "nth_upper = " << num_sections << ";"
                    << "nth_step = 1;"
                    << "nth_schedule = 1;"
                    << "nth_chunk = 1;"

//                    << "extern void in__tone_begin_for_(int*, int*, int*, int*, int*);"
//                    << "extern int in__tone_next_iters_(int*, int*, int*);"
//                    << "extern void in__tone_end_for_(int*);"

                    << "in__tone_begin_for_ (&nth_low, &nth_upper, &nth_step, &nth_chunk, &nth_schedule);"
                    << "while (in__tone_next_iters_ (&intone_start, &intone_end, &intone_last) != 0)"
                    << "{"
                    <<    "for (nth_i = intone_start; nth_i <= intone_end; nth_i += nth_step)"
                    <<    "{"
                    <<         "switch (nth_i)"
                    <<         "{"
                    <<            modified_parallel_body_stmt.prettyprint()
                    <<            "default: break;" 
                    <<         "}"
                    <<    "}"
                    << "}"
                    ;

                return loop_distribution;
            }

            Source get_loop_distribution_code(ForStatement for_statement,
                    ReplaceIdExpression replace_references)
            {
                Source parallel_for_body;

                Source loop_initialization;

                Source schedule_decisions;
                Source distributed_loop_body;
                Source loop_reductions;
                Source reduction_initialization;

                parallel_for_body
                    << reduction_initialization
                    << loop_initialization
                    << schedule_decisions
                    << distributed_loop_body
                    << loop_reductions
                    ;

                Statement loop_body = for_statement.get_loop_body();


                IdExpression induction_var = for_statement.get_induction_variable();
                Source induction_var_name;
                // Induction var name is handled specially
                induction_var_name << "p_" << induction_var.mangle_id_expression();

                // Define here the bounds of the loop
                loop_initialization 
                    << "int nth_low;"
                    << "int nth_upper;"
                    << "int nth_step;"
                    << "int intone_start;"
                    << "int intone_end;"
                    << "int intone_last;"
					<< "int nth_barrier;"

                    << "nth_low = " << for_statement.get_lower_bound().prettyprint() << ";"
                    << "nth_upper = " << for_statement.get_upper_bound().prettyprint() << ";"
                    << "nth_step = " << for_statement.get_step().prettyprint() << ";"
                    ;

                // Schedule decisions
                // TODO - dynamic, guided and runtime support is lacking
                schedule_decisions
                    << "int nth_schedule;"
                    << "int nth_chunk;"
                    << "nth_schedule = 0;"
                    << "nth_chunk = 0;"
                    ;

                // Loop distribution
                Source modified_loop_body;
                distributed_loop_body
//                    << "extern void in__tone_begin_for_(int*, int*, int*, int*, int*);"
//                    << "extern int in__tone_next_iters_(int*, int*, int*);"
//                    << "extern void in__tone_end_for_(int*);"

                    << "in__tone_begin_for_(&nth_low, &nth_upper, &nth_step, &nth_chunk, &nth_schedule);"

                    // Get a slice of the iteration space
                    << "while (in__tone_next_iters_(&intone_start, &intone_end, &intone_last) != 0)"
                    << "{"
                           // And do something with it
                    << "   for (" << induction_var_name << " = intone_start; "
                    << "        nth_step >= 1 ? " << induction_var_name << " <= intone_end : " << induction_var_name << ">= intone_end;"
                    << "        " << induction_var_name << " += nth_step)"
                    << "   {"
                    << "   " << modified_loop_body
                    << "   }"
                    << "}"
                    ;

                // Replace references using set "replace_references" over loop body
                Statement modified_loop_body_stmt = replace_references.replace(loop_body);
                // and get the source of the modified tree
                modified_loop_body << modified_loop_body_stmt.prettyprint();

                return parallel_for_body;
            }

            Source get_loop_finalization(bool do_barrier)
            {
                Source loop_finalization;

                loop_finalization
                    << "nth_barrier = " << (int)(do_barrier) << ";"
                    << "in__tone_end_for_(&nth_barrier);"
                    ;

                return loop_finalization;
            }

            Source get_privatized_declarations(ObjectList<IdExpression> privatized_entities,
                    ObjectList<IdExpression> pass_by_pointer,
                    ObjectList<IdExpression> firstprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references)
            {
                Source private_declarations;

                for (ObjectList<IdExpression>::iterator it = privatized_entities.begin();
                        it != privatized_entities.end();
                        it++)
                {
                    Symbol sym = it->get_symbol();
                    Type type = sym.get_type();

                    // Get a declaration of the mangled name of the id-expression
                    if (firstprivate_references.contains(functor(&IdExpression::get_symbol), sym))
                    {
                        Source initializer;

                        if (pass_by_pointer.contains(functor(&IdExpression::get_symbol), sym))
                        {
                            initializer << "(*" << it->mangle_id_expression() << ")";
                        }
                        else
                        {
                            initializer << it->prettyprint();
                        }
                        
                        private_declarations 
							<< type.get_declaration_with_initializer(
									it->get_scope(),
									"p_" + it->mangle_id_expression(),
									initializer.get_source()) 
							<< ";";
                    }
                    else if (reduction_references.contains(functor(&OpenMP::ReductionIdExpression::get_symbol), sym))
                    {
                        ObjectList<OpenMP::ReductionIdExpression> red_id_expr_list = 
                            reduction_references.find(
                                    functor(&OpenMP::ReductionIdExpression::get_symbol), sym
                                    );

                        OpenMP::ReductionIdExpression red_id_expr = *(red_id_expr_list.begin());
                        IdExpression id_expr = red_id_expr.get_id_expression();

                        std::string neuter = red_id_expr.get_neuter().prettyprint();

						std::cerr << "Declaring reduction private " << neuter << std::endl;
                        
                        // Initialize to the neuter
                        private_declarations 
                            << type.get_declaration_with_initializer(
									id_expr.get_scope(),
									"p_" + id_expr.mangle_id_expression(), neuter) 
							<< ";";
                    }
                    else
                    {
                        private_declarations 
							<< type.get_declaration(it->get_scope(), "p_" + it->mangle_id_expression()) 
							<< ";";
                    }
                }

                return private_declarations;
            }

            Source get_lastprivate_assignments(ObjectList<IdExpression> lastprivate_references,
                    ObjectList<IdExpression> pass_by_pointer)
            {
                Source lastprivate_assignments;

                // Lastprivates
                for (ObjectList<IdExpression>::iterator it = lastprivate_references.begin();
                        it != lastprivate_references.end();
                        it++)
                {
                    if (pass_by_pointer.contains(functor(&IdExpression::get_symbol), it->get_symbol()))
                    {
                        lastprivate_assignments
                            << "(*" << it->prettyprint() << ")" << " = p_" << it->mangle_id_expression() << ";"
                            ;
                    }
                    else
                    {
                        lastprivate_assignments
                            << it->prettyprint() << " = p_" << it->mangle_id_expression() << ";"
                            ;
                    }
                }

                return lastprivate_assignments;
            }

            Source get_outlined_function_name(IdExpression function_name, bool want_fully_qualified = true)
            {
                Source result;
                if (function_name.is_qualified() && want_fully_qualified)
                {
                    result
                        << function_name.get_qualified_part()
                        ;
                }
                result
                    << "nth__" << function_name.get_unqualified_part() << "_" << num_parallels;

                return result;
            }

			bool is_nonstatic_member_function(FunctionDefinition function_definition)
			{
				IdExpression function_name = function_definition.get_function_name();
				Symbol function_symbol = function_name.get_symbol();

				// It must be a member
				if (!function_symbol.is_member())
				{
					return false;
				}

				Statement function_body = function_definition.get_function_body();
				Scope function_body_scope = function_body.get_scope();

				Symbol sym = function_body_scope.get_symbol_from_name("this");

				if (!sym.is_valid())
				{
					return false;
				}

				return true;
			}
    };
}

EXPORT_PHASE(TL::OpenMPTransform);
