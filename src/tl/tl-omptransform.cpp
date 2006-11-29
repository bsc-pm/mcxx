#include "tl-omp.hpp"
#include "tl-omptransform.hpp"
#include "tl-predicateutils.hpp"
#include "tl-source.hpp"
#include <iostream>
#include <utility>


namespace TL
{
    class OpenMPTransform : public OpenMP::OpenMPPhase
    {
        private:
            int num_parallels;
            int parallel_nesting;
        public:
            OpenMPTransform()
            {
            }

            virtual ~OpenMPTransform()
            {
            }

            virtual void init()
            {
                // Register the handlers (callbacks) for every construction
                // for now only '#pragma omp parallel for'
                on_parallel_for_pre.connect(&OpenMPTransform::parallel_for_pre, *this);
                on_parallel_for_post.connect(&OpenMPTransform::parallel_for_post, *this);
            }

            void parallel_for_pre(OpenMP::ParallelForConstruct parallel_for_construct)
            {
                num_parallels++;
                parallel_nesting++;
            }

            void parallel_for_post(OpenMP::ParallelForConstruct parallel_for_construct)
            {
                parallel_nesting--;

                // Get the directive
                OpenMP::Directive directive = parallel_for_construct.directive();
                
                // Get the enclosing function definition
                FunctionDefinition function_definition = parallel_for_construct.get_enclosing_function();
                Scope function_scope = function_definition.get_scope();
                IdExpression function_name = function_definition.get_function_name();

                // The will hold the entities as they appear in the clauses
                ObjectList<IdExpression> shared_references;
                ObjectList<IdExpression> private_references;
                ObjectList<IdExpression> firstprivate_references;
                ObjectList<IdExpression> lastprivate_references;
                ObjectList<OpenMP::ReductionIdExpression> reduction_references;
                
                // Get the construct_body of the statement
                Statement construct_body = parallel_for_construct.body();
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

                IdExpression induction_var = for_statement.get_induction_variable();
                if (!private_references.contains(functor(&IdExpression::get_symbol), induction_var.get_symbol()))
                {
                    private_references.append(induction_var);
					shared_references = shared_references.not_find(functor(&IdExpression::get_symbol), 
							induction_var.get_symbol());
				}

                ObjectList<IdExpression> pass_by_pointer;
                ObjectList<IdExpression> privatized_entities;
                // Create the replacement map and the pass_by_pointer set
                ReplaceIdExpression replace_references = 
                    set_replacements(function_scope,
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
                AST_t outline_code = create_outline_parallel_for(
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

                function_definition.get_ast().prepend_sibling_function(outline_code);

                AST_t spawn_code = create_spawn_code(parallel_for_construct.get_scope(),
						parallel_for_construct.get_scope_link(),
						outlined_function_name,
						pass_by_pointer,
						reduction_references
						);

				parallel_for_construct.get_ast().replace_with(spawn_code);
            }

			AST_t create_spawn_code(
					Scope scope,
					ScopeLink scope_link,
					Source outlined_function_name,
					ObjectList<IdExpression> pass_by_pointer,
					ObjectList<OpenMP::ReductionIdExpression> reduction_references)
			{
				Source spawn_code;
				Source reduction_vectors;
				Source groups_definition;
				Source referenced_parameters;

				Source reduction_code;

				spawn_code
					<< "{"
					<< "  int nth_nprocs;"
					<< "  struct nth_desc *nth_selfv;"
					<< "  int nth_nprocs_2;"
					<< "  int nth_arg;"
					<< "  unsigned long long nth_mask;"
					<< "  int nth_num_params;"
					<< "  int nth_p;"
					<< "  extern struct nth_desc *nthf_self_();"
					<< "  extern void nthf_depadd_(struct nth_desc **, int *);"
					<< "  extern void nthf_create_1s_vp_(void (*)(...), int *, int *, struct nth_desc **, unsigned long long *, int *, ...);"
					<< "  extern void nthf_block_();"
					<<    reduction_vectors
					<<    groups_definition
					<< "  nth_selfv = nthf_self_();"
					<< "  nth_nprocs_2 = nth_nprocs + 1;"
					<< "  nthf_depadd_(&nth_selfv, &nth_nprocs_2);"
					<< "  nth_arg = 0;"
					<< "  nth_mask = (unsigned long long)(~0ULL);"
					<< "  nth_num_params = " << pass_by_pointer.size() << ";"
					<< "  for (nth_p = 0; nth_p < nth_nprocs_2 - 1; nth_p++)"
					<< "  {"
					<< "     nthf_create_1s_vp_((void(*)(...))(" << outlined_function_name << "), &nth_arg, &nth_p, &nth_selfv, "
					<< "        &nth_mask, &nth_num_params " << referenced_parameters << ");"
					<< "  }"
					<< "  nthf_block_();"
					<<    reduction_code
					<< "}"
					;

				// Referenced parameters
				for (ObjectList<IdExpression>::iterator it = pass_by_pointer.begin();
						it != pass_by_pointer.end();
						it++)
				{
					referenced_parameters << ", &(" << it->get_ast().prettyprint() << ")";
				}

				// Reduction vectors
				// TODO
				
				// Groups definition
				// TODO
				
				// Reduction code
				// TODO
				
                std::cerr << "CODI SPAWN" << std::endl;
                std::cerr << spawn_code.get_source(true) << std::endl;
                std::cerr << "End CODI SPAWN" << std::endl;
				
				AST_t result = spawn_code.parse_statement(scope, scope_link);
				
				return result;
			}

            AST_t create_outline_parallel_for(
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
                Source outline_parallel_for;

                Source private_declarations;
                Source firstprivate_initializations;
                Source schedule_decisions;
                Source loop_initialization;
                Source distributed_loop_body;
                Source lastprivate_assignments;
                Source loop_reductions;
                Source loop_finalization;
                Source formal_parameters;

                outline_parallel_for 
                    << "void " << outlined_function_name << "(" << formal_parameters << ")"
                    << "{"
                    <<    private_declarations
                    <<    loop_initialization
                    <<    schedule_decisions
                    <<    distributed_loop_body
                    <<    lastprivate_assignments
                    <<    loop_reductions
                    <<    loop_finalization
                    << "}"
                    ;


                Source induction_var_name;

                IdExpression induction_var = for_statement.get_induction_variable();
                induction_var_name << "p_" << induction_var.mangle_id_expression();


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
                    formal_parameters.append_with_separator(pointer_type.get_declaration(it->mangle_id_expression()), ", ");
                }

                // Private declarations
                for (ObjectList<IdExpression>::iterator it = privatized_entities.begin();
                        it != privatized_entities.end();
                        it++)
                {
                    Symbol sym = it->get_symbol();
                    Type type = sym.get_type();

                    // Get a declaration of the mangled name of the id-expression
                    
                    if (!firstprivate_references.contains(functor(&IdExpression::get_symbol), sym))
                    {
                        private_declarations << type.get_declaration("p_" + it->mangle_id_expression()) << ";";
                    }
                    else
                    {
                        // TODO - We'll have to write the proper initialization here
                        //
                        // private_declarations << type->get_declaration_with_initializer("p_" + it->mangle_id_expression(),
                        //                                                 "(*" + 
                    }
                }

                loop_initialization 
                    << "int nth_low;"
                    << "int nth_upper;"
                    << "int nth_step;"
                    << "int intone_start;"
                    << "int intone_end;"
                    << "int intone_last;"
                    << "int nth_barrier;"

                    << "nth_low = " << for_statement.get_lower_bound().get_ast().prettyprint() << ";"
                    << "nth_upper = " << for_statement.get_upper_bound().get_ast().prettyprint() << ";"
                    << "nth_step = " << for_statement.get_step().get_ast().prettyprint() << ";"
                    ;
                
                // Schedule decisions
                schedule_decisions
                    << "int nth_schedule;"
                    << "int nth_chunk;"
                    << "nth_schedule = 0;"
                    << "nth_chunk = 0;"
                    ;

                // Loop distribution
                Source modified_loop_body;
                distributed_loop_body
                    // FIXME - Eventually an include will solve this
                    << "extern void in__tone_begin_for_(int*, int*, int*, int*, int*);"
                    << "extern int in__tone_next_iters_(int*, int*, int*);"
                    << "extern void in__tone_end_for_(int*);"

                    << "in__tone_begin_for_(&nth_low, &nth_upper, &nth_step, &nth_chunk, &nth_schedule);"
                    
                    << "while (in__tone_next_iters_(&intone_start, &intone_end, &intone_last) != 0)"
                    << "{"
                    << "   for (" << induction_var_name << " = intone_start; "
                    << "        nth_step >= 1 ? " << induction_var_name << " <= intone_end : " << induction_var_name << ">= intone_end;"
                    << "        " << induction_var_name << " += nth_step)"
                    << "   {"
                    << "   " << modified_loop_body
                    << "   }"
                    << "}"
                    ;

				// Replace references
                Statement modified_loop_body_stmt = replace_references.replace(loop_body);

                modified_loop_body << modified_loop_body_stmt.get_ast().prettyprint();

                // Lastprivate assignments
                for (ObjectList<IdExpression>::iterator it = lastprivate_references.begin();
                        it != lastprivate_references.end();
                        it++)
                {
                    if (pass_by_pointer.contains(functor(&IdExpression::get_symbol), it->get_symbol()))
                    {
                        lastprivate_assignments 
                            << "(*" << it->get_ast().prettyprint() << ")" << " = p_" << it->mangle_id_expression() << ";"
                            ;
                    }
                    else
                    {
                        lastprivate_assignments 
                            << it->get_ast().prettyprint() << " = p_" << it->mangle_id_expression() << ";"
                            ;
                    }
                }

                // Loop reductions
                // TODO

                // Loop finalization
                loop_finalization
                    << "nth_barrier = 0;"
                    << "in__tone_end_for_(&nth_barrier);"
                    ;

                std::cerr << "CODI OUTLINE" << std::endl;
                std::cerr << outline_parallel_for.get_source(true) << std::endl;
                std::cerr << "End CODI OUTLINE" << std::endl;

                AST_t result;

                result = outline_parallel_for.parse_global(function_definition.get_scope(), 
                        function_definition.get_scope_link());

                return result;
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

				OpenMP::DefaultClause default_clause = directive.default_clause();

				// Everything should have been tagged by the user
				if (default_clause.is_none())
					return;

				ObjectList<IdExpression> non_local_symbols = construct_body.non_local_symbol_occurrences(Statement::ONLY_VARIABLES);

				// Filter in any of the private sets
				non_local_symbols = non_local_symbols.filter(not_in_set(shared_references, functor(&IdExpression::get_symbol)));
				non_local_symbols = non_local_symbols.filter(not_in_set(private_references, functor(&IdExpression::get_symbol)));
				non_local_symbols = non_local_symbols.filter(not_in_set(firstprivate_references, functor(&IdExpression::get_symbol)));
				non_local_symbols = non_local_symbols.filter(not_in_set(lastprivate_references, functor(&IdExpression::get_symbol)));

				ObjectList<IdExpression> reduction_id_expressions = 
					reduction_references.map(functor(&OpenMP::ReductionIdExpression::get_id_expression));
				non_local_symbols = non_local_symbols.filter(not_in_set(reduction_id_expressions, functor(&IdExpression::get_symbol)));

				shared_references.append(non_local_symbols);
            }

            ReplaceIdExpression set_replacements(Scope function_scope,
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

                for (ObjectList<IdExpression>::iterator it = shared_references.begin();
                        it != shared_references.end();
                        it++)
                {
                    Symbol current_sym = it->get_symbol();

                    // This symbol already appears in "pass_by_pointer" id-expressions
                    if (pass_by_pointer.contains(functor(&IdExpression::get_symbol), current_sym))
                        continue;

                    Symbol global_sym = function_scope.get_symbol_from_id_expr(it->get_ast());

                    if (!global_sym.is_valid() ||
                            (global_sym != current_sym))
                    {
                        // The symbol is either not accessible or it is not the same
                        // Since it is shared it must be referenced via a pointer
                        //

                        // This must be passed by pointer
                        pass_by_pointer.append(*it);

                        // The mapping is needed only if in shared because
                        // every reference will have to be replaced
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

                }
                
                // First mix all things that will be made private
                ObjectList<IdExpression> privatized_references;
                privatized_references.append(private_references);
                privatized_references.append(firstprivate_references);
                privatized_references.append(lastprivate_references);

                for (ObjectList<IdExpression>::iterator it = privatized_references.begin();
                        it != privatized_references.end();
                        it++)
                {
                    Symbol current_sym = it->get_symbol();

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

            Source get_outlined_function_name(IdExpression function_name)
            {
                Source result;
                if (function_name.is_qualified())
                {
                    result
                        << function_name.get_qualified_part() << "::"
                        ;
                }
                result
                    << "nth__" << function_name.get_unqualified_part() << "_" << num_parallels;

                return result;
            }
    };
}

EXPORT_PHASE(TL::OpenMPTransform);
