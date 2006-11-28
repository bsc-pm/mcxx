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

				// Get the data attributes for every entity
				get_data_attributes(function_scope,
						directive,
						construct_body,
						shared_references,
						private_references,
						firstprivate_references,
						lastprivate_references,
						reduction_references);

				ObjectList<IdExpression> pass_by_pointer;
				ObjectList<IdExpression> privatized_entities;
				// Create the replacement map and the pass_by_pointer set
				ReplaceIdExpression replace_references = 
					set_replacements(function_scope,
							directive,
							construct_body,
							shared_references,
							private_references,
							firstprivate_references,
							lastprivate_references,
							reduction_references,
							pass_by_pointer,
							privatized_entities);

				Source outlined_function_name = get_outlined_function_name(function_name);

				AST_t outline_code = create_outline_parallel_for(outlined_function_name, 
						for_statement,
						replace_references,
						pass_by_pointer,
						privatized_entities,
						firstprivate_references,
						lastprivate_references,
						reduction_references);

				// AST_t spawn_code = create_spawn_code();
			}

			AST_t create_outline_parallel_for(Source outlined_function_name,
					ForStatement for_statement,
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

				// induction_var_name
				// 	<< "p_" + for_statement.induction_var().prettyprint()
				// 	;

				// Formal parameters, basically pass_by_pointer things
				// (and sometimes something else)
				for (ObjectList<IdExpression>::iterator it = pass_by_pointer.begin();
						it != pass_by_pointer.end();
						it++)
				{
					Symbol sym = it->get_symbol();
					Type type = sym.get_type();

					// Get a declaration of the mangled name of the id-expression
					formal_parameters << type.get_declaration(it->mangle_id_expression()) << ", ";
				}

				// FIXME - We have nice operations that can help avoiding that
				private_declarations << " void* _prova";
				
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
					<< "int nth_chunk;"
					<< "int nth_schedule;"
					<< "int intone_start;"
					<< "int intone_end;"
					<< "int intone_last;"
					<< "int nth_barrier;"

					// << "nth_low = " << for_statement.lower_expression().prettyprint() << ";"
					// << "nth_upper = " << for_statement.upper_expression().prettyprint() << ";"
					// << "nth_step = " << for_statement.step_expression().prettyprint() << ";"
					;
				
				// Schedule decisions
				// TODO
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
					<< "external void in__tone_begin_for_(int*, int*, int*, int*, int*);"
					<< "external int in__tone_next_iters_(int*, int*, int*);"
					<< "external void in__tone_end_for_(int*);"

					<< "in__tone_begin_for_(&nth_low, &nth_upper, &nth_step, &nth_chunk, &nth_schedule);"
					
					<< "while (in__tone_next_iters_(&intone_start, &intone_end, &intone_last) != 0)"
					<< "{"
					<< "   for (" << induction_var_name << " = intone_start; "
					<< "        step >= 1 ? " << induction_var_name << " <= intone_end : " << induction_var_name << ">= intone_end;"
					<< "        " << induction_var_name << " += step)"
					<< "   {"
					<< "   " << modified_loop_body
					<< "   }"
					<< "}"
					;

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

							// FIXME: this is a bit dangerous since the
							// mangled_name might not exist but we do not have
							// to disambiguate this
							AST_t derref_tree = derref_name.parse_expression(it->get_scope());

							result.add_replacement(current_sym, derref_tree);
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

					std::string mangled_name = std::string("p_") + it->mangle_id_expression();

					Source private_ref;
					private_ref << mangled_name;

					AST_t private_ref_tree = private_ref.parse_expression(it->get_scope());
					
					// This entity is privatized
					privatized_entities.append(*it);

					// Create a replacement for it
					result.add_replacement(current_sym, private_ref_tree);
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
