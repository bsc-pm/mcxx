#include "tl-omp.hpp"
#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "tl-source.hpp"
#include "tl-scopelink.hpp"
#include "tl-traverse.hpp"
#include "tl-predicateutils.hpp"
#include "cxx-attrnames.h"
#include <iostream>
#include <set>

extern "C" 
{
    TL::CompilerPhase* give_compiler_phase_object(void)
    {
        return new TL::OpenMPTransform();
    }
}

namespace TL
{
    typedef PredicateBool<OMP_IS_PARALLEL_CONSTRUCT> ParallelConstruct;
    typedef PredicateBool<LANG_IS_ID_EXPRESSION> IdExpression;

    class ParallelFunctor : public TraverseFunctor
    {
        private:
            OpenMPTransform& omp_context;
        public:
            ParallelFunctor(OpenMPTransform& omp_ctx)
                : omp_context(omp_ctx)
            {
            }

            virtual void preorder(Context ctx, AST_t node)
            {
                omp_context.num_parallels++;
				omp_context.parallel_nesting++;
            }

            virtual void postorder(Context ctx, AST_t node)
            {
				omp_context.parallel_nesting--;

                AST_t construct_body = node.get_attribute(OMP_CONSTRUCT_BODY);

                Source outlined_function;
                Source outlined_function_parameters;

				// Get all the references and construct its pointer type
                IdExpression id_expression;
                AST_set_t references_set = construct_body.get_all_subtrees_predicate(id_expression);
                AST_set_t::iterator it;
                for (it = references_set.begin(); it != references_set.end(); it++)
                {
                    Scope scope = omp_context.scope_link.get_scope(*it);

                    Symbol sym = scope.get_symbol_from_id_expr(*it);

                    Type type = sym.get_type().get_pointer_to();

					// Append to the outline function declaration
                    outlined_function_parameters.append_with_separator(
                            type.get_parameter_declaration_str(sym.get_name()), ", ");
                }

				// Create the skeleton of the outline function
                outlined_function << "void outline_" << omp_context.num_parallels << "(" << outlined_function_parameters << ")"
                    << "{"
                    << construct_body.prettyprint() 
                    << "}";

                AST_t enclosing_function = construct_body.get_enclosing_function_definition();
                Scope enclosing_function_scope = omp_context.scope_link.get_scope(enclosing_function);

                AST_t outlined_function_tree = outlined_function.parse_global(enclosing_function_scope, omp_context.scope_link);

				// Now get all the references of the outline function
				AST_list_t references_list = outlined_function_tree.get_all_subtrees_predicate(id_expression);
				for (it = references_list.begin(); it != references_list.end(); it++)
				{
                    Scope scope = omp_context.scope_link.get_scope(*it);

					Source id_expression = it->prettyprint();
					Source derref_id_expression;

					// And replace them with the derreference expression
					derref_id_expression << "(*" << id_expression << ")";

					AST_t derref_expr = derref_id_expression.parse_expression(scope);

					it->replace_with(derref_expr);
				}

				Source spawn_arguments;
				for (it = references_set.begin(); it != references_set.end(); it++)
				{
					spawn_arguments.append_with_separator("&" + it->prettyprint(), ", ");
				}

				Source spawn_parallel;
				spawn_parallel 
					<< "{"
					<< "  void nthf_create(...);" 
					<< "  for (int nthf_i = 0; nthf_i < nthf_num_cpus(); nthf_i++)"
					<< "  {"
					<< "        nthf_create(outline_" << omp_context.num_parallels << ", " << spawn_arguments << ");"
					<< "  }"
					<< "}";

				Scope spawn_scope = omp_context.scope_link.get_scope(construct_body);
				AST_t spawn_parallel_tree = spawn_parallel.parse_statement(spawn_scope, omp_context.scope_link);

				node.prepend_to_translation_unit(outlined_function_tree);

				node.replace_with(spawn_parallel_tree);
				// Prepend to the translation unit
            }

            virtual ~ParallelFunctor() { }
    };

    void OpenMPTransform::run(DTO& dto)
    {
        translation_unit = dto["translation_unit"];
        scope_link = dto["scope_link"];
        global_scope = scope_link.get_scope(translation_unit);

        DepthTraverse depth_traverse;

        ParallelConstruct on_parallel;
        ParallelFunctor parallel_functor(*this);

        depth_traverse.add_predicate(on_parallel, parallel_functor);
        depth_traverse.traverse(translation_unit);
    }
}
