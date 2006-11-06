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
			// The OpenMP transform context
            OpenMPTransform& omp_context;
        public:
            ParallelFunctor(OpenMPTransform& omp_ctx)
                : omp_context(omp_ctx)
            {
            }

			// This is invoked in preorder for #pragma omp parallel
            virtual void preorder(Context ctx, AST_t node)
            {
				// One more level of nesting
				omp_context.parallel_nesting++;
            }

			// This is executed in postorder for #pragma omp parallel
			//   "node" will be the whole "#pragma omp parallel" construct
            virtual void postorder(Context ctx, AST_t node)
            {
				// One more parallel region found
                omp_context.num_parallels++;
				// One less level of nesting
				omp_context.parallel_nesting--;

				// Get the body of the "#pragma omp parallel" construct
                AST_t construct_body = node.get_attribute(OMP_CONSTRUCT_BODY);

				// It will hold the outlined function
                Source outlined_function;
				// and its parameters
                Source outlined_function_parameters;

				// Now we get all the references (FIXME we should check that they are
				// not privatized and that is necessary to pass them by
				// reference they can be globally accessable or context accessable)
				// Only local variables should be passed to the outline function

				// Predicate for id_expression (a, A::a, A<B>::a)
                IdExpression id_expression;

				// Get all nonrepeated subtrees that satisfy the predicate for id_expression
                AST_set_t references_set = construct_body.depth_subtrees().filter(id_expression);
                AST_set_t::iterator it;
				// And iterate over it
                for (it = references_set.begin(); it != references_set.end(); it++)
                {
					// Get the scope of every id_expression
                    Scope scope = omp_context.scope_link.get_scope(*it);
					// Get the symbol of the id_expression
                    Symbol sym = scope.get_symbol_from_id_expr(*it);
					// And get a pointer type to the original type
                    Type type = sym.get_type().get_pointer_to();
					
					// Append to the outline function parameter clause
					outlined_function_parameters.append_with_separator(
							// This one below gives you a declaration of this type with
							// this symbol name
							type.get_parameter_declaration_str(sym.get_name()), 
							", ");
                }

				// Create the skeleton of the outline function
                outlined_function 
					<< "void outline_" << omp_context.num_parallels << "(" << outlined_function_parameters << ")"
					<< "{"
					<< construct_body.prettyprint() // The original body untouched
					<< "}";

				// Get the enclosing function definition tree (FIXME this is very C/C++ specific!)
                AST_t enclosing_function = construct_body.get_enclosing_function_definition();
				// and its scope
                Scope enclosing_function_scope = omp_context.scope_link.get_scope(enclosing_function);

				// Now parse the outline function to get a tree, in the same scope as the enclosing_function_scope
                AST_t outlined_function_tree = outlined_function.parse_global(enclosing_function_scope, omp_context.scope_link);

				// Now get all the references of the outline function (FIXME we
				// should check that they have been passed by pointer
				// parameter)
				AST_list_t references_list = outlined_function_tree.depth_subtrees().filter(id_expression);
				for (it = references_list.begin(); it != references_list.end(); it++)
				{
					// Get the scope for this id_expression
                    Scope scope = omp_context.scope_link.get_scope(*it);

					// Prettyprint it
					Source id_expression = it->prettyprint();
					Source derref_id_expression;

					// And replace them with the derreference expression
					//   "k" -> "(*k)"
					derref_id_expression << "(*" << id_expression << ")";

					// Parse this expression
					AST_t derref_expr = derref_id_expression.parse_expression(scope);

					// Replace with the derreferred expression
					it->replace_with(derref_expr);
				}

				// Prepend to the translation unit (FIXME this because not always
				// we're going to prepend to the translation unit)
				node.prepend_to_translation_unit(outlined_function_tree);

				// Now create the spawn code

				// First create the spawn arguments
				Source spawn_arguments;
				for (it = references_set.begin(); it != references_set.end(); it++)
				{
					// Its simply the references we passed as parameters
					// but with referenciation
					spawn_arguments.append_with_separator("&" + it->prettyprint(), ", ");
				}

				// Code for spawn
				Source spawn_parallel;
				spawn_parallel 
					<< "{"
					<< "  void nthf_create(...);" 
					<< "  for (int nthf_i = 0; nthf_i < nthf_num_cpus(); nthf_i++)"
					<< "  {"
					<< "        nthf_create(outline_" << omp_context.num_parallels << ", " << spawn_arguments << ");"
					<< "  }"
					<< "}";

				// Get the scope for the whole construct_body
				Scope spawn_scope = omp_context.scope_link.get_scope(construct_body);

				// And parse this statement
				AST_t spawn_parallel_tree = spawn_parallel.parse_statement(spawn_scope, omp_context.scope_link);

				// And replace the whole "#pragma omp parallel" construc
				node.replace_with(spawn_parallel_tree);
            }

            virtual ~ParallelFunctor() { }
    };

    void OpenMPTransform::run(DTO& dto)
    {
		// Get the translation_unit tree
        translation_unit = dto["translation_unit"];
		// Get the scope_link
        scope_link = dto["scope_link"];
		// Get the global_scope
        global_scope = scope_link.get_scope(translation_unit);

		// Instantiate a DepthTraverse
        DepthTraverse depth_traverse;

		// Predicate for #pragma omp parallel
        ParallelConstruct on_parallel;
		// Functor for #pragma omp parallel
        ParallelFunctor parallel_functor(*this);

		// Register the #pragma omp parallel 
		// filter with its functor
        depth_traverse.add_predicate(on_parallel, parallel_functor);

		// Traverse in a depth-first fashion the AST
        depth_traverse.traverse(translation_unit);
    }
}
