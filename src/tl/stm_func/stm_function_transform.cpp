#include "stm_function_transform.hpp"
#include "tl-compilerphase.hpp"
#include "tl-langconstruct.hpp"
#include "tl-traverse.hpp"
#include "tl-functionfilter.hpp"

namespace TL
{
    class STMFunctionDefFunctor : public TraverseFunctor
    {
        private:
			FunctionFilterFile function_filter;
        public:
            virtual void postorder(Context ctx, AST_t node) 
			{
				FunctionDefinition function_def(node, ctx.scope_link);
				IdExpression function_name = function_def.get_function_name();
				Symbol function_symbol = node.get_attribute(LANG_FUNCTION_SYMBOL);
				Type function_type = function_symbol.get_type();

				ObjectList<AST_t> functional_declarator = 
					node.depth_subtrees(PredicateBool<LANG_IS_FUNCTIONAL_DECLARATOR>(), 
						AST_t::NON_RECURSIVE);

				AST_t first_functional_declarator = *(functional_declarator.begin());
				ObjectList<AST_t> declared_parameters = 
					first_functional_declarator.depth_subtrees(
							PredicateBool<LANG_IS_DECLARED_PARAMETER>(),
							AST_t::NON_RECURSIVE);

				// std::cerr << "Found function -> " << function_def.get_function_name().prettyprint() << std::endl;
				// for (ObjectList<AST_t>::iterator it = declared_parameters.begin();
				// 		it != declared_parameters.end();
				// 		it++)
				// {
				// 	AST_t declarator_name = it->get_attribute(LANG_DECLARED_PARAMETER);
				// 	std::cerr << " -> param : '" << declarator_name.prettyprint() << "'" << std::endl;
				// }
				// std::cerr << std::endl;

				Type return_type = function_type.returns();

				Source stm_function_source;
				Source stm_parameters;
				Source stm_function_body;
				Statement function_tree = function_def.get_function_body();

				if (function_filter.match(function_name.prettyprint())
						|| function_symbol.is_member())
				{
					return;
				}

                std::cerr << "Wrapping function '" << function_name.prettyprint() << " in " << function_name.get_ast().get_locus() << std::endl;

				stm_function_source
					<< return_type.get_declaration(function_def.get_scope(), "") 
					<< " "
					<< "__stm_" << function_name.prettyprint() << "_ (" << stm_parameters << ")"
					<< "{\n"
					<< "#pragma omp transaction converted_function(1)\n"
					<<    stm_function_body
					<< "}"
					;

				stm_function_body 
					<< function_tree.prettyprint()
					;

				stm_parameters.append_with_separator("Transaction *__t", ",");

				bool has_ellipsis;
				ObjectList<Type> parameter_types = function_type.parameters(has_ellipsis);

				int parameter_num = 0;
				for (ObjectList<Type>::iterator it = parameter_types.begin();
						it != parameter_types.end();
						it++)
				{
					AST_t declarator_name = 
						declared_parameters[parameter_num].get_attribute(LANG_DECLARED_PARAMETER);
					stm_parameters.append_with_separator(
							it->get_declaration(
								function_def.get_scope(), 
								declarator_name.prettyprint()
								),
							","
							);

					parameter_num++;
				}

				if (has_ellipsis)
				{
					stm_parameters.append_with_separator("...", ",");
				}

				AST_t stm_function_tree = stm_function_source.parse_declaration(function_def.get_ast(),
						function_def.get_scope_link());

				function_def.prepend_sibling(stm_function_tree);
			}

            virtual ~STMFunctionDefFunctor() { }
    };

	class STMFunctionTransform : public CompilerPhase
	{
		public:
			virtual void run(DTO& dto)
			{
				AST_t root_node = dto["translation_unit"];
				ScopeLink scope_link = dto["scope_link"];
				
				PredicateBool<LANG_IS_FUNCTION_DEFINITION> function_def_pred;
				DepthTraverse depth_traverse;

				STMFunctionDefFunctor function_def_functor;

				depth_traverse.add_predicate(function_def_pred, function_def_functor);

				depth_traverse.traverse(root_node, scope_link);
			}
	};
}

EXPORT_PHASE(TL::STMFunctionTransform);
