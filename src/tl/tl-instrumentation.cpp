#include "tl-instrumentation.hpp"
#include "tl-compilerphase.hpp"
#include "tl-predicateutils.hpp"
#include "tl-langconstruct.hpp"
#include "tl-traverse.hpp"
#include "tl-scopelink.hpp"

#include <iostream>
#include <set>

namespace TL
{

	class Instrumentation : public CompilerPhase
	{
		private:
			class InstrumentationFunctor : public TraverseFunctor
			{
				private:
					std::set<std::string> defined_shadows;
				public:
					virtual void preorder(Context ctx, AST_t node)
					{
						// Do nothing
					}

					virtual void postorder(Context ctx, AST_t node)
					{
						ScopeLink scope_link = ctx.scope_link;
						
						AST_t called_expression_tree = node.get_attribute(LANG_CALLED_EXPRESSION);
						Expression called_expression(called_expression_tree, scope_link);

						// Only function-names are considered here
						if (!called_expression.is_id_expression())
						{
							std::cerr << "Called expression is not an id expression" << std::endl;
							return;
						}

						IdExpression called_id_expression = called_expression.get_id_expression();
						
						std::string shadow_function_name = 
							"_" + called_id_expression.mangle_id_expression() + "_instr";

						if (defined_shadows.find(shadow_function_name) == defined_shadows.end())
						{
							// The shadow has not been defined, define it here
							define_shadow(called_id_expression, shadow_function_name);
							defined_shadows.insert(shadow_function_name);
						}

						// Now create an expression tree
						Source shadow_function_call;

						// Note that this is just what you find before the "(...)"
						shadow_function_call
							<< shadow_function_name;

						AST_t shadow_function_call_tree = 
							shadow_function_call.parse_expression(called_id_expression.get_scope());

						// And replace it
						called_expression_tree.replace_with(shadow_function_call_tree);
					}

					void define_shadow(IdExpression function_name, std::string shadow_function_name)
					{
						FunctionDefinition function_definition = function_name.get_enclosing_function();

						Symbol function_symbol = function_name.get_symbol();
						Type function_type = function_symbol.get_type();

						ObjectList<std::string> parameter_names;
						
						std::string shadow_declaration = function_type.get_declaration_with_parameters(function_symbol.get_scope(), 
								shadow_function_name, parameter_names);

						Source shadow_function_definition;

						Source original_arguments;

						for (ObjectList<std::string>::iterator it = parameter_names.begin();
								it != parameter_names.end();
								it++)
						{
							original_arguments.append_with_separator((*it), ",");
						}

						shadow_function_definition
							<< "static inline " << shadow_declaration
							<< "{"
							<<   "return "  << function_name.prettyprint() << "(" << original_arguments << ");"
							<< "}"
							;

						AST_t shadow_function_def_tree = 
							shadow_function_definition.parse_global(function_definition.get_scope(),
									function_definition.get_scope_link());

						function_definition.get_ast().prepend_sibling_function(shadow_function_def_tree);
					}
			};

		public:
			virtual void run(DTO& data_flow)
			{
				std::cerr << "Running instrumentation" << std::endl;
				AST_t root_node = data_flow["translation_unit"];
				ScopeLink scope_link = data_flow["scope_link"];

				DepthTraverse depth_traverse;

				PredicateBool<LANG_IS_FUNCTION_CALL> function_call_pred;
				InstrumentationFunctor instrumentation_functor;

				depth_traverse.add_predicate(function_call_pred, instrumentation_functor);
				depth_traverse.traverse(root_node, scope_link);
			}

			virtual ~Instrumentation()
			{
			}

			Instrumentation()
			{
				std::cerr << "Instrumentation support loaded" << std::endl;
			}
	};

}

EXPORT_PHASE(TL::Instrumentation);
