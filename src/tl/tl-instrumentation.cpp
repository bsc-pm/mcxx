#include "cxx-utils.h"
#include "tl-instrumentation.hpp"
#include "tl-compilerphase.hpp"
#include "tl-predicateutils.hpp"
#include "tl-langconstruct.hpp"
#include "tl-traverse.hpp"
#include "tl-scopelink.hpp"
#include "tl-externalvars.hpp"

#include <iostream>
#include <fstream>
#include <set>

namespace TL
{
	class InstrumentFilterFile 
	{
		private:
			bool _filter_inverted;
			std::set<std::string> _filter_set;
		public:
			InstrumentFilterFile()
			{
				std::ifstream filter_file;
				std::string filter_file_name = ExternalVars::get("instrument_file_name", "./filter_instrument");

				std::string filter_mode_var = ExternalVars::get("instrument_mode", "normal");

				_filter_inverted = false;
				if (filter_mode_var == "inverted")
				{
					_filter_inverted = true;
				}
				else if (filter_mode_var != "normal")
				{
					std::cerr << "Variable 'instrument_mode' only can be 'inverted' or 'normal' (you set '" 
						<< filter_mode_var << "')" << std::endl;
				}

				filter_file.open(filter_file_name.c_str());
				if (!filter_file.good())
				{
					std::cerr << "Could not open file '" << filter_file_name << "'. Skipping." << std::endl;
					return;
				}

				// Read all lines of the file
				char line[256];
				while (filter_file.good())
				{
					filter_file.getline(line, 256);

					char* p = line;

					while (*p == ' ' || *p == '\t')
					{
						p++;
					}

					if (*p == '#')
					{
						// Comment
						continue;
					}

					if (is_blank_string(p))
					{
						continue;
					}

					_filter_set.insert(p);
				}

				filter_file.close();

				// Always include this
				_filter_set.insert("mintaka*");
			}

			bool match(const std::string& function_name)
			{
				bool found = false;
				for (std::set<std::string>::iterator it = _filter_set.begin();
						it != _filter_set.end();
						it++)
				{
					std::string::const_reverse_iterator rit = it->rbegin();

					if (*rit == '*')
					{
						// Prefix
						std::string prefix = it->substr(0, it->size() - 1);
						std::string match_prefix = function_name;


						if (match_prefix.size() >= prefix.size())
						{
							match_prefix = match_prefix.substr(0, prefix.size());

							if (match_prefix == prefix)
							{
								found = true;
								break;
							}
						}
					}
					else
					{
						if (function_name == *it)
						{
							found = true;
							break;
						}
					}
				}

				if (!_filter_inverted)
				{
					// If found it does have to be filtered
					return found ? true : false;
				}
				else
				{
					// If not found it does not have to be filtered
					return found ? false : true;
				}
			}
	};

	class Instrumentation : public CompilerPhase
	{
		private:
			InstrumentFilterFile _instrument_filter;
			class InstrumentationFunctor : public TraverseFunctor
			{
				private:
					std::set<std::string> defined_shadows;
					InstrumentFilterFile& _instrument_filter;
				public:
					InstrumentationFunctor(InstrumentFilterFile& instrument_filter)
						: _instrument_filter(instrument_filter)
					{
					}

					~InstrumentationFunctor()
					{
					}

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

						if (_instrument_filter.match(called_id_expression.prettyprint()))
								return;


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
						called_expression_tree.replace(shadow_function_call_tree);
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

						Source invocation;
						Source before_code;
						Source after_code;

						shadow_function_definition
							<< "static inline " << shadow_declaration
							<< "{"
							<<     invocation
							<< "}"
							;

						if (!function_type.returns().is_void())
						{
							invocation
								<< before_code
								<< function_type.returns().get_declaration(function_definition.get_scope(), "_result") << " = "
								<< function_name.prettyprint() << "(" << original_arguments << ");"
								<< after_code
								<< "return _result;"
								;
						}
						else
						{
							invocation
								<< before_code
								<< function_name.prettyprint() << "(" << original_arguments << ");"
								<< after_code
								;
						}

						int file_line = 0;
						std::string mangled_function_name = "\"" + function_name.mangle_id_expression() + "\"";

						// Should not this be uint64_t instead of int ?
						before_code
							<< "const int EVENT_CALL_USER_FUNCTION = 6000;"
							<< "int _user_function_event = mintaka_index_get(" << mangled_function_name << "," << file_line << ");"
							<< "if (_user_function_event == -1)"
							<< "{"
							<<     "_user_function_event = mintaka_index_allocate(" << mangled_function_name << "," << file_line << ");"
							<< "}"
							<< "mintaka_event(EVENT_CALL_USER_FUNCTION, _user_function_event);"
							;

						after_code
							<< "mintaka_event(EVENT_CALL_USER_FUNCTION, 0);"
							;

						AST_t shadow_function_def_tree = 
							shadow_function_definition.parse_global(function_definition.get_scope(),
									function_definition.get_scope_link());

						function_definition.get_ast().prepend_sibling_function(shadow_function_def_tree);
					}
			};

			class MainWrapper : public TraverseFunctor
			{
				private:
					ScopeLink _sl;
				public:
					MainWrapper(ScopeLink sl)
						: _sl(sl)
					{
					}

					virtual void preorder(Context ctx, AST_t node)
					{
						// Do nothing
					}

					virtual void postorder(Context ctx, AST_t node)
					{
						FunctionDefinition function_def(node, _sl);
						IdExpression function_name = function_def.get_function_name();

						Symbol function_symbol = function_name.get_symbol();
						Type function_type = function_symbol.get_type();

						ObjectList<std::string> parameters;

						Source main_declaration = function_type.get_declaration_with_parameters(function_symbol.get_scope(),
								"main", parameters);

						// "main" is always an unqualified name so this transformation is safe
						function_name.get_ast().replace_text("__instrumented_main");

						Source instrumented_main_declaration = function_type.get_declaration(function_symbol.get_scope(),
								"__instrumented_main");

						Source new_main;
						new_main
							<< "static void __begin_mintaka_per_thread()"
							<< "{"
							<< "   mintaka_thread_begin(0, nth_get_physical_thread_num());"
							<< "}"

							<< "static void __end_mintaka_per_thread()"
							<< "{"
							<< "   mintaka_thread_end();"
							<< "}"

							<< "static void __begin_mintaka(char* exec_basename)"
							<< "{"
							<< "  mintaka_app_begin(exec_basename);"
							<< "  int nth_nprocs;"
							<< "  nth_desc *nth_selfv;"
							<< "  int nth_arg;"
							<< "  nth_argdesc_t nth_mask;"
							<< "  int nth_num_params;"
							<< "  int nth_p;"
							<< "  nth_selfv = nthf_self_();"
                            << "  nth_nprocs =  nthf_cpus_actual_();"
							<< "  nthf_team_set_nplayers_ (&nth_nprocs);"
							<< "  nth_arg = 0;"
							<< "  nth_mask = (nth_argdesc_t)(~0);"
							<< "  nth_num_params = 0;"
							<< "  for (nth_p = 0; nth_p < nth_nprocs; nth_p++)"
							<< "  {"
							<< "     nthf_create_1s_vp_((void*)(__begin_mintaka_per_thread), &nth_arg, &nth_p, &nth_selfv, "
							<< "        &nth_mask, &nth_num_params);"
							<< "  }"
							<< "  nthf_block_();"
							<< "}"

							<< "static void __end_mintaka()"
							<< "{"
							<< "  int nth_nprocs;"
							<< "  nth_desc *nth_selfv;"
							<< "  int nth_arg;"
							<< "  nth_argdesc_t nth_mask;"
							<< "  int nth_num_params;"
							<< "  int nth_p;"
							<< "  nth_selfv = nthf_self_();"
                            << "  nth_nprocs =  nthf_cpus_actual_();"
							<< "  nthf_team_set_nplayers_ (&nth_nprocs);"
							<< "  nth_arg = 0;"
							<< "  nth_mask = (nth_argdesc_t)(~0);"
							<< "  nth_num_params = 0;"
							<< "  for (nth_p = 0; nth_p < nth_nprocs; nth_p++)"
							<< "  {"
							<< "     nthf_create_1s_vp_((void*)(__end_mintaka_per_thread), &nth_arg, &nth_p, &nth_selfv, "
							<< "        &nth_mask, &nth_num_params);"
							<< "  }"
							<< "  nthf_block_();"
							<< "  mintaka_merge();"
							<< "  mintaka_app_end();"
							<< "}"

							<< instrumented_main_declaration << ";"
							<< main_declaration
							<< "{"
							<< "  __begin_mintaka(basename(_p_1[0]));"
							<< "  __instrumented_main(_p_0, _p_1);"
							<< "  __end_mintaka();"
							<< "}"
							<< node.prettyprint()
							;

						AST_t new_main_tree = new_main.parse_global(function_def.get_scope(),
								function_def.get_scope_link());

						node.replace(new_main_tree);
					}

					~MainWrapper()
					{
					}
			};

			class MainPredicate : public Predicate<AST_t>
			{
				private:
					ScopeLink _sl;
					PredicateBool<LANG_IS_FUNCTION_DEFINITION> is_function_def;
				public:
					MainPredicate(ScopeLink& sl)
						: _sl(sl)
					{
					}

					virtual bool operator()(AST_t& t) const
					{
						if (is_function_def(t))
						{
							FunctionDefinition function_def(t, _sl);

							IdExpression function_name = function_def.get_function_name();

							if (function_name.mangle_id_expression() == "main")
							{
								Symbol function_symbol = function_name.get_symbol();

								if (!function_symbol.is_member())
								{
									return true;
								}
							}

						}
						return false;
					}

					virtual ~MainPredicate() { }
			};

		public:
			virtual void run(DTO& data_flow)
			{
				std::cerr << "Running instrumentation" << std::endl;
				AST_t root_node = data_flow["translation_unit"];
				ScopeLink scope_link = data_flow["scope_link"];

				DepthTraverse depth_traverse;

				PredicateBool<LANG_IS_FUNCTION_CALL> function_call_pred;
				InstrumentationFunctor instrumentation_functor(_instrument_filter);

				MainWrapper mainwrapper_functor(scope_link);
				MainPredicate main_function_def(scope_link);

				depth_traverse.add_predicate(function_call_pred, instrumentation_functor);
				depth_traverse.add_predicate(main_function_def, mainwrapper_functor);

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
