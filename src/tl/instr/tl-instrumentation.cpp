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

                if (!_filter_inverted)
                {
                    // Always include this
                    _filter_set.insert("mintaka*");
                }
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
                        AST_t arguments_tree = node.get_attribute(LANG_FUNCTION_ARGUMENTS);
                        Expression called_expression(called_expression_tree, scope_link);

                        // Only function-names are considered here
                        if (!called_expression.is_id_expression())
                        {
                            // std::cerr << "Called expression is not an id expression" << std::endl;
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
                            if (!define_shadow(called_id_expression, shadow_function_name))
                            {
                                // Ignore this shadow
                                return;
                            }
                            defined_shadows.insert(shadow_function_name);
                        }

                        // Now replace the arguments
                        Source replaced_arguments;

                        replaced_arguments 
                            << "\"" << node.get_file() << "\""
                            << ","
                            << node.get_line()
                            ;

                        if (arguments_tree.prettyprint() != "")
                        {
                            replaced_arguments << "," << arguments_tree.prettyprint(/*commas=*/true);
                        }

                        // Now create an expression tree
                        Source shadow_function_call;
                        shadow_function_call
                            << shadow_function_name << "(" << replaced_arguments << ")"
                            ;

                        AST_t shadow_function_call_tree = 
                            shadow_function_call.parse_expression(scope_link.get_scope(node));
                        
                        node.replace(shadow_function_call_tree);
                    }

                    bool define_shadow(IdExpression function_name, std::string shadow_function_name)
                    {
                        FunctionDefinition function_definition = function_name.get_enclosing_function();

                        Symbol function_symbol = function_name.get_symbol();

                        if (!function_symbol.is_valid())
                        {
                            std::cerr << "Function '" << function_name.prettyprint() << "' referenced in " 
                                << function_name.get_ast().get_locus() 
                                << " is unknown. It will not be instrumented." << std::endl;
                            return false;
                        }

                        Type function_type = function_symbol.get_type();

                        ObjectList<std::string> parameter_names;
                        
                        std::string shadow_declaration;

                        shadow_declaration = function_type.returns().get_declaration(function_symbol.get_scope(), "");
                        shadow_declaration += shadow_function_name;
                        shadow_declaration += "(";
                        shadow_declaration += "const char* __file, int __line";

                        bool has_ellipsis = false;
                        ObjectList<Type> parameters = function_type.parameters(has_ellipsis);

                        if (has_ellipsis)
                        {
                            std::cerr << "Function '" << function_name.prettyprint() << "' referenced in "
                                << function_name.get_ast().get_locus()
                                << " has variable-length arguments. It will not be instrumented." << std::endl;
                            return false;
                        }

                        int param_num = 0;
                        for (ObjectList<Type>::iterator it = parameters.begin();
                                it != parameters.end();
                                it++)
                        {
                            std::stringstream ss;
                            ss << "_p_" << param_num;
                            shadow_declaration += "," + it->get_declaration(function_symbol.get_scope(), ss.str());

                            parameter_names.append(ss.str());
                            param_num++;
                        }

                        if (has_ellipsis)
                        {
                            shadow_declaration += ", ...";
                        }

                        shadow_declaration += ")";
                        // std::string shadow_declaration = function_type.get_declaration_with_parameters(function_symbol.get_scope(), 
                        //         shadow_function_name, parameter_names);

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

                        std::string mangled_function_name = "\"" + function_name.mangle_id_expression() + "\"";

                        before_code
                            << "const int EVENT_CALL_USER_FUNCTION = 60000018;"
                            << "int _user_function_event = mintaka_index_get(__file, __line);"
                            << "if (_user_function_event == -1)"
                            << "{"
                            << "   #pragma omp critical\n"
                            << "   {"
                            << "        _user_function_event = mintaka_index_allocate2(__file, __line, "
                                                  << mangled_function_name << ", EVENT_CALL_USER_FUNCTION);"
                            << "   }"
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

                        return true;
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
                            << "static void __begin_mintaka(char* exec_basename)"
                            << "{"
                            << "  mintaka_app_begin();"
                            << "  mintaka_set_filebase(exec_basename);"
                            // Register events
                            // TODO - OpenMP events descriptions
                            << "  const int EVENT_CALL_USER_FUNCTION = 60000018;"
                            << "  static const char* EVENT_CALL_USER_FUNCTION_DESCR = \"User function call\";"
                            << "  mintaka_index_event(EVENT_CALL_USER_FUNCTION, EVENT_CALL_USER_FUNCTION_DESCR);"
                            // Initialize every thread
                            <<    "#pragma omp parallel\n"
                            <<    "{"
                            <<    "   mintaka_thread_begin(1, omp_get_thread_num() + 1);"
                            <<    "   if (omp_get_thread_num() != 0)"
                            <<    "        mintaka_state_idle();"
                            <<    "   else"
                            <<    "        mintaka_state_run();"
                            <<    "}"
                            << "}"

                            << "static void __end_mintaka(void)"
                            << "{"
                            << "  #pragma omp parallel\n"
                            << "  {"
                            << "     mintaka_thread_end();"
                            << "  }"
                            << "  mintaka_app_end();"
                            << "  mintaka_merge();"
                            << "  mintaka_index_generate();"
                            << "}"

                            << instrumented_main_declaration << ";"
                            << main_declaration
                            << "{"
                            << "  __begin_mintaka(_p_1[0]);"
                            << "  int __result = __instrumented_main(_p_0, _p_1);"
                            << "  __end_mintaka();"
                            << "  return __result;"
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
                AST_t root_node = data_flow["translation_unit"];
                ScopeLink scope_link = data_flow["scope_link"];
                
                // Always define the mutex for mintaka
                Source mintaka_mutex_def;
                mintaka_mutex_def
                    << "long mintaka_mutex;"
                    ;

                AST_t mintaka_mutex_tree = mintaka_mutex_def.parse_global(
                        scope_link.get_scope(root_node), scope_link
                        );

                root_node.prepend_to_translation_unit(mintaka_mutex_tree);

                if (ExternalVars::get("instrument", "0") == "1")
                {
                    std::cerr << "Running instrumentation" << std::endl;
                }
                else
                {
                    std::cerr << "Instrumentation disabled. You can enable with '--variable=instrument:1'" << std::endl;
                    return;
                }

                // Traversal of LANG_IS_FUNCTION_CALLs
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
