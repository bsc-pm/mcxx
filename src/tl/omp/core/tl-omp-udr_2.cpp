/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information 
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/



#include "tl-omp-core.hpp"
#include "tl-omp-udr_2.hpp"
#include "tl-source.hpp"

#include "cxx-parser.h"
#include "c99-parser.h"

#include "cxx-ambiguity.h"
#include "cxx-exprtype.h"
#include "cxx-koenig.h"
#include "cxx-instantiation.h"
#include "cxx-utils.h"


namespace TL
{
    namespace OpenMP
    {
        static void parse_omp_udr_declare_arguments_2(const std::string &omp_udr_str, Nodecl::NodeclBase ref_tree, 
                std::string &udr_name, ObjectList<UDRParsedInfo>& udr_parsed_info_list,
                Nodecl::NodeclBase &ref_tree_of_clause, Scope& scope_of_clause);
        static void parse_udr_identity(const std::string& omp_udr_identity, Nodecl::NodeclBase reference_tree,
                Type udr_type, Nodecl::NodeclBase &parsed_tree, bool& is_constructor, bool& need_equal_initializer);

        static std::string get_valid_zero_initializer(Type t)
        {
            if (t.is_class())
            {
                ObjectList<Symbol> nonstatic_data = t.get_nonstatic_data_members();
                if (nonstatic_data.empty())
                {
                    return "{ }";
                }
                else
                {
                    return "{" + get_valid_zero_initializer(t.get_nonstatic_data_members()[0].get_type()) + "}";
                }
            }
            else
            {
                return "0";
            }
        }

        static std::string get_valid_value_initializer(Type t)
        {
            // Fall back
            if (t.is_dependent())
                return "";

            if (t.is_class())
            {
                if (!t.is_pod())
                {
                    // If it is not pod, default initialization should do the right thing
                    return "";
                }
            }
            // For most cases, get_valid_zero_initializer is enough
            return get_valid_zero_initializer(t);
        }

        void initialize_builtin_udr_reductions_2(Nodecl::NodeclBase translation_unit)
        {
            internal_error("Not implemented yet", 0);
#if 0
            static bool already_initialized = false;
            if (already_initialized)
                return;
            already_initialized = true;

            typedef struct 
            {
                std::string udr_specifier;
                std::string identity;
            } reduction_info_t;

            Scope global_scope = scope_link.get_scope(translation_unit);

            // FIXME - This should be moved to nodecl!!!
            std::string zero = "0";
            std::string real_zero = "0.0";
            std::string one = "1";
            std::string real_one = "1.0";
            std::string neg_zero = "~0";

            const std::string complex_types = "float _Complex, double _Complex, long double _Complex ";
            const std::string real_types = "float, double, long double, " + complex_types;

            std::string integer_types = "";
			CXX_LANGUAGE()
            {
                integer_types = "bool, ";
            }
            C_LANGUAGE()
            {
                integer_types = "_Bool, "; 
            }
			integer_types += "signed char, char, short int, int, long int, " \
                      "unsigned char, unsigned short, unsigned int, unsigned long, long long int, " \
                      "unsigned long long int";
            const std::string scalar_types = integer_types + ", " + real_types;

            reduction_info_t builtin_operators[] =
            {
                // arithmetic operators
                {"+: " + integer_types + ": _out += _in", zero},
                {"+: " + real_types + ": _out += _in", real_zero},
                {"-: " + integer_types + ": _out -= _in", zero},
                {"-: " + real_types + ": _out -= _in", real_zero}, 
                {"*: " + integer_types + ": _out *= _in", one}, 
                {"*: " + real_types + ": _out *= _in", real_one},
                // logic bit operators
                {"&: " + integer_types + ": _out &= _in", neg_zero}, 
                {"|: " + integer_types + ": _out |= _in", zero}, 
                {"^: " + integer_types + ": _out ^= _in", zero}, 
                {"&&: " + scalar_types + ": _out = _out && _in", one}, 
                {"||: " + scalar_types + ": _out = _out || _in", zero},
                {"", ""}
            };

            // call 'parse_omp_udr_declare_arguments_2' to create one UDRInfoItem2 for each builtin case  
            int i = 0;
            for(i; builtin_operators[i].udr_specifier != ""; i++)
            {
			    std::string name;
                ObjectList<UDRParsedInfo> parsed_info_list;
                Scope scope_of_clause;
                Nodecl::NodeclBase ref_tree_of_clause(NULL);

                parse_omp_udr_declare_arguments_2(builtin_operators[i].udr_specifier, 
                        translation_unit, 
                        scope_link,
                        name,
                        parsed_info_list,
                        ref_tree_of_clause,
                        scope_of_clause);

                std::string udr_sp = builtin_operators[i].udr_specifier;
		        // Declare a new UDR for each type
		        for (ObjectList<UDRParsedInfo>::iterator it = parsed_info_list.begin();
		                it != parsed_info_list.end();
		                it++)
		        {
		            // New udr being declared
		            bool found = false;
		            UDRInfoItem2 builtin_udr;
		            builtin_udr.set_name(name);
		            builtin_udr.set_type((*it).type);
		            builtin_udr.set_combine_expr((*it).combine_expression);
                    builtin_udr.set_is_builtin_operator(true);
                    
                    builtin_udr.set_builtin_operator(udr_sp.substr(0, udr_sp.find(':')));
		            builtin_udr.set_in_symbol((*it).in_symbol);
		            builtin_udr.set_out_symbol((*it).out_symbol);
                    Nodecl::NodeclBase identity_expr(NULL);
                    bool is_constructor, need_equal_initializer;
                    parse_udr_identity(builtin_operators[i].identity, ref_tree_of_clause,
                                scope_link, (*it).type, identity_expr, is_constructor, need_equal_initializer);
                    builtin_udr.set_identity(identity_expr);
                    builtin_udr.set_is_constructor(false);
                    builtin_udr.set_need_equal_initializer(false);
                    builtin_udr.set_function_definition_symbol(NULL);    // Builtin UDRs don't have a function definition

                    builtin_udr.sign_in_scope(global_scope, (*it).type);
                }
            }
#endif
        }

        struct OnlyMembers : Predicate<Symbol>
        {
            virtual bool do_(OnlyMembers::ArgType sym) const
            {
                // Well, it turns that the frontend is not properly labelling template names
                // as being members
                Symbol current = sym;
                if (current.get_type().is_template_type())
                {
                    current = current.get_type().get_primary_template().get_symbol();
                }
                return current.is_member()
                    && !current.is_static();
            }
        };

        struct OnlyNonMembers : Predicate<Symbol>
        {
            virtual bool do_(OnlyNonMembers::ArgType sym) const
            {
                return !OnlyMembers()(sym);
            }
        };


        Nodecl::NodeclBase UDRInfoItem2::parse_omp_udr_operator_name(
                Source::ReferenceScope ref_scope,
                const std::string &omp_udr_oper_name)
        {
            internal_error("Not implemented yet", 0);
#if 0
            std::string mangled_str = "@OMP_OPERATOR_NAME@ " + omp_udr_oper_name;
            char* str = strdup(mangled_str.c_str());

            C_LANGUAGE()
            {
                mc99_prepare_string_for_scanning(str);
            }
            CXX_LANGUAGE()
            {
                mcxx_prepare_string_for_scanning(str);
            }

            int parse_result = 0;
            AST a;

            C_LANGUAGE()
            {
                parse_result = mc99parse(&a);
            }
            CXX_LANGUAGE()
            {
                parse_result = mcxxparse(&a);
            }

            if (parse_result != 0)
            {
                running_error("Could not parse OpenMP user-defined reduction operator name\n\n%s\n", 
                        TL::Source::format_source(mangled_str).c_str());
            }

            // Get the scope and declarating context of the reference tree
            Scope sc = sl.get_scope(ref_tree);
            decl_context_t decl_context = sc.get_decl_context();

            nodecl_t nodecl_output = nodecl_null();

            enter_test_expression();
            check_expression(a, decl_context, &nodecl_output);
            leave_test_expression();

            // Set properly the context of the reference tree
            scope_link_t* _scope_link = sl.get_internal_scope_link();
            scope_link_set(_scope_link, a, decl_context);

            Nodecl::NodeclBase result(a);
            return result;
#endif
        }

        // omp_udr_declare_arg_2 : omp_udr_operator_2 ':' omp_udr_type_specifier ':' omp_udr_expression
        // {
        //     $$ = ASTMake3(AST_OMP_UDR_DECLARE_ARG_2, $1, $3, $5, ASTFileName($1), ASTLine($1), NULL);
        // }
        static void parse_omp_udr_declare_arguments_2(const std::string &omp_udr_str, 
                Nodecl::NodeclBase ref_tree, 
                std::string &udr_name,
                ObjectList<UDRParsedInfo>& udr_parsed_info_list,
                Nodecl::NodeclBase &ref_tree_of_clause,
                Scope& scope_of_clause)
        {
            internal_error("Not yet implemented", 0);
#if 0
            std::stringstream ss;
            ss << "#line " << ref_tree.get_line() << " \"" << ref_tree.get_file() << "\"\n";

            std::string mangled_str = ss.str() + "@OMP_UDR_DECLARE_2@ " + omp_udr_str;

            char *str = strdup(mangled_str.c_str());
            C_LANGUAGE()
            {
                mc99_prepare_string_for_scanning(str);
            }
            CXX_LANGUAGE()
            {
                mcxx_prepare_string_for_scanning(str);
            }

            int parse_result = 0;
            AST a = NULL;
            CXX_LANGUAGE()
            {
                parse_result = mcxxparse(&a);
            }
            C_LANGUAGE()
            {
                parse_result = mc99parse(&a);
            }

            if (parse_result != 0)
            {
                running_error("Could not parse OpenMP user-defined reduction argument\n\n%s\n", 
                        TL::Source::format_source(mangled_str).c_str());
            } 

            free(str);

            Scope sc = sl.get_scope(ref_tree);
            decl_context_t decl_context = sc.get_decl_context();

            scope_link_t* _scope_link = sl.get_internal_scope_link();

            AST id_expr = ASTSon0(a);
            AST type_expr = ASTSon1(a);
            AST combine_expr = ASTSon2(a);

            // Set the proper scope link
            scope_link_set(_scope_link, a, decl_context);
            ref_tree_of_clause = Nodecl::NodeclBase(a);
            scope_of_clause = Scope(decl_context);

            udr_name = Nodecl::NodeclBase(id_expr).prettyprint();

            AST type_it;
      
            for_each_element(type_expr, type_it)
            {
                UDRParsedInfo udr_parsed_info;
                decl_context_t new_context = new_block_context(decl_context);

                // Build type
                AST type_id = ASTSon1(type_it);

                type_t* type_info = NULL;
                gather_decl_spec_t gather_info;
                memset(&gather_info, 0, sizeof(gather_info));

                AST type_specifier_seq = ASTSon0(type_id);
                AST abstract_decl = ASTSon1(type_id);
                
                nodecl_t dummy_nodecl_output = { NULL };
                build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                        decl_context, &dummy_nodecl_output);

                type_t* declarator_type = type_info;
                compute_declarator_type(abstract_decl, &gather_info, type_info, &declarator_type,
                        decl_context, &dummy_nodecl_output);

                udr_parsed_info.type = Type(declarator_type);

                // Check combine expression
                AST expression = ast_copy(combine_expr);

                scope_entry_t* out_symbol = new_symbol(new_context, new_context.current_scope, "_out");
    			out_symbol->kind = SK_VARIABLE;
    			out_symbol->file = ASTFileName(expression);
    			out_symbol->line = ASTLine(expression);
    			out_symbol->type_information = declarator_type;

                scope_entry_t* in_symbol = new_symbol(new_context, new_context.current_scope, "_in");
    			in_symbol->kind = SK_VARIABLE;
	    		in_symbol->file = ASTFileName(expression);
	    		in_symbol->line = ASTLine(expression);
	    		in_symbol->type_information = get_const_qualified_type(declarator_type);

                nodecl_t nodecl_output = nodecl_null();
	    	    bool res = check_expression(expression, new_context, &nodecl_output);
	    		if (!res)
                {
                    running_error("%s: error: invalid expression '%s' for OpenMP UDR reduction\n", 
	    				ast_location(combine_expr), prettyprint_in_buffer(combine_expr));
                }

				scope_link_set(sl.get_internal_scope_link(), expression, new_context);

                udr_parsed_info.combine_expression = Nodecl::NodeclBase(expression);
                udr_parsed_info.in_symbol = Symbol(in_symbol);
                udr_parsed_info.out_symbol = Symbol(out_symbol);

                udr_parsed_info_list.append(udr_parsed_info);
            }    
#endif
        }

        static void parse_udr_identity(const std::string& omp_udr_identity,
                Nodecl::NodeclBase reference_tree,
                Type udr_type,
                Nodecl::NodeclBase &parsed_tree,
                bool& is_constructor,
                bool& need_equal)
        {
            internal_error("Not yet implemented", 0);
#if 0
            std::stringstream ss;
            ss << "#line " << reference_tree.get_line() << " \"" << reference_tree.get_file() << "\"\n";

            std::string parsed_string = ss.str() + "@OMP_UDR_IDENTITY@ ";

            std::string constructor_str = "constructor";

            // Replace 'constructor' with a special token (otherwise
            // 'constructor'syntax  would be seen as a plain function call)
            if (omp_udr_identity.substr(0, constructor_str.size()) == constructor_str)
            {
                parsed_string += "@OMP_UDR_CONSTRUCTOR@" + omp_udr_identity.substr(constructor_str.size());
            }
            else
            {
                parsed_string += omp_udr_identity;
            }

            char *str = strdup(parsed_string.c_str());

            C_LANGUAGE()
            {
                mc99_prepare_string_for_scanning(str);
            }
            CXX_LANGUAGE()
            {
                mcxx_prepare_string_for_scanning(str);
            }

            int parse_result = 0;
            AST a;

            CXX_LANGUAGE()
            {
                parse_result = mcxxparse(&a);
            }
            C_LANGUAGE()
            {
                parse_result = mc99parse(&a);
            }

            if (parse_result != 0)
            {
                running_error("Could not parse OpenMP user-defined reduction identity\n\n%s\n", 
                        TL::Source::format_source(parsed_string).c_str());
            }

            Scope sc = sl.get_scope(reference_tree);
            decl_context_t decl_context = sc.get_decl_context();

            if (ASTType(a) != AST_OMP_UDR_CONSTRUCTOR)
            {
                is_constructor = false;
                // check_initializer_clause(a, decl_context, udr_type.get_internal_type());
            }
            else
            {
                is_constructor = true;
                AST omp_udr_args = ASTSon0(a);
                AST expr_list = ASTSon0(omp_udr_args);

                if (expr_list != NULL)
                {
                    // check_expression_list(expr_list, decl_context);
                }
            }

            parsed_tree = Nodecl::NodeclBase(a);

            if (ast_get_type(a)==AST_INITIALIZER_BRACES)
            {
                need_equal = true;
            }
            else
            {
                need_equal = false;
            }

            free(str);
#endif
        }

#if 0
        void Core::declare_reduction_handler_pre_2(PragmaCustomConstruct construct)
        {
            DEBUG_CODE()
            {
                std::cerr << "=== Declare reduction 2 [" << construct.get_ast().get_locus() << "]===" << std::endl;
            }

			// NEW: #pragma omp declare reduction(op-name-list : type-list : combine-expr) [ identity( expr ) ]
            ScopeLink scope_link = construct.get_scope_link();

            if (!construct.is_parameterized())
            {
                std::cerr << construct.get_ast().get_locus() << ": warning: skipping 'declare reduction 2' pragma with wrong syntax" << std::endl;
                return;
            }

            std::string parameter_str = construct.get_parameter_arguments()[0];

			std::string name;
            ObjectList<UDRParsedInfo> parsed_info_list;
            Scope scope_of_clause;
            Nodecl::NodeclBase ref_tree_of_clause(NULL);
            parse_omp_udr_declare_arguments_2(parameter_str,
                    construct.get_ast(),
                    construct.get_scope_link(),
                    name,
                    parsed_info_list,
                    ref_tree_of_clause,
                    scope_of_clause);

            ObjectList<Symbol> op_symbols;

            // Remove any cv-qualifications for each type
            for (ObjectList<UDRParsedInfo>::iterator parsed_info_it = parsed_info_list.begin();
                    parsed_info_it != parsed_info_list.end();
                    parsed_info_it++)
            {
                Type &reduction_type(parsed_info_it->type);
                reduction_type = reduction_type.advance_over_typedefs().get_unqualified_type();

                if (reduction_type.is_function()
  		    	        || reduction_type.is_array() 
			    	    || reduction_type.is_reference())
                {
                    running_error("%s: error: '%s' is not a valid type for a declare reduction directive",
                            construct.get_ast().get_locus().c_str(),
                            reduction_type.get_declaration(construct.get_scope(), "").c_str());
                }
            }

            ObjectList<UDRInfoItem2> udrs;
            // Declare a new UDR for each type
            for (ObjectList<UDRParsedInfo>::iterator it = parsed_info_list.begin();
                    it != parsed_info_list.end();
                    it++)
            {
                // New udr being declared
                bool found = false;
                UDRInfoItem2 new_udr;
                new_udr.set_name(name);
                new_udr.set_type((*it).type);
                new_udr.set_combine_expr((*it).combine_expression);
                new_udr.set_in_symbol((*it).in_symbol);
                new_udr.set_out_symbol((*it).out_symbol);
                new_udr.lookup_udr(construct.get_scope(), 
                        found,
                        (*it).type,
                        NULL, /* tree for koenig lookup */
                        -1);

                if (!found)
                {
                    // Identity treatment
                    Nodecl::NodeclBase identity_expr(NULL);
                    PragmaCustomClause identity_clause = construct.get_clause("identity");
                    bool is_constructor, need_equal;
                    if (identity_clause.is_defined())
                    {
                        std::string identity_str = identity_clause.get_arguments(ExpressionTokenizerTrim())[0];

                        parse_udr_identity(identity_str, ref_tree_of_clause,
                                construct.get_scope_link(), (*it).type, identity_expr, is_constructor, need_equal);
                        new_udr.set_identity(identity_expr);
                        new_udr.set_is_constructor(is_constructor);
                        new_udr.set_need_equal_initializer(need_equal);
                    }
                    else
                    {
                        std::string initializer;
                        C_LANGUAGE()
                        {
                            initializer = get_valid_zero_initializer((*it).type);
                        }
                        CXX_LANGUAGE()
                        {
                            initializer = get_valid_value_initializer((*it).type);
                        }
                        if (initializer != "")
                        {
		                    Nodecl::NodeclBase default_identity_expr;
		                    parse_udr_identity(initializer, ref_tree_of_clause, 
		                            construct.get_scope_link(), (*it).type, default_identity_expr, is_constructor, need_equal);
		                    new_udr.set_identity(default_identity_expr);
		                    new_udr.set_is_constructor(false);
		                    new_udr.set_need_equal_initializer(true);
                        }
                    }

                    std::stringstream ss;
                    ss << _udr_counter;
                    std::string function_name = new_udr.get_symbol_name((*it).type) + "_" + ss.str();
		            function_name = function_name.substr(1, function_name.size());   // symbol name without initial dot

                    new_udr.sign_in_scope(construct.get_scope(), (*it).type);

                    udrs.append(new_udr);

                    std::cerr << construct.get_ast().get_locus() << ": note: declaring user-defined reduction with identifier '"
                            << name << "' and type '"
                            << ((*it).type).get_declaration(scope_of_clause, "") << "'"
                            << std::endl;

                    _udr_counter++;
                }
                else
                {
                    running_error("%s: error: user defined reduction for identifier '%s' and type '%s' already defined",
                            construct.get_ast().get_locus().c_str(),
                            name.c_str(),
                            ((*it).type).get_declaration(construct.get_scope_link().get_scope(construct.get_ast()), "").c_str());
                }
            }

            _openmp_info->set_udr_list(construct.get_ast(), udrs);
        }
#endif

#if 0
        void Core::declare_reduction_handler_post_2(PragmaCustomConstruct ctr) 
        {
		    if (_new_udr)
	        {
	            ObjectList<OpenMP::UDRInfoItem2> udr_list = _openmp_info->get_udr_list(ctr.get_ast());
                ObjectList<Symbol> udr_symbol_list;
	            for(ObjectList<OpenMP::UDRInfoItem2>::iterator it = udr_list.begin();
	                    it != udr_list.end(); 
	                    it++)
	            {
	                Source pragma_functions;
			        OpenMP::UDRInfoItem2 udr2 = (*it);
			        Type udr_type = udr2.get_type();

			        Symbol out = udr2.get_out_symbol();
			        Symbol in = udr2.get_in_symbol();
			        std::string function_name = udr2.get_symbol_name(udr_type);

					// Remove initial period 
                    function_name = function_name.substr(1, function_name.size());

			        pragma_functions
			            << "static void " << function_name
			            << " ("
			        ;

			        C_LANGUAGE()
			        {
	                    Source combine_expr_replace;
			            pragma_functions
			                << out.get_type().get_pointer_to().get_declaration(out.get_scope(), out.get_name()) 
			                << ", " 
			                << in.get_type().get_pointer_to().get_declaration(in.get_scope(), in.get_name())
			                << ")"
			                << "{ " 
			                << combine_expr_replace << ";"
			                << "}"
			            ;

			            ReplaceSrcIdExpression replace_udr_sym(ctr.get_scope_link());
			            replace_udr_sym.add_replacement(in, "(*" + in.get_name() + ")");
			            replace_udr_sym.add_replacement(out, "(*" + out.get_name() + ")");
			            combine_expr_replace << replace_udr_sym.replace(udr2.get_combine_expr());
			        }

			        CXX_LANGUAGE()
			        {
			            pragma_functions
			                << out.get_type().get_reference_to().get_declaration(out.get_scope(), out.get_name())
			                << ", " 
			                << in.get_type().get_reference_to().get_declaration(in.get_scope(), in.get_name())
			                << ")"
			                << "{ " 
			                << udr2.get_combine_expr().prettyprint() << ";"
			                << "}"
			            ;
			        }

                    Symbol function_sym;
                    TL::Nodecl::NodeclBase pragma_functions_tree;
                    C_LANGUAGE()
                    {
		                if (ctr.get_ast().get_enclosing_function_definition().is_valid())
		                {
							TL::Nodecl::NodeclBase ref_tree = ctr.get_ast().get_enclosing_function_definition_declaration();
			                pragma_functions_tree = pragma_functions.parse_declaration(ref_tree, ctr.get_scope_link());
			                ref_tree.prepend(pragma_functions_tree);
                        }
		                else 
		                {
			                pragma_functions_tree = pragma_functions.parse_declaration(ctr.get_ast(),
			                        ctr.get_scope_link());
			                ctr.get_ast().prepend(pragma_functions_tree);
                        }
                    }

                    CXX_LANGUAGE()
                    {
                        if (ctr.get_scope().inside_class_scope())
                        {
                            if (ctr.get_scope().inside_block_scope())
                            {
				                FunctionDefinition func_def(ctr.get_ast().get_enclosing_function_definition(), ctr.get_scope_link());
				                IdExpression func_id = func_def.get_function_name();
		                        if (func_id.is_unqualified())
		                        {
									pragma_functions_tree = pragma_functions.parse_member(ctr.get_ast(),
									        ctr.get_scope_link(), ctr.get_scope().get_class_of_scope());
		                        }
		                        else
		                        {
		                            pragma_functions_tree = pragma_functions.parse_declaration(ctr.get_ast(),
							                ctr.get_scope_link());
		                        }
                                TL::Nodecl::NodeclBase ref_tree = ctr.get_ast().get_enclosing_function_definition_declaration();
                                ref_tree.prepend(pragma_functions_tree);
                            }
                            else
                            {
		                         pragma_functions_tree = pragma_functions.parse_member(ctr.get_ast(),
									        ctr.get_scope_link(), ctr.get_scope().get_class_of_scope());
                                 ctr.get_ast().prepend(pragma_functions_tree);
                            }
                        }
                        else
                        {
					        pragma_functions_tree = pragma_functions.parse_declaration(ctr.get_ast(),
					                ctr.get_scope_link());
                            ctr.get_ast().prepend(pragma_functions_tree);
                        }
                    }

                    FunctionDefinition function_def(pragma_functions_tree, ctr.get_scope_link());
					function_sym = function_def.get_function_symbol();

					udr_symbol_list.append(function_sym);

                    // Add the symbol to the UDR info
                    (*it).set_function_definition_symbol(function_sym);
                     RefPtr<UDRInfoItem2> cp(new UDRInfoItem2(*it));
                    ctr.get_scope().get_symbol_from_name(udr2.get_symbol_name(udr_type)).set_attribute("udr_info", cp);
	            }
                ctr.get_ast().remove_in_list();
	        }
	        else
	        {
	            // Do nothing but remove the directive
	            ctr.get_ast().remove_in_list();
	        }
		}
#endif


        UDRInfoItem2::UDRInfoItem2(): 
            _name(""),
            _type(NULL),
            _combine_expression(),
            _in_symbol(),
            _out_symbol(),
            _is_builtin(false),
            _builtin_op(""),
            _has_identity(false),
            _identity(),
            _function_definition_symbol()
        {
        }

        // UDRInfoItem2 Methods
        std::string UDRInfoItem2::get_symbol_name(Type t) const
        {
		    Type canonic_type = t.get_unqualified_type().get_canonical_type();

			std::stringstream ss;
			ss << canonic_type.get_internal_type();

		    return (".udr_" + _name + "_" + ss.str());
        }


        void UDRInfoItem2::sign_in_scope(Scope sc, Type type) const
        {
            std::string sym_name = this->get_symbol_name(type);
            Symbol sym = sc.new_artificial_symbol(sym_name);

            RefPtr<UDRInfoItem2> cp(new UDRInfoItem2(*this));
            sym.set_attribute("udr_info", cp);

            DEBUG_CODE()
            {
                    std::cerr << "UDR: Signing in '" << sym_name << std::endl;
            }
        }

        static void find_bases(Type t, ObjectList<Symbol> &bases)
        {
            ObjectList<Symbol> actual_bases = t.get_bases_class_symbol_list();
            if (actual_bases.empty())
            {
                return;
            }

            // Append the founded bases if needed
            for(ObjectList<Symbol>::iterator it=actual_bases.begin();
                    it != actual_bases.end();
                    it++)
            {
                if (!bases.contains(*it)) bases.append(*it);
            }

            // Recursive call for each base
            for(ObjectList<Symbol>::iterator it=actual_bases.begin();
                    it != actual_bases.end();
                    it++)
            {
                if (it->get_type().is_class())
                {
                    find_bases(it->get_type(), bases);
                }
            }
        }

        UDRInfoItem2 UDRInfoItem2::bases_lookup(Type type,
                Nodecl::NodeclBase reductor_tree,
                bool &found) const
        {
            UDRInfoItem2 udr2;
	        ObjectList<Symbol> bases;
	        find_bases(type, bases);
            ObjectList<Symbol> candidate_bases;
            for (int i=0; i<bases.size(); i++)
			{
	            if (bases[i].get_type().is_class())
	            {
	                std::string sym_name = this->get_symbol_name(bases[i].get_type());
					ObjectList<Symbol> lookup = bases[i].get_scope().get_symbols_from_name(sym_name);
					if (!lookup.empty())
					{
                        candidate_bases.append(lookup);
					}
	            }
			}
            if (!candidate_bases.empty())
            {
	            if (candidate_bases.size()>1)
	            {
			        running_error("%s: error: ambiguous user defined reduction with identifier '%s'\n",
			                reductor_tree.get_locus().c_str(),
			                _name.c_str());
	            }
	            else if (candidate_bases.size()==1)
	            {
					found = true;
					RefPtr<UDRInfoItem2> obj = 
							RefPtr<UDRInfoItem2>::cast_dynamic(candidate_bases[0].get_attribute("udr_info"));
					udr2 = (*obj);
	            }
            }
            return udr2;
        }

        UDRInfoItem2 UDRInfoItem2::argument_dependent_lookup(Type type,
                Nodecl::NodeclBase reductor_tree,
                bool &found,
                Scope sc) const
        {
            internal_error("Not implemented yet", 0);
#if 0
            UDRInfoItem2 udr2;

            ObjectList<Symbol> bases;
            bases.append(type.get_symbol());
            find_bases(type, bases);

     		ObjectList<Type> arg_list;
			arg_list.append(type);

			ObjectList<Symbol> koenig_symbols;
            int candidate_type = -1;
	    	for (int it = 0; it != bases.size(); it++)
		    {
		        std::string sym_name = this->get_symbol_name(bases[it].get_type());
                ObjectList<Symbol> actual_koenig_symbols = sc.koenig_lookup(arg_list, Scope::wrap_symbol_name(sym_name));
                if (!actual_koenig_symbols.empty()) 
                {
                    candidate_type = it;
                    koenig_symbols.append(actual_koenig_symbols);
                }
		    }

			if (!koenig_symbols.empty())
			{
		        if (koenig_symbols.size()>1)
		        {
		            running_error("%s: error: ambiguous user defined reduction with identifier '%s'\n",
		                    reductor_tree.get_locus().c_str(),
		                    _name.c_str());
		        }
		        else
		        {
                    std::string sym_name = this->get_symbol_name(bases[candidate_type].get_type());
					ObjectList<Symbol> lookup = koenig_symbols[0].get_scope().get_symbols_from_name(sym_name);
					found = true;
					RefPtr<UDRInfoItem2> obj = 
							RefPtr<UDRInfoItem2>::cast_dynamic(lookup.at(0).get_attribute("udr_info"));
					udr2 = (*obj);
		        }
			}

            return udr2;
#endif
        }

        UDRInfoItem2 UDRInfoItem2::lookup_udr(Scope sc,
                bool &found,
                Type type,
                Nodecl::NodeclBase reductor_tree,
                int udr_counter) const
        {
            found = false;
            std::string sym_name = this->get_symbol_name(type);

            DEBUG_CODE()
            {
                std::cerr << "UDR: Lookup start '"  << sym_name << "'" << std::endl;
            }

            C_LANGUAGE()
            {
                ObjectList<Symbol> lookup = sc.get_symbols_from_name(sym_name);
		        if (!lookup.empty())
		        {
		            found = true;
				    RefPtr<UDRInfoItem2> obj = 
                            RefPtr<UDRInfoItem2>::cast_dynamic(lookup.at(0).get_attribute("udr_info"));
  				    return (*obj);
		        }
                return *this;
            }

            CXX_LANGUAGE()
            {
                // Simple lookup for declarations
                if (udr_counter==-1)
                {
		            ObjectList<Symbol> lookup = sc.get_symbols_from_name(sym_name);
		            if (!lookup.empty())
		            {
		                found = true;
		            }
                    return *this;            
                }

                // Koenig lookup and koenig in bases for unqualified types
                if (type.get_unqualified_type() == type && type.is_named())
                {
                    UDRInfoItem2 koenig_udr = argument_dependent_lookup(type, reductor_tree, found, sc);
                    if (found)
                    {
                        return koenig_udr;
                    }
                }
  
                // Normal lookup and in bases for classe types
                ObjectList<Symbol> lookup = sc.get_symbols_from_name(sym_name);
                if (!lookup.empty())
                {
                    found = true;
					RefPtr<UDRInfoItem2> obj = 
							RefPtr<UDRInfoItem2>::cast_dynamic(lookup.at(0).get_attribute("udr_info"));
					return (*obj);
                }
                if (type.is_class())
                {
                    return bases_lookup(type, reductor_tree, found);
                }
            }

            return *this;
        }

        // UDRInfoItem2 Getters, setters and consults
        std::string UDRInfoItem2::get_name() const
        {
            return _name;
        }

        void UDRInfoItem2::set_name(const std::string& str)
        {
            _name = str;
        }

        Type UDRInfoItem2::get_type() const
        {
            return _type;
        }

        void UDRInfoItem2::set_type(Type t)
        {
            _type = t;
        }

        Nodecl::NodeclBase UDRInfoItem2::get_combine_expr() const
        {
            return _combine_expression;
        }

        void UDRInfoItem2::set_combine_expr(Nodecl::NodeclBase combine_expr)
        {
            _combine_expression = combine_expr;
        }

        Symbol UDRInfoItem2::get_in_symbol() const
        {
            return _in_symbol;
        }

        void UDRInfoItem2::set_in_symbol(Symbol s)
        {
            _in_symbol = s;
        }

        Symbol UDRInfoItem2::get_out_symbol() const
        {
            return _out_symbol;
        }

        void UDRInfoItem2::set_out_symbol(Symbol s)
        {
            _out_symbol = s;
        }

        bool UDRInfoItem2::is_builtin_operator() const
        {
            return _is_builtin;
        }

        bool udr_is_builtin_operator_2(const std::string& op_name)
        {
            return (op_name == "+"
                    || op_name == "-"
                    || op_name == "*"
                    || op_name == "/"
                    || op_name == "&"
                    || op_name == "|"
                    || op_name == "^"
                    || op_name == "&&"
                    || op_name == "||");
        }

        void UDRInfoItem2::set_is_builtin_operator(bool is_builtin)
        {
            _is_builtin = is_builtin;
        }

        std::string UDRInfoItem2::get_builtin_operator() const
        {
            return _builtin_op;
        }
        
        void UDRInfoItem2::set_builtin_operator(const std::string builtin_op)
        {
            _builtin_op = builtin_op;
        }

        bool UDRInfoItem2::get_is_constructor() const
        {
            return _is_constructor;
        }

        void UDRInfoItem2::set_is_constructor(bool constructor)
        {
            _is_constructor = constructor;
        }

        bool UDRInfoItem2::get_need_equal_initializer() const
        {
            return _need_equal_initializer;
        }

        void UDRInfoItem2::set_need_equal_initializer(bool need_equal_init)
        {
            _need_equal_initializer = need_equal_init;
        }

        bool UDRInfoItem2::has_identity() const
        {
            return _has_identity;
        }

        Nodecl::NodeclBase UDRInfoItem2::get_identity() const
        {
            if (identity_is_constructor())
            {
                return _identity.children()[0];
            }
            else
                return _identity;
        }

        Nodecl::NodeclBase UDRInfoItem2::get_raw_identity() const
        {
            return _identity;
        }

        void UDRInfoItem2::set_identity(Nodecl::NodeclBase identity)
        {
            _identity = identity;
            _has_identity = !_identity.is_null();
        }

        bool UDRInfoItem2::identity_is_constructor() const
        {
            internal_error("Not yet implemented", 0);
#if 0
            if (!_identity.is_null())
            {
                return _identity.internal_ast_type_() == AST_OMP_UDR_CONSTRUCTOR;
            }
            else 
                return false;
#endif
        }

        Symbol UDRInfoItem2::get_function_definition_symbol() const
        {
            return _function_definition_symbol;
        }

        void UDRInfoItem2::set_function_definition_symbol(Symbol sym)
        {
            _function_definition_symbol = sym;
        }

    }
}
