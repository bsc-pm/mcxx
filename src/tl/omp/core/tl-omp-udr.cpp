/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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
#include "tl-omp-udr.hpp"
#include "tl-source.hpp"

#include "cxx-parser.h"
#include "c99-parser.h"

#include "cxx-ambiguity.h"
#include "cxx-exprtype.h"
#include "cxx-koenig.h"
#include "cxx-instantiation.h"
#include "cxx-utils.h"
#include "cxx-prettyprint.h"


namespace TL
{
    namespace OpenMP
    {
        static void parse_omp_udr_declare_arguments(const std::string &omp_udr_str, Nodecl::NodeclBase ref_tree,
                std::string &udr_name, ObjectList<UDRParsedInfo>& udr_parsed_info_list);

        static void parse_udr_identity(const std::string& omp_udr_identity,
                Nodecl::NodeclBase ref_tree,
                Type udr_type,
                Nodecl::NodeclBase &parsed_tree,
                bool& is_constructor, 
                bool& need_equal_initializer)
        {
            std::stringstream ss;
            ss << "#line " << ref_tree.get_line() << " \"" << ref_tree.get_filename() << "\"\n";

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

            Scope sc = ref_tree.retrieve_context();
            decl_context_t decl_context = sc.get_decl_context();

            if (ASTType(a) != AST_OMP_UDR_CONSTRUCTOR)
            {
                // Plain expression
                nodecl_t nodecl_expr;
                check_expression(a, decl_context, &nodecl_expr);
                parsed_tree = nodecl_expr;
            }
            else
            {
                is_constructor = true;

                AST omp_udr_args = ASTSon0(a);
                AST expr_list = ASTSon0(omp_udr_args);

                if (expr_list != NULL)
                {
                    nodecl_t nodecl_expr;
                    check_list_of_expressions(expr_list, decl_context, &nodecl_expr);

                    parsed_tree = nodecl_expr;
                }
            }

            if (ast_get_type(a) == AST_INITIALIZER_BRACES)
            {
                need_equal_initializer = true;
            }
            else
            {
                need_equal_initializer = false;
            }
        }

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
            if (t.is_dependent_typename())
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


        Nodecl::NodeclBase UDRInfoItem::parse_omp_udr_operator_name(
                ReferenceScope ref_scope,
                const std::string &omp_udr_oper_name)
        {
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
            Scope sc = ref_scope.get_scope();
            decl_context_t decl_context = sc.get_decl_context();

            nodecl_t nodecl_output = nodecl_null();

            enter_test_expression();
            check_expression(a, decl_context, &nodecl_output);
            leave_test_expression();

            return nodecl_output;
        }

        // omp_udr_declare_arg_2 : omp_udr_operator_2 ':' omp_udr_type_specifier ':' omp_udr_expression
        // {
        //     $$ = ASTMake3(AST_OMP_UDR_DECLARE_ARG_2, $1, $3, $5, ASTFileName($1), ASTLine($1), NULL);
        // }
        static void parse_omp_udr_declare_arguments(const std::string &omp_udr_str, 
                Nodecl::NodeclBase ref_tree, 
                std::string &udr_name,
                ObjectList<UDRParsedInfo>& udr_parsed_info_list)
        {
            std::stringstream ss;
            ss << "#line " << ref_tree.get_line() << " \"" << ref_tree.get_filename() << "\"\n";

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

            Scope sc = ref_tree.retrieve_context();
            decl_context_t decl_context = sc.get_decl_context();

            AST udr_name_tree = ASTSon0(a);
            AST type_expr = ASTSon1(a);
            AST combine_expr = ASTSon2(a);

            // FIXME 
            udr_name = ASTText(udr_name_tree);

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
                        decl_context, /* first_declarator */ NULL, &dummy_nodecl_output);

                type_t* declarator_type = type_info;
                compute_declarator_type(abstract_decl, &gather_info, type_info, &declarator_type,
                        decl_context, abstract_decl, &dummy_nodecl_output);

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

                nodecl_t nodecl_combine_expression = nodecl_null();
	    	    bool res = check_expression(expression, new_context, &nodecl_combine_expression);
	    		if (!res)
                {
                    running_error("%s: error: invalid expression '%s' for OpenMP UDR reduction\n", 
	    				ast_location(combine_expr), prettyprint_in_buffer(combine_expr));
                }

                udr_parsed_info.combine_expression = nodecl_combine_expression;
                udr_parsed_info.in_symbol = Symbol(in_symbol);
                udr_parsed_info.out_symbol = Symbol(out_symbol);

                udr_parsed_info_list.append(udr_parsed_info);
            }    
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

            ObjectList<UDRInfoItem> udrs;
            // Declare a new UDR for each type
            for (ObjectList<UDRParsedInfo>::iterator it = parsed_info_list.begin();
                    it != parsed_info_list.end();
                    it++)
            {
                // New udr being declared
                bool found = false;
                UDRInfoItem new_udr;
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
                    std::string function_name = new_udr.udr_get_symbol_name((*it).type) + "_" + ss.str();
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
	            ObjectList<OpenMP::UDRInfoItem> udr_list = _openmp_info->get_udr_list(ctr.get_ast());
                ObjectList<Symbol> udr_symbol_list;
	            for(ObjectList<OpenMP::UDRInfoItem>::iterator it = udr_list.begin();
	                    it != udr_list.end(); 
	                    it++)
	            {
	                Source pragma_functions;
			        OpenMP::UDRInfoItem udr2 = (*it);
			        Type udr_type = udr2.get_type();

			        Symbol out = udr2.get_out_symbol();
			        Symbol in = udr2.get_in_symbol();
			        std::string function_name = udr2.udr_get_symbol_name(udr_type);

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
                     RefPtr<UDRInfoItem> cp(new UDRInfoItem(*it));
                    ctr.get_scope().get_symbol_from_name(udr2.udr_get_symbol_name(udr_type)).set_attribute("udr_info", cp);
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


        UDRInfoItem::UDRInfoItem(): 
            _name(""),
            _type(NULL),
            _basic_function(NULL),
            _cleanup_function(NULL),
            _is_builtin(false)
            // _combine_expression(),
            // _in_symbol(),
            // _out_symbol(),
            // _builtin_op(""),
            // _has_identity(false),
            // _identity(),
            // _function_definition_symbol()
        {
        }

        // UDRInfoItem Methods
        std::string UDRInfoItem::udr_get_symbol_name(
                const std::string &red_name,
                Type t)
        {
            std::string reductor_name = red_name;

		    Type canonic_type = t.get_unqualified_type().get_canonical_type();

			std::stringstream ss;
			ss << canonic_type.get_internal_type();

		    return (".udr_" + reductor_name + "_" + ss.str());
        }

        TL::Symbol UDRInfoItem::get_symbol_holder() const
        {
            return _symbol_holder;
        }

        void UDRInfoItem::sign_in_scope(Scope sc) const
        {
            std::string sym_name = this->udr_get_symbol_name(_name, _type);
            Symbol sym = sc.new_artificial_symbol(sym_name);

            RefPtr<UDRInfoItem> cp(new UDRInfoItem(*this));
            sym.set_attribute("udr_info", cp);

            cp->_symbol_holder = sym;

            DEBUG_CODE()
            {
                std::cerr << "UDR: Signing in '" << sym_name << std::endl;
            }
        }

        Nodecl::NodeclBase UDRInfoItem::compute_nodecl_of_udr_name(
                const std::string& reductor_name,
                TL::Type udr_type,
                const std::string& filename,
                int line)
        {
            if (IS_C_LANGUAGE
                    || IS_FORTRAN_LANGUAGE)
            {
                return Nodecl::CxxDepNameSimple::make(
                        udr_get_symbol_name(reductor_name, udr_type),
                        filename,
                        line);
            }
            else
            {
                internal_error("Not yet implemented for C++", 0);
            }
        }

        UDRInfoItem* UDRInfoItem::lookup_udr(
                Scope sc,
                Nodecl::NodeclBase reductor_name)
        {
            ERROR_CONDITION(!reductor_name.is<Nodecl::CxxDepNameSimple>(), "Invalid tree", 0);

            std::string udr_name = reductor_name.get_text();

            DEBUG_CODE()
            {
                std::cerr << "UDR: Lookup start '"  << udr_name << "'" << std::endl;
            }

            ObjectList<Symbol> lookup = sc.get_symbols_from_name(udr_name);
            if (!lookup.empty())
            {
                RefPtr<UDRInfoItem> obj =
                    RefPtr<UDRInfoItem>::cast_dynamic(lookup.at(0).get_attribute("udr_info"));
                return obj.get_pointer();
            }
            return NULL;
        }

        // UDRInfoItem Getters, setters and consults
        std::string UDRInfoItem::get_name() const
        {
            return _name;
        }

        void UDRInfoItem::set_name(const std::string& str)
        {
            _name = str;
        }

        Type UDRInfoItem::get_type() const
        {
            return _type;
        }

        void UDRInfoItem::set_type(Type t)
        {
            _type = t;
        }

        bool udr_is_builtin_operator(const std::string& op_name)
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

        void UDRInfoItem::set_is_builtin_operator(bool is_builtin)
        {
            _is_builtin = is_builtin;
        }

        bool UDRInfoItem::is_builtin_operator() const
        {
            return _is_builtin;
        }

        void UDRInfoItem::set_identity(Nodecl::NodeclBase identity)
        {
            _identity = identity;
        }

        Nodecl::NodeclBase UDRInfoItem::get_identity() const
        {
            return _identity;
        }

        Symbol UDRInfoItem::get_basic_reductor_function() const
        {
            return _basic_function;
        }

        void UDRInfoItem::set_basic_reductor_function(Symbol sym)
        {
            _basic_function = sym;
        }

        Symbol UDRInfoItem::get_cleanup_function() const
        {
            return _cleanup_function;
        }

        void UDRInfoItem::set_cleanup_function(Symbol sym)
        {
            _cleanup_function = sym;
        }

        UDRInfoItem& UDRInfoItem::get_udr_info_item_from_symbol_holder(TL::Symbol symbol)
        {
            RefPtr<UDRInfoItem> obj =
                RefPtr<UDRInfoItem>::cast_dynamic(symbol.get_attribute("udr_info"));
            return *obj;
        }
    }
}
