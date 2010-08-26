/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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
#include "tl-overload.hpp"

#include "tl-source.hpp"

#include "cxx-utils.h"

#include "cxx-parser.h"
#include "c99-parser.h"

#include "cxx-exprtype.h"
#include "cxx-instantiation.h"

#define integer_types "_Bool, signed char, char, short int, int, long int, " \
                      "unsigned char, unsigned short, unsigned int, unsigned long, long long int, " \
                      "unsigned long long int"
#define complex_types "float _Complex, double _Complex, long double _Complex"
#define real_types    "float, double, long double, " complex_types
#define scalar_types  integer_types ", " real_types

namespace TL
{
    namespace OpenMP
    {
        static void parse_omp_udr_declare_arguments_2(const std::string &omp_udr_str, AST_t ref_tree, 
                ScopeLink sl, std::string &udr_name, ObjectList<UDRParsedInfo>& udr_parsed_info_list,
                AST_t &ref_tree_of_clause, Scope& scope_of_clause);
        static void parse_udr_identity(const std::string& omp_udr_identity, AST_t reference_tree,
                ScopeLink sl, Type udr_type, AST_t &parsed_tree);

        static std::string get_valid_zero_initializer(Type t)
        {
            if (t.is_array())
            {
                return "{" + get_valid_zero_initializer(t.array_element()) + "}";
            }
            else if (t.is_class())
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
                return "constructor()";

            if (t.is_class())
            {
                if (!t.is_pod())
                {
                    // If it is not pod, default initialization should do the right thing
                    return "constructor()";
                }
            }
            // For most cases, get_valid_zero_initializer is enough
            return get_valid_zero_initializer(t);
        }

        // FIXME - This is awful
        // This function creates a fake symbol which is not actually signed in any scope
        // It is used just for builtin UDR which do not have a backing symbol actually
        static scope_entry_t* new_udr_builtin_symbol(type_t* type, 
                const std::string& str, 
                decl_context_t decl_context)
        {
            scope_entry_t* result = (scope_entry_t*)calloc(1, sizeof(*result));
            result->symbol_name = uniquestr(("operator " + str).c_str());
            result->kind = SK_FUNCTION;

            parameter_info_t parameter_info[2] = 
            {
                { 0, type, type },
                { 0, type, type },
            };

            type_t* function_type_info = get_new_function_type(type,
                    parameter_info, 2);

            result->type_information = function_type_info;

            result->decl_context = decl_context;
            result->file = uniquestr("(global-scope)");
            result->line = 0;

            return result;
        }


        void initialize_builtin_udr_reductions_2(AST_t translation_unit, ScopeLink scope_link)
        {
            static bool already_initialized = false;
            if (already_initialized)
                return;
            already_initialized = true;

            typedef struct 
            {
                std::string udr_specifier;
                AST_t identity;
            } reduction_info_t;

            Scope global_scope = scope_link.get_scope(translation_unit);

            AST_t zero(internal_expression_parse("0", global_scope.get_decl_context()));
            AST_t real_zero(internal_expression_parse("0.0", global_scope.get_decl_context()));
            AST_t one(internal_expression_parse("1", global_scope.get_decl_context()));
            AST_t real_one(internal_expression_parse("1.0", global_scope.get_decl_context()));
            AST_t neg_zero(internal_expression_parse("~0", global_scope.get_decl_context()));

            reduction_info_t builtin_arithmetic_operators[] =
            {
                {"+: " integer_types ": _out += _in", zero},
                {"+: " real_types ": _out += _in", real_zero},
                {"-: " real_types ": _out -= _in", zero},
                {"-: " real_types ": _out -= _in", real_zero}, 
                {"*: " integer_types ": _out *= _in", one}, 
                {"*: " real_types ": _out *= _in", real_one},
                {"", AST_t(NULL)}
            };

            reduction_info_t builtin_logic_bit_operators[] =
            {
                {"&: " integer_types ": _out &= _in", neg_zero}, 
                {"|: " integer_types ": _out |= _in", zero}, 
                {"^: " integer_types ": _out ^= _in", zero}, 
                {"&&: " scalar_types ": _out = _out && _in", one}, 
                {"||: " scalar_types ": _out = _out || _in", zero}, 
                {"", AST_t(NULL)}
            };


            // call 'parse_omp_udr_declare_arguments_2' to create one UDRInfoItem2 for each builtin case  
            int i = 0;
            for(i; builtin_arithmetic_operators[i].udr_specifier != ""; i++)
            {
			    std::string name;
                ObjectList<UDRParsedInfo> parsed_info_list;
                Scope scope_of_clause;
                AST_t ref_tree_of_clause(NULL);

                parse_omp_udr_declare_arguments_2(builtin_arithmetic_operators[i].udr_specifier, 
                        translation_unit, 
                        scope_link,
                        name,
                        parsed_info_list,
                        ref_tree_of_clause,
                        scope_of_clause);

                ObjectList<UDRInfoItem2> udrs;
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
		            builtin_udr.set_in_symbol((*it).in_symbol);
		            builtin_udr.set_out_symbol((*it).out_symbol);
                    AST_t identity_expr(NULL);
                    parse_udr_identity(builtin_arithmetic_operators[i].identity.prettyprint(), ref_tree_of_clause,
                                scope_link, (*it).type, identity_expr);
                    builtin_udr.set_identity(identity_expr);
                    builtin_udr.set_function_name("");    // Builtin UDRs don't need a function name

                    builtin_udr.sign_in_scope(global_scope, (*it).type);
                }
            }
        }

        struct OnlyMembers : Predicate<Symbol>
        {
            virtual bool do_(Symbol& sym) const
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
            virtual bool do_(Symbol& sym) const
            {
                return !OnlyMembers()(sym);
            }
        };

        // omp_udr_declare_arg_2 : omp_udr_operator_2 ':' omp_udr_type_specifier ':' omp_udr_expression
        // {
        //     $$ = ASTMake3(AST_OMP_UDR_DECLARE_ARG_2, $1, $3, $5, ASTFileName($1), ASTLine($1), NULL);
        // }
        static void parse_omp_udr_declare_arguments_2(const std::string &omp_udr_str, 
                AST_t ref_tree, 
                ScopeLink sl,
                std::string &udr_name,
                ObjectList<UDRParsedInfo>& udr_parsed_info_list,
                AST_t &ref_tree_of_clause,
                Scope& scope_of_clause)
        {
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
            ref_tree_of_clause = AST_t(a);
            scope_of_clause = Scope(decl_context);

            udr_name = AST_t(id_expr).prettyprint();

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
                
                build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                        decl_context);

                type_t* declarator_type = type_info;
                compute_declarator_type(abstract_decl, &gather_info, type_info, &declarator_type,
                        decl_context);

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

	    	    bool res = check_for_expression(expression, new_context);
	    		if (!res)
                {
                    running_error("%s: error: invalid expression '%s' for OpenMP UDR reduction\n", 
	    				ast_location(combine_expr), prettyprint_in_buffer(combine_expr));
                }

				scope_link_set(sl.get_internal_scope_link(), expression, new_context);

                udr_parsed_info.combine_expression = AST_t(expression);
                udr_parsed_info.in_symbol = Symbol(in_symbol);
                udr_parsed_info.out_symbol = Symbol(out_symbol);

                udr_parsed_info_list.append(udr_parsed_info);
            }    
        }

        static void parse_udr_identity(const std::string& omp_udr_identity,
                AST_t reference_tree,
                ScopeLink sl,
                Type udr_type,
                AST_t &parsed_tree)
        {
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
                check_for_initializer_clause(a, decl_context, udr_type.get_internal_type());
            }
            else
            {
                AST omp_udr_args = ASTSon0(a);
                AST expr_list = ASTSon0(omp_udr_args);

                if (expr_list != NULL)
                {
                    check_for_expression_list(expr_list, decl_context);
                }
            }

            parsed_tree = AST_t(a);

            free(str);
        }

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
            AST_t ref_tree_of_clause(NULL);
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
                        (*it).type);

                if (!found)
                {
                    // Identity treatment
                    AST_t identity_expr(NULL);
                    PragmaCustomClause identity_clause = construct.get_clause("identity");
                    if (identity_clause.is_defined())
                    {
                        std::string identity_str = identity_clause.get_arguments(ExpressionTokenizerTrim())[0];

                        parse_udr_identity(identity_str, ref_tree_of_clause,
                                construct.get_scope_link(), (*it).type, identity_expr);
                        new_udr.set_identity(identity_expr);
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

                        AST_t default_identity_expr;
                        parse_udr_identity(initializer, ref_tree_of_clause, 
                                construct.get_scope_link(), (*it).type, default_identity_expr);
                        new_udr.set_identity(default_identity_expr);
                    }

                    std::string function_name = new_udr.get_symbol_name((*it).type);
		            function_name = function_name.substr(1, function_name.size());   // symbol name without initial dot
                    new_udr.set_function_name(function_name);

                    new_udr.sign_in_scope(construct.get_scope(), (*it).type);

                    udrs.append(new_udr);

                    std::cerr << construct.get_ast().get_locus() << ": note: declaring user-defined reduction with identifier '"
                            << name << "' and type '"
                            << ((*it).type).get_declaration(scope_of_clause, "") << "'"
                            << std::endl;
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

        void Core::declare_reduction_handler_post_2(PragmaCustomConstruct construct) { }


        UDRInfoItem2::UDRInfoItem2(): 
            _name(""),
            _type(NULL),
            _combine_expression(NULL),
            _in_symbol(NULL),
            _out_symbol(NULL),
            _is_builtin(false),
            _has_identity(false),
            _identity(NULL),
            _function_name("")
        {
        }

        // UDRInfoItem2 Methods
        std::string UDRInfoItem2::get_symbol_name(Type t) const
        {
		    Type canonic_type = t;
		    canonic_type = canonic_type.get_canonical_type();

			std::stringstream ss;
			ss << canonic_type.get_internal_type();

		    return (".udr_" + _name + "_" + ss.str());
        }


        void UDRInfoItem2::sign_in_scope(Scope sc, Type type) const
        {
            Symbol sym = sc.new_artificial_symbol(this->get_symbol_name(type));

            RefPtr<UDRInfoItem2> cp(new UDRInfoItem2(*this));

            sym.set_attribute("udr_info", cp);

            DEBUG_CODE()
            {             
                    std::cerr << "UDR: Signing in " << this->get_symbol_name(type) << std::endl;
            }
        }

        UDRInfoItem2 UDRInfoItem2::lookup_udr(Scope sc,
                bool &found,
                Type type) const
        {
            DEBUG_CODE()
            {
                std::cerr << "UDR: Lookup start" << std::endl;
            }

            const UDRInfoItem2& current_udr = *this;

            found = false;
            ObjectList<Symbol> lookup = sc.get_symbols_from_name(current_udr.get_symbol_name(type));
            if (!lookup.empty())
            {
                found = true;
            }

            return current_udr;
        }


        UDRInfoItem2 UDRInfoItem2::lookup_udr_2(Scope sc,
                bool &found,
                Type type) const
        {
            DEBUG_CODE()
            {
                std::cerr << "UDR: Lookup start" << std::endl;
            }

            found = false;

            UDRInfoItem2 new_udr;
            ObjectList<Symbol> lookup = sc.get_symbols_from_name(this->get_symbol_name(type));
            if (!lookup.empty())
            {
                found = true;
                RefPtr<UDRInfoItem2> obj = 
                    RefPtr<UDRInfoItem2>::cast_dynamic(lookup.at(0).get_attribute("udr_info"));
                new_udr = (*obj); 
            }

            return new_udr;
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

        AST_t UDRInfoItem2::get_combine_expr() const
        {
            return _combine_expression;
        }

        void UDRInfoItem2::set_combine_expr(AST_t combine_expr)
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

        bool UDRInfoItem2::udr_is_builtin_operator_2(const std::string& op_name)
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

        bool UDRInfoItem2::has_identity() const
        {
            return _has_identity;
        }

        AST_t UDRInfoItem2::get_identity() const
        {
            if (identity_is_constructor())
            {
                return _identity.children()[0];
            }
            else
                return _identity;
        }

        AST_t UDRInfoItem2::get_raw_identity() const
        {
            return _identity;
        }

        void UDRInfoItem2::set_identity(AST_t identity)
        {
            _identity = identity;
            _has_identity = _identity.is_valid();
        }

        bool UDRInfoItem2::identity_is_constructor() const
        {
            if (_identity.is_valid())
            {
                return _identity.internal_ast_type_() == AST_OMP_UDR_CONSTRUCTOR;
            }
            else 
                return false;
        }

        std::string UDRInfoItem2::get_function_name() const
        {
            return _function_name;
        }

        void UDRInfoItem2::set_function_name(const std::string& str)
        {
            _function_name = str;
        }

    }
}
