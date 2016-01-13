/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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
#include "tl-omp-reduction.hpp"
#include "cxx-diagnostic.h"
#include "cxx-buildscope.h"
#include "cxx-exprtype.h"
#include "cxx-entrylist.h"
#include "cxx-ambiguity.h"
#include "cxx-prettyprint.h"
#include "cxx-entrylist.h"
#include "cxx-koenig.h"
#include "fortran03-exprtype.h"
#include "fortran03-typeutils.h"
#include "fortran03-buildscope.h"

namespace TL { namespace OpenMP {

    static bool is_cxx_operator(const std::string& op_name)
    {
        if (IS_CXX_LANGUAGE)
        {
            return (op_name == "+"
                    || op_name == "*"
                    || op_name == "-"
                    || op_name == "&"
                    || op_name == "|"
                    || op_name == "^"
                    || op_name == "&&"
                    || op_name == "||");
        }
        return false;
    }

    void Core::get_reduction_symbols(
            TL::PragmaCustomLine construct,
            TL::PragmaCustomClause clause,
            const ObjectList<Symbol>& symbols_in_construct,
            DataEnvironment& data_sharing_environment,
            ObjectList<ReductionSymbol>& sym_list,
            ObjectList<Symbol>& extra_symbols)
    {
        if (!clause.is_defined())
            return;

        ObjectList<std::string> clause_arguments = clause.get_raw_arguments();

        for (ObjectList<std::string>::iterator list_it = clause_arguments.begin();
                list_it != clause_arguments.end();
                list_it++)
        {
            // The first argument is special, we have to look for a ':' that is not followed by any other ':'
            // #pragma omp parallel for reduction(A::F : A::d)
            std::string current_argument = *list_it;

            // Trim blanks
            current_argument.erase(std::remove(current_argument.begin(), current_argument.end(), ' '), current_argument.end());

            std::string::iterator split_colon = current_argument.end();
            for (std::string::iterator it = current_argument.begin();
                    it != current_argument.end();
                    it++)
            {
                if ((*it) == ':'
                        && (it + 1) != current_argument.end())
                {
                    if (*(it + 1) != ':')
                    {
                        split_colon = it;
                        break;
                    }
                    else
                    {
                        // Next one is also a ':' but it is not a valid splitting
                        // ':', so ignore it
                        it++;
                    }
                }
            }

            if (split_colon == current_argument.end())
            {
                error_printf_at(construct.get_locus(),
                        "reduction clause does not have a valid operator. Skipping\n");
                return;
            }

            std::string original_reductor_name;
            std::copy(current_argument.begin(), split_colon, std::back_inserter(original_reductor_name));

            if (IS_FORTRAN_LANGUAGE)
            {
                original_reductor_name = strtolower(original_reductor_name.c_str());
            }

            std::string remainder_arg;
            std::copy(split_colon + 1, current_argument.end(), std::back_inserter(remainder_arg));

            // Tokenize variable list
            ObjectList<std::string> variables = ExpressionTokenizerTrim().tokenize(remainder_arg);

            for (ObjectList<std::string>::iterator it = variables.begin();
                    it != variables.end();
                    it++)
            {
                std::string &variable(*it);
                Source src;
                src
                    << "#line " << construct.get_line() << " \"" << construct.get_filename() << "\"\n"
                    << pad_to_column(construct.get_column()) << variable
                    ;

                Nodecl::NodeclBase var_tree = src.parse_expression(clause.get_pragma_line());

                TL::Symbol var_sym;
                TL::Type var_type;

                if (var_tree.get_symbol().is_valid())
                {
                    // reduction(+ : x)
                    var_sym = var_tree.get_symbol();
                    var_type = var_sym.get_type();
                }
                else if (_allow_array_reductions
                        && (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                        && var_tree.is<Nodecl::Shaping>()
                        && var_tree.as<Nodecl::Shaping>().get_postfix().get_symbol().is_valid())
                {
                    var_sym = var_tree.as<Nodecl::Shaping>().get_postfix().get_symbol();
                    var_type = var_tree.get_type();
                }
                else
                {
                    error_printf_at(construct.get_locus(),
                            "variable '%s' in reduction clause is not valid. Skipping\n",
                            var_tree.prettyprint().c_str());
                    continue;
                }

                if (_discard_unused_data_sharings
                        && !symbols_in_construct.contains(var_sym))
                {
                    warn_printf_at(construct.get_locus(),
                            "skipping reduction variable '%s' "
                            "since it does not appear in the construct\n",
                            var_sym.get_qualified_name().c_str());
                    continue;
                }

                if (var_type.is_any_reference())
                    var_type = var_type.references_to();
                var_type = var_type.get_unqualified_type();

                if ((IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                        && !_allow_array_reductions)
                {
                    if (var_type.is_array())
                    {
                        error_printf_at(construct.get_locus(),
                                "reduced variable '%s' cannot have array type. Skipping\n",
                                var_tree.prettyprint().c_str());
                        continue;
                    }
                }

                std::string reductor_name = original_reductor_name;

                if (var_sym.is_dependent_entity())
                {
                    warn_printf_at(
                            construct.get_locus(),
                            "symbol '%s' is type-dependent. Skipping\n",
                            var_sym.get_qualified_name().c_str());
                    continue;
                }

                Reduction *reduction = NULL;
                if (IS_CXX_LANGUAGE)
                {
                    if (is_cxx_operator(reductor_name))
                    {
                        reductor_name = "operator " + reductor_name;
                    }

                    // Now compute a suitable cxx-nodecl-name
                    // We can't directly parse the name because it is not registered
                    // as the syntax means
                    Source id_expression_src;
                    id_expression_src << reductor_name;
                    Nodecl::NodeclBase id_expression = id_expression_src.parse_id_expression(construct);

                    ObjectList<Reduction*> red_set = Reduction::lookup(construct.retrieve_context(),
                            id_expression,
                            var_type,
                            _allow_array_reductions);

                    if (red_set.empty())
                    {
                    }
                    else if (red_set.size() > 1)
                    {
                        error_printf_at(construct.get_locus(),
                                "ambiguous reduction '%s' for reduced variable '%s' of type '%s'\n",
                                reductor_name.c_str(),
                                var_sym.get_qualified_name().c_str(),
                                var_type.get_declaration(var_sym.get_scope(), "").c_str());
                        for (ObjectList<Reduction*>::iterator it2 = red_set.begin();
                                it2 != red_set.end();
                                it2++)
                        {
                            info_printf_at(
                                    (*it2)->get_locus(),
                                    "candidate reduction for type '%s'\n",
                                    (*it2)->get_type().get_declaration(var_sym.get_scope(), "").c_str());
                        }
                    }
                    else
                    {
                        reduction = red_set[0];
                    }
                }
                else
                {
                    // Fortran and C can use this simpler lookup
                    reduction = Reduction::simple_lookup(construct.retrieve_context(),
                            reductor_name,
                            var_type,
                            _allow_array_reductions);
                }

                if (reduction != NULL)
                {
                    const char* type_name = NULL;
                    if (IS_FORTRAN_LANGUAGE)
                    {
                        type_name = fortran_print_type_str(var_sym.get_type().no_ref().get_internal_type());
                    }
                    else
                    {
                        type_name = uniquestr(var_type.get_declaration(var_sym.get_scope(), "").c_str());
                    }

                    ReductionSymbol red_sym(var_sym, var_tree, var_type, reduction);
                    sym_list.append(red_sym);

                    info_printf_at(
                            construct.get_locus(),
                            "reduction of variable '%s' of type '%s' solved to '%s'\n",
                            var_sym.get_name().c_str(),
                            type_name,
                            reductor_name.c_str());
                    info_printf_at(reduction->get_locus(), "reduction declared here\n");
                }
                else
                {
                    const char* type_name = NULL;
                    if (IS_FORTRAN_LANGUAGE)
                    {
                        type_name = fortran_print_type_str(var_sym.get_type().no_ref().get_internal_type());
                    }
                    else
                    {
                        type_name = uniquestr(var_type.get_declaration(var_sym.get_scope(), "").c_str());
                    }

                    error_printf_at(
                            construct.get_locus(),
                            "no suitable reduction '%s' was found for reduced variable '%s' of type '%s'\n",
                            reductor_name.c_str(),
                            var_sym.get_qualified_name().c_str(),
                            type_name);
                }

                add_extra_symbols(DataReference(var_tree), data_sharing_environment, extra_symbols);
            }
        }
    }

    static const decl_context_t* decl_context_map_id(const decl_context_t* d)
    {
        return d;
    }

    static void check_omp_initializer(AST a, const decl_context_t* decl_context,
            nodecl_t* nodecl_output, bool& is_init_declarator)
    {
        // Due to some syntactic infelicities in the initializer clause we have to manually check
        // this here
        // Cases:
        //   - an expression
        //   - an AST_INIT_DECLARATOR
        //   - an ambiguity including an init-declarator and a at least one expression

        if (ASTKind(a) == AST_INIT_DECLARATOR)
        {
            is_init_declarator = true;
            AST init_declarator = a;
            AST declarator = ASTSon0(init_declarator);
            AST initializer = ASTSon1(init_declarator);

            AST declarator_id_expr = ASTSon0(declarator);
            AST id_expr = ASTSon0(declarator_id_expr);

            scope_entry_list_t* entry_list = query_id_expression(decl_context, id_expr, NULL);

            if (entry_list == NULL)
            {
                error_printf_at(ast_get_locus(id_expr),
                        "unknown '%s' in initializer clause\n",
                        prettyprint_in_buffer(id_expr));
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(a));
                return;
            }
            scope_entry_t* entry = entry_list_head(entry_list);
            entry_list_free(entry_list);
            if (strcmp(entry->symbol_name, "omp_priv") != 0
                    || entry->kind != SK_VARIABLE)
            {
                error_printf_at(ast_get_locus(id_expr),
                        "invalid '%s' in initializer clause\n",
                        get_qualified_symbol_name(entry, decl_context));
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(a));
                return;
            }

            type_t* declarator_type = entry->type_information;
            char init_check = check_initialization(initializer,
                    entry->decl_context,
                    entry,
                    get_unqualified_type(declarator_type),
                    nodecl_output,
                    /* is_auto */ 0,
                    /* is_decltype_auto*/ 0);
            if (!init_check)
            {
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(a));
                return;
            }
        }
        else if (ASTKind(a) == AST_AMBIGUITY)
        {
            int i;
            int valid = -1;
            for (i = 0; i < ast_get_num_ambiguities(a); i++)
            {
                *nodecl_output = nodecl_null();
                diagnostic_context_push_buffered();
                bool dummy_is_init_declarator = false;
                check_omp_initializer(ast_get_ambiguity(a, i), decl_context, nodecl_output, dummy_is_init_declarator);
                diagnostic_context_pop_and_discard();
                if (!nodecl_is_err_expr(*nodecl_output))
                {
                    if (valid < 0)
                    {
                        valid = i;
                    }
                    else
                    {
                        internal_error("More than one ambiguity is valid", 0);
                    }
                }
            }

            if (valid < 0)
            {
                // Pick one to fail
                valid = 0;
            }
            check_omp_initializer(ast_get_ambiguity(a, valid), decl_context, nodecl_output, is_init_declarator);
        }
        else
        {
            // Expression
            check_expression(a, decl_context, nodecl_output);
        }
    }

    static void compute_nodecl_udr_(AST tree, const decl_context_t* decl_context, nodecl_t* nodecl_output, bool is_builtin)
    {
        AST omp_dr_reduction_id = ASTSon0(tree);
        AST omp_dr_typename_list = ASTSon1(tree);
        AST omp_dr_combiner = ASTSon2(tree);
        AST omp_dr_initializer = ASTSon3(tree);

        // for each <id, type> register a reduction
        AST it;
        for_each_element(omp_dr_typename_list, it)
        {
            AST current_type = ASTSon1(it);

            type_t* reduction_type = NULL;
            nodecl_t nodecl_out_type = nodecl_null();
            // Compute the type
            if (IS_C_LANGUAGE
                    || IS_CXX_LANGUAGE)
            {
                gather_decl_spec_t gather_info;
                memset(&gather_info, 0, sizeof(gather_info));
                build_scope_decl_specifier_seq(current_type,
                        &gather_info,
                        &reduction_type, decl_context,
                        &nodecl_out_type);
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                reduction_type = fortran_gather_type_from_declaration_type_spec(current_type,
                        decl_context,
                        /* character_length_out */ NULL);
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            ERROR_CONDITION(ASTText(omp_dr_reduction_id) == NULL, "Invalid tree", 0);
            std::string reduction_name = ASTText(omp_dr_reduction_id);

            if (IS_CXX_LANGUAGE
                    && is_cxx_operator(reduction_name))
            {
                reduction_name = "operator " + reduction_name;
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                reduction_name = strtolower(reduction_name.c_str());
            }

            Reduction* new_red = NULL;
            if (!is_error_type(reduction_type))
            {
                new_red = Reduction::new_reduction(decl_context, reduction_name, reduction_type);
            }

            if (new_red == NULL)
            {
                const char* type_name = NULL;
                if (IS_C_LANGUAGE
                        || IS_CXX_LANGUAGE)
                {
                    type_name = print_type_str(reduction_type, decl_context);
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    type_name = fortran_print_type_str(reduction_type);
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }

                error_printf_at(ast_get_locus(tree),
                        "reduction '%s' cannot be declared for type '%s' in the current scope\n",
                        reduction_name.c_str(),
                        type_name);
                continue;
            }

            new_red->set_is_builtin(is_builtin);

            // Now parse the combiner expression in the new expression scope
            if (IS_C_LANGUAGE
                    || IS_CXX_LANGUAGE)
            {
                // Combiner
                nodecl_t nodecl_combiner_expr = nodecl_null();

                check_expression(omp_dr_combiner,
                        new_red->get_expressions_scope().get_decl_context(),
                        &nodecl_combiner_expr);
                if (nodecl_is_err_expr(nodecl_combiner_expr))
                    continue;

                // FIXME - Check that only omp_in and omp_out appear as variables
                new_red->set_combiner(nodecl_combiner_expr);

                if (omp_dr_initializer != NULL)
                {
                    nodecl_t nodecl_initializer_expr = nodecl_null();
                    bool is_init_declarator = false;
                    check_omp_initializer(omp_dr_initializer,
                            new_red->get_expressions_scope().get_decl_context(),
                            &nodecl_initializer_expr,
                            is_init_declarator);

                    // FIXME - Check that only omp_priv and omp_red appear as variables
                    if (nodecl_is_err_expr(nodecl_initializer_expr))
                        continue;

                    new_red->set_initializer(nodecl_initializer_expr);
                    new_red->set_is_initialization(is_init_declarator);
                }

                new_red->set_locus(ast_get_locus(tree));

                if (!Core::_silent_declare_reduction)
                {
                    info_printf_at(
                            ast_get_locus(tree),
                            "declared reduction '%s' for type '%s'\n", 
                            reduction_name.c_str(),
                            print_type_str(reduction_type, decl_context));
                }
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                // Combiner
                nodecl_t nodecl_combiner_expr = nodecl_null();

                fortran_check_expression(omp_dr_combiner,
                        new_red->get_expressions_scope().get_decl_context(),
                        &nodecl_combiner_expr);
                if (nodecl_is_err_expr(nodecl_combiner_expr))
                    continue;

                // FIXME - Check that only omp_in and omp_out appear as variables
                new_red->set_combiner(nodecl_combiner_expr);

                if (omp_dr_initializer != NULL)
                {
                    nodecl_t nodecl_initializer_expr = nodecl_null();

                    fortran_check_expression(omp_dr_initializer,
                            new_red->get_expressions_scope().get_decl_context(),
                            &nodecl_initializer_expr);
                    if (nodecl_get_kind(nodecl_initializer_expr) == NODECL_ASSIGNMENT)
                    {
                        nodecl_t lhs = nodecl_get_child(nodecl_initializer_expr, 0);
                        if (nodecl_get_kind(lhs) != NODECL_SYMBOL
                                || strcmp(nodecl_get_symbol(lhs)->symbol_name, "omp_priv") != 0)
                        {
                            error_printf_at(nodecl_get_locus(nodecl_initializer_expr),
                                    "invalid syntax in initializer clause. "
                                    "It must start with 'omp_priv ='\n");
                        }
                        nodecl_t rhs = nodecl_get_child(nodecl_initializer_expr, 1);
                        nodecl_initializer_expr = rhs;
                    }

                    new_red->set_initializer(nodecl_initializer_expr);
                }
                else
                {
                    if (is_bool_type(reduction_type))
                    {
                        new_red->set_initializer(nodecl_make_boolean_literal(
                                    reduction_type,
                                    const_value_get_zero(/* bytes */ type_get_size(reduction_type), /* signed */ 1),
                                    make_locus("", 0, 0)));
                    }
                    else if (is_integral_type(reduction_type)
                        || is_complex_type(reduction_type))
                    {
                        new_red->set_initializer(
                                nodecl_make_integer_literal(
                                    reduction_type,
                                    const_value_get_signed_int(0), make_locus("", 0, 0)));
                    }
                }

                new_red->set_locus(ast_get_locus(tree));

                if (!Core::_silent_declare_reduction)
                {
                    info_printf_at(
                            ast_get_locus(tree),
                            "declared reduction '%s' for type '%s'\n",
                            reduction_name.c_str(),
                            fortran_print_type_str(reduction_type));
                }
            }
            else
            {
                internal_error("Code unreachable", 0);
            }
        }
    }

    static void compute_nodecl_udr_builtin(AST tree, const decl_context_t* decl_context, nodecl_t* nodecl_output)
    {
        compute_nodecl_udr_(tree, decl_context, nodecl_output, /* is_builtin */ true);
    }

    static void compute_nodecl_udr_regular(AST tree, const decl_context_t* decl_context, nodecl_t* nodecl_output)
    {
        compute_nodecl_udr_(tree, decl_context, nodecl_output, /* is_builtin */ false);
    }

    typedef void (*compute_nodecl_udr_t)(AST tree, const decl_context_t* decl_context, nodecl_t* nodecl_output);

    static compute_nodecl_udr_t compute_nodecl_udr(bool is_builtin)
    {
        if (is_builtin)
            return compute_nodecl_udr_builtin;
        else
            return compute_nodecl_udr_regular;
    }

    void Core::parse_declare_reduction(ReferenceScope ref_sc, const std::string &declare_reduction, bool is_builtin)
    {
        Source declare_reduction_src;
        declare_reduction_src << declare_reduction;

        declare_reduction_src.parse_generic(
                ref_sc,
                /* ParseFlags */ Source::DEFAULT,
                "@OMP-DECLARE-REDUCTION@",
                compute_nodecl_udr(is_builtin),
                decl_context_map_id);
    }

    void Core::parse_declare_reduction(ReferenceScope ref_sc, Source declare_reduction_src, bool is_builtin)
    {
        declare_reduction_src.parse_generic(
                ref_sc,
                /* ParseFlags */ Source::DEFAULT,
                "@OMP-DECLARE-REDUCTION@",
                compute_nodecl_udr(is_builtin),
                decl_context_map_id);
    }

    void Core::declare_reduction_handler_pre(TL::PragmaCustomDirective directive)
    {
        /*
         * C/C++
         * #pragma omp declare reduction(reduction-id : typename-list : combiner-expr) [initializer-clause]
         * Fortran
         * !$omp declare reduction (reduction-id : type-list : combiner) [initializer-clause]
         */

        Source src;

        PragmaCustomLine pragma_line = directive.get_pragma_line();
        PragmaCustomParameter parameter = pragma_line.get_parameter();
        if (!parameter.is_defined())
        {
            if (IS_C_LANGUAGE
                    || IS_CXX_LANGUAGE)
            {
                error_printf_at(directive.get_locus(),
                        "invalid syntax for #pragma omp declare reduction. Skipping\n");
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                error_printf_at(directive.get_locus(),
                        "invalid syntax for !$omp declare reduction. Skipping\n");
            }
            else
            {
                internal_error("Code unreachable", 0);
            }
        }

        std::string declare_reduction_arg = parameter.get_raw_arguments();

        Source declare_reduction_src;
        declare_reduction_src << "# " << directive.get_line() << " \"" << directive.get_filename() << "\"\n";
        declare_reduction_src << pad_to_column(directive.get_column()) << declare_reduction_arg;

        PragmaCustomClause initializer = pragma_line.get_clause("initializer");
        if (initializer.is_defined())
        {
            declare_reduction_src << " : " << initializer.get_raw_arguments()[0];
        }

        parse_declare_reduction(directive.get_context_of_decl(), declare_reduction_src, /* is_builtin */ false);
    }

    void Core::declare_reduction_handler_post(TL::PragmaCustomDirective directive) { }

    Reduction::Reduction(TL::Scope sc, const std::string& name, TL::Type t)
        : _scope(sc), _name(name), _type(t), _locus(NULL), _is_initialization(false), _is_builtin(false)
    {
        // Create a new expression scope
        _expr_scope = new_block_context(sc.get_decl_context());

        // Create a fake function so the block scope behaves like others
        scope_entry_t* omp_udr_function = ::new_symbol(
                sc.get_decl_context(), 
                sc.get_decl_context()->current_scope,
                UNIQUESTR_LITERAL(".omp_udr_function"));
        omp_udr_function->kind = SK_FUNCTION;
        omp_udr_function->related_decl_context = sc.get_decl_context();

        _expr_scope.get_decl_context()->current_scope->related_entry = omp_udr_function;

        // Sign in omp_{in,out,priv,orig}
        typedef std::pair<std::string, TL::Symbol Reduction::*> pair_t;
        std::vector<pair_t> omp_special;
        omp_special.push_back(pair_t("omp_in", &Reduction::_omp_in));
        omp_special.push_back(pair_t("omp_out", &Reduction::_omp_out));
        omp_special.push_back(pair_t("omp_orig", &Reduction::_omp_orig));
        omp_special.push_back(pair_t("omp_priv", &Reduction::_omp_priv));

        for (std::vector<pair_t>::iterator it = omp_special.begin();
                it != omp_special.end();
                it++)
        {
            scope_entry_t* omp_sym = ::new_symbol(
                    _expr_scope.get_decl_context(), 
                    _expr_scope.get_decl_context()->current_scope,
                    uniquestr(it->first.c_str()));

            omp_sym->kind = SK_VARIABLE;
            omp_sym->do_not_print = 1;
            omp_sym->type_information = t.get_internal_type();

            TL::Symbol Reduction::* p = it->second;
            (this->*p) = omp_sym;
        }
    }

    static std::string get_internal_name_for_reduction(std::string name, TL::Type t)
    {
        std::stringstream ss;
        ss << ".udr_" << name << "_" << t.get_internal_type();

        return ss.str();
    }

    static TL::Type get_canonical_type_for_reduction(TL::Type t)
    {
        return t.advance_over_typedefs().get_unqualified_type();
    }

    Reduction* Reduction::new_reduction(TL::Scope sc, const std::string& name, TL::Type t)
    {
        Reduction* new_red = NULL;

        // Come up with a canonical type
        t = get_canonical_type_for_reduction(t);
        std::string internal_name = get_internal_name_for_reduction(name, t);

        const decl_context_t* decl_context = sc.get_decl_context();
        scope_entry_list_t* entry_list = query_in_scope_str(decl_context, uniquestr(internal_name.c_str()), NULL);

        if (entry_list == NULL)
        {
            new_red = new Reduction(sc, name, t);

            scope_entry_t* new_red_sym = new_symbol(decl_context, decl_context->current_scope, uniquestr(internal_name.c_str()));
            new_red_sym->kind = SK_OTHER;

            OpenMP::Core::reduction_map_info[new_red_sym] = new_red;

            new_red->set_symbol(new_red_sym);
        }
        else
        {
            entry_list_free(entry_list);
        }

        DEBUG_CODE()
        {
            std::cerr << "OMP-REDUCTION: New registered UDR '" << internal_name << "'" << std::endl;
        }

        return new_red;
    }

    static bool some_reduction_is_in_class_scope(scope_entry_list_t* entry_list)
    {
        if (entry_list != NULL)
        {
            scope_entry_list_iterator_t* it;
            for (it = entry_list_iterator_begin(entry_list);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* current = entry_list_iterator_current(it);
                if (current->decl_context->current_scope->kind == CLASS_SCOPE)
                    return true;
            }
        }
        return false;
    }

    ObjectList<Reduction*> Reduction::lookup(TL::Scope sc,
            Nodecl::NodeclBase id_expression_orig,
            TL::Type t,
            bool disable_koenig,
            bool allow_array_types)
    {
        ERROR_CONDITION(!IS_CXX_LANGUAGE, "This function is only for C++", 0);

        if (allow_array_types
                && t.is_array())
        {
            while (t.is_array())
                t = t.array_element();
        }

        t = get_canonical_type_for_reduction(t);
        Nodecl::NodeclBase id_expression = id_expression_orig.shallow_copy();

        if (id_expression.is<Nodecl::CxxDepGlobalNameNested>()
                || id_expression.is<Nodecl::CxxDepNameNested>())
        {
            Nodecl::List dep_names = id_expression
                .as<Nodecl::CxxDepNameNested>()
                .get_items()
                .as<Nodecl::List>();

            Nodecl::NodeclBase last_component = dep_names.back();
            if (last_component.is<Nodecl::CxxDepNameSimple>())
            {
            // Fix the last component
                last_component.set_text(
                        get_internal_name_for_reduction(last_component.get_text(), t)
                        );
            }
            else
            {
                internal_error("Invalid last component of id-expression", 0);
            }
        }
        else if (id_expression.is<Nodecl::CxxDepNameSimple>())
        {
            // Fix the name
            id_expression.set_text(get_internal_name_for_reduction(id_expression.get_text(), t));
        }
        else
        {
            internal_error("Invalid id-expression", 0);
        }

        ObjectList<Reduction*> result;

        const decl_context_t* decl_context = sc.get_decl_context();

        scope_entry_list_t* entry_list = NULL;

        if (id_expression.is<Nodecl::CxxDepNameSimple>()
                && !disable_koenig)
        {
            // First do normal lookup
            entry_list = query_nodecl_name(decl_context, id_expression.get_internal_nodecl(), NULL);

            // If normal lookup did not find a member, attempt a koenig
            if (entry_list == NULL
                    || !some_reduction_is_in_class_scope(entry_list))
            {
                if (entry_list != NULL)
                    entry_list_free(entry_list);

                type_t* argument_type_list[] = { t.get_internal_type() };
                entry_list = koenig_lookup(
                        1, argument_type_list,
                        decl_context,
                        id_expression.get_internal_nodecl(),
                        id_expression.get_locus());
            }
        }
        else
        {
            entry_list = query_nodecl_name(decl_context, id_expression.get_internal_nodecl(), NULL);
        }

        if (entry_list != NULL)
        {
            scope_entry_t* red_sym = entry_list_head(entry_list);
            entry_list_free(entry_list);

            Reduction* red = Reduction::get_reduction_info_from_symbol(red_sym);
            if (red != NULL)
            {
                result.insert(red);
            }
        }

        if (IS_CXX_LANGUAGE
                && result.empty()
                && t.is_class())
        {
            ObjectList<TL::Symbol> bases = t.get_bases_class_symbol_list();
            for (TL::ObjectList<TL::Symbol>::iterator it = bases.begin();
                    it != bases.end();
                    it++)
            {
                TL::ObjectList<Reduction*> base_results = Reduction::lookup(sc,
                        id_expression_orig,
                        it->get_user_defined_type(),
                        /* disable koenig = */ true,
                        allow_array_types);
                result.insert(base_results);
            }
        }

        return result;
    }

    ObjectList<Reduction*> Reduction::lookup(TL::Scope sc,
            Nodecl::NodeclBase id_expression_orig,
            TL::Type t,
            bool allow_array_types)
    {
        if (IS_FORTRAN_LANGUAGE
                && fortran_is_array_type(t.get_internal_type()))
        {
            t = fortran_get_rank0_type(t.get_internal_type());
        }
        return lookup(sc, id_expression_orig, t, /* disable_koenig */ false, allow_array_types);
    }


    Reduction* Reduction::simple_lookup(TL::Scope sc,
            const std::string& name,
            TL::Type t,
            bool allow_array_types)
    {
        if (IS_FORTRAN_LANGUAGE)
        {
            if (fortran_is_array_type(t.get_internal_type()))
                t = fortran_get_rank0_type(t.get_internal_type());
        }
        else if (IS_C_LANGUAGE)
        {
            if (allow_array_types
                    && t.is_array())
            {
                while (t.is_array())
                    t = t.array_element();
            }
        }
        else
        {
            fatal_error("Code unreachable");
        }

        t = get_canonical_type_for_reduction(t);
        std::string internal_name = get_internal_name_for_reduction(name, t);

        const decl_context_t* decl_context = sc.get_decl_context();

        scope_entry_list_t* entry_list = query_name_str(decl_context, uniquestr(internal_name.c_str()), NULL);

        if (entry_list == NULL)
        {
            return NULL;
        }

        scope_entry_t* red_sym = entry_list_head(entry_list);
        entry_list_free(entry_list);

        Reduction* red = Reduction::get_reduction_info_from_symbol(red_sym);
        return red;
    }

    Reduction* Reduction::get_reduction_info_from_symbol(TL::Symbol sym)
    {
        scope_entry_t* red_sym = sym.get_internal_symbol();

        Reduction *red = OpenMP::Core::reduction_map_info[red_sym];
        return red;
    }

} }
