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
#include "tl-langconstruct.hpp"
#include "tl-source.hpp"
#include "tl-omp-udr.hpp"
#include "tl-omp-udr_2.hpp"

#include <algorithm>

namespace TL
{
    namespace OpenMP
    {
        bool Core::_already_registered(false);

        Core::Core()
            : PragmaCustomCompilerPhase("omp"), _new_udr_str(""), _new_udr(true), _udr_counter(0)
        {
            set_phase_name("OpenMP Core Analysis");
            set_phase_description("This phase is required for any other phase implementing OpenMP. "
                    "It performs the common analysis part required by OpenMP");
            register_omp_constructs();
            register_parameter("new_udr", "Alternative implementation for UDRs",
                    _new_udr_str, "1").connect(functor(&Core::parse_new_udr,*this));
        }


        void Core::parse_new_udr(const std::string& str)
        {
            parse_boolean_option("new_udr", str, _new_udr, "Assuming true.");
        }


        void Core::pre_run(TL::DTO& dto)
        {
            PragmaCustomCompilerPhase::pre_run(dto);

            if (!dto.get_keys().contains("openmp_info"))
            {
                DataSharingEnvironment* root_data_sharing = new DataSharingEnvironment(NULL);
                _openmp_info = RefPtr<OpenMP::Info>(new OpenMP::Info(root_data_sharing));
                dto.set_object("openmp_info", _openmp_info);
            }
            else
            {
                _openmp_info = RefPtr<OpenMP::Info>::cast_static(dto["openmp_info"]);
            }

            if (!dto.get_keys().contains("openmp_task_info"))
            {
                _function_task_set = RefPtr<OpenMP::FunctionTaskSet>(new OpenMP::FunctionTaskSet());
                dto.set_object("openmp_task_info", _function_task_set);
            }
            else
            {
                _function_task_set = RefPtr<FunctionTaskSet>::cast_static(dto["openmp_task_info"]);
            }

            if (!dto.get_keys().contains("openmp_core_should_run"))
            {
                RefPtr<TL::Bool> should_run(new TL::Bool(true));
                dto.set_object("openmp_core_should_run", should_run);
            }
        }

        void Core::run(TL::DTO& dto)
        {
#ifdef FORTRAN_SUPPORT
            FORTRAN_LANGUAGE()
            {
                // Not yet implemented
                return;
            }
#endif

            // "openmp_info" should exist
            if (!dto.get_keys().contains("openmp_info"))
            {
                std::cerr << "OpenMP Info was not found in the pipeline" << std::endl;
                set_phase_status(PHASE_STATUS_ERROR);
                return;
            }

            if (dto.get_keys().contains("openmp_core_should_run"))
            {
                RefPtr<TL::Bool> should_run = RefPtr<TL::Bool>::cast_dynamic(dto["openmp_core_should_run"]);
                if (!(*should_run))
                    return;

                // Make this phase a one shot by default
                *should_run = false;
            }

			if (dto.get_keys().contains("show_warnings"))
			{
				dto.set_value("show_warnings", RefPtr<Integer>(new Integer(1)));
			}

            // Reset any data computed so far
            _openmp_info->reset();

            AST_t translation_unit = dto["translation_unit"];
            ScopeLink scope_link = dto["scope_link"];

            Scope global_scope = scope_link.get_scope(translation_unit);

            if (_new_udr) 
            {
                initialize_builtin_udr_reductions_2(translation_unit, scope_link);
            }
            else 
            {
                initialize_builtin_udr_reductions(global_scope);
            }

            PragmaCustomCompilerPhase::run(dto);
        }

        void Core::register_omp_constructs()
        {
#define OMP_CONSTRUCT(_directive, _name) \
                { \
                    if (!_already_registered) \
                    { \
                      register_construct(_directive); \
                    } \
                    on_directive_pre[_directive].connect(functor(&Core::_name##_handler_pre, *this)); \
                    on_directive_post[_directive].connect(functor(&Core::_name##_handler_post, *this)); \
                }
#define OMP_DIRECTIVE(_directive, _name) \
                { \
                    if (!_already_registered) \
                    { \
                      register_directive(_directive); \
                    } \
                    on_directive_pre[_directive].connect(functor(&Core::_name##_handler_pre, *this)); \
                    on_directive_post[_directive].connect(functor(&Core::_name##_handler_post, *this)); \
                }
#define OMP_CONSTRUCT_NOEND(_directive, _name) \
                { \
                    if (!_already_registered) \
                    { \
                      register_construct(_directive, true); \
                    } \
                    on_directive_pre[_directive].connect(functor(&Core::_name##_handler_pre, *this)); \
                    on_directive_post[_directive].connect(functor(&Core::_name##_handler_post, *this)); \
                }
#include "tl-omp-constructs.def"
#undef OMP_DIRECTIVE
#undef OMP_CONSTRUCT
#undef OMP_CONSTRUCT_NOEND
            _already_registered = true;
        }

        void Core::get_clause_symbols(PragmaCustomClause clause, 
                ObjectList<DataReference>& data_ref_list,
                bool allow_extended_references)
        {
            ObjectList<Expression> expr_list;
            if (clause.is_defined())
            {
                expr_list = clause.get_expression_list();

                for (ObjectList<Expression>::iterator it = expr_list.begin();
                        it != expr_list.end(); 
                        it++)
                {
                    DataReference data_ref(*it);

                    std::string warning;
                    if (!data_ref.is_valid(warning)
                            || (!allow_extended_references && !it->is_id_expression()))
                    {
                        std::cerr << warning;
                        std::cerr << data_ref.get_ast().get_locus() << ": warning: '" << data_ref 
                            << "' is not a valid name for data sharing" << std::endl;
                    }
                    else
                    {
                        Symbol base_sym = data_ref.get_base_symbol();

                        if (base_sym.is_member()
                                && !base_sym.is_static())
                        {
                            std::cerr << data_ref.get_ast().get_locus() << ": ignoring: '" << data_ref 
                                << "' since nonstatic data members cannot appear un data-sharing clauses" << std::endl;
                        }

                        data_ref_list.append(data_ref);
                    }
                }
            }
        }

        void Core::get_reduction_symbols(
                PragmaCustomConstruct construct,
                PragmaCustomClause clause, 
                ObjectList<ReductionSymbol>& sym_list)
        {
            DEBUG_CODE()
            {
                std::cerr << "=== Reduction clause [" << construct.get_ast().get_locus() << "]===" << std::endl;
            }

            if (!clause.is_defined())
                return;

            // FIXME - Change the name of this function!
            ObjectList<ObjectList<std::string> > clause_arguments = clause.get_arguments_unflattened();

            for (ObjectList<ObjectList<std::string> >::iterator list_it = clause_arguments.begin();
                    list_it != clause_arguments.end();
                    list_it++)
            {
                ObjectList<std::string>& arguments(*list_it);

                // The first argument is special, we have to look for a ':' that is not followed by any other ':'
                // #pragma omp parallel for reduction(A::F : A::d)

                std::string first_arg = arguments[0];

                // Remove blanks
                first_arg.erase(std::remove(first_arg.begin(), first_arg.end(), ' '), first_arg.end());

                std::string::iterator split_colon = first_arg.end();
                for (std::string::iterator it = first_arg.begin();
                        it != first_arg.end();
                        it++)
                {
                    if ((*it) == ':'
                            && (it + 1) != first_arg.end())
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

                if (split_colon == first_arg.end())
                {
                    std::cerr << clause.get_ast().get_locus() << ": warning: 'reduction' clause does not have a valid operator" << std::endl;
                    std::cerr << clause.get_ast().get_locus() << ": warning: skipping the whole clause" << std::endl;
                    return;
                }

                std::string original_reductor_name;
                std::copy(first_arg.begin(), split_colon, std::back_inserter(original_reductor_name));

                std::string remainder_arg;
                std::copy(split_colon + 1, first_arg.end(), std::back_inserter(remainder_arg));

                // Put back the arguments after tokenization
                arguments = ExpressionTokenizer().tokenize(remainder_arg);

                // Rename 'arguments' to variables, since 'arguments' would be
                // too vague
                ObjectList<std::string> &variables(arguments);
                for (ObjectList<std::string>::iterator it = variables.begin();
                        it != variables.end();
                        it++)
                {
                    std::string &variable(*it);
                    Source src;
                    src
                        << "#line " << construct.get_ast().get_line() << " \"" << construct.get_ast().get_file() << "\"\n"
                        << variable
                        ;

                    AST_t var_tree = src.parse_id_expression(clause.get_ast(), clause.get_scope_link());
                    IdExpression var_id_expr(var_tree, clause.get_scope_link());
                    Symbol var_sym = var_id_expr.get_symbol();

                    if (!var_sym.is_valid())
                    {
                        running_error("%s: error: variable '%s' in reduction clause has not been found in the scope\n",
                                construct.get_ast().get_locus().c_str(),
                                var_tree.prettyprint().c_str());
                    }

                    Type var_type = var_sym.get_type();

                    std::string reductor_name = original_reductor_name;
                    // Ammend as needed the reductor name for this variable
                    CXX_LANGUAGE()
                    {
                        if (reductor_name[0] == '.')
                        {
                            if (!var_type.is_named_class()
                                    && !var_type.is_dependent())
                            {
                                std::cerr << construct.get_ast().get_locus() << ": warning: reductor '" << reductor_name 
                                    << "' is no valid for non class-type variable '" << var_id_expr.prettyprint() << "'"
                                    << ", skipping"
                                    << std::endl;
                                continue;
                            }
                            else
                            {
                                reductor_name = var_type.get_declaration(construct.get_scope(), "") + "::" + reductor_name.substr(1);
                            }
                        }
                    }

                    if (var_sym.is_dependent_entity())
                    {
                        std::cerr << construct.get_ast().get_locus() << ": warning: symbol "
                            << "'" << var_tree.prettyprint() << "' is dependent, skipping it" << std::endl;
                    }
                    else
                    {
		                AST_t reductor_name_tree;
		                IdExpression reductor_id_expr(NULL, ScopeLink());

                        if (_new_udr)
                        {
                            bool found = false;
                            UDRInfoItem2 udr2;
                            udr2.set_type(var_type);

				            if (var_type.is_class())
				            {
				                reductor_name_tree
				                        = udr2.parse_omp_udr_operator_name(reductor_name, construct.get_ast(), construct.get_scope_link());
				                reductor_id_expr = IdExpression(reductor_name_tree, clause.get_scope_link());
				            }
                            else if (!udr_is_builtin_operator(reductor_name))
                            {
                                reductor_name_tree = 
                                        Source(reductor_name).parse_id_expression_wo_check(construct.get_scope(), 
                                        construct.get_scope_link());
                            }

                            if (!udr_is_builtin_operator(reductor_name) && reductor_name_tree.internal_ast_type_() == AST_QUALIFIED_ID)
                            {
                                udr2.set_name(reductor_name_tree.children()[2].prettyprint());
                                std::string symbol_name = udr2.get_symbol_name(var_type);

                                // Change the third son 'name' -> '.udr_name_0xXXXXXXX'
                                reductor_name_tree.children()[2].replace_text(symbol_name);
                                IdExpression reductor_expression(reductor_name_tree, construct.get_scope_link());
                                Symbol reductor_sym = reductor_expression.get_symbol();
                                found = true;
                                
                                // Fill UDR info
                                RefPtr<UDRInfoItem2> obj = 
                                        RefPtr<UDRInfoItem2>::cast_dynamic(reductor_sym.get_attribute("udr_info"));
                                udr2 = (*obj);
                            }
                            else
                            {
                                CXX_LANGUAGE()
                                {
                                    if (udr_is_builtin_operator_2(reductor_name) && var_type.is_enum())
                                    {
                                        var_type = var_type.get_enum_underlying_type();
                                    }
                                }

                                udr2.set_builtin_operator(reductor_name);

		                        if (!reductor_name.compare("+")) reductor_name = "_plus_";
		                        else if (!reductor_name.compare("-")) reductor_name = "_minus_";
		                        else if (!reductor_name.compare("*")) reductor_name = "_mult_";
		                        else if (!reductor_name.compare("&")) reductor_name = "_and_";
		                        else if (!reductor_name.compare("|")) reductor_name = "_or_";
		                        else if (!reductor_name.compare("^")) reductor_name = "_exp_";
		                        else if (!reductor_name.compare("&&")) reductor_name = "_andand_";
		                        else if (!reductor_name.compare("||")) reductor_name = "_oror_";

                                udr2.set_name(reductor_name);
		                        udr2 = udr2.lookup_udr(construct.get_scope(),
		                                found,
		                                var_type,
                                        reductor_name_tree,
                                        _udr_counter);
                            }

                            if (found)
                            {
                                ReductionSymbol red_sym(var_sym, udr2);
                                sym_list.append(red_sym);
                                if (!udr2.is_builtin_operator() && construct.get_show_warnings())
                                {
                                    std::cerr << construct.get_ast().get_locus() 
                                        << ": note: reduction of variable '" << var_sym.get_name() << "' solved to '" 
                                        << reductor_name << "'"
                                        << std::endl;
                                }
                            }
                            else
                            {
                                // Make this a hard error, otherwise lots of false positives will slip in
                                running_error("%s: error: no suitable reductor operator '%s' was found for reduced variable '%s' of type '%s'",
                                        construct.get_ast().get_locus().c_str(),
                                        reductor_name.c_str(),
                                        var_tree.prettyprint().c_str(),
                                        var_sym.get_type().get_declaration(var_sym.get_scope(), "").c_str());
                            }
                        }
                        else
                        {
				            if (!udr_is_builtin_operator(reductor_name))
				            {
				                reductor_name_tree
				                    = Source(reductor_name).parse_id_expression(construct.get_ast(), construct.get_scope_link());
				                reductor_id_expr = IdExpression(reductor_name_tree, clause.get_scope_link());
				            }

				            // Adjust pointers to arrays
				            if (!var_sym.is_parameter()
				                    && var_type.is_pointer()
				                    && var_type.points_to().is_array())
				            {
				                // Ignore the additional pointer
				                var_type = var_type.points_to();
				            }

				            // Lower array types
				            int num_dimensions = var_type.get_num_dimensions();

                            UDRInfoItem udr;
                            if (udr_is_builtin_operator(reductor_name))
                            {
                                udr.set_builtin_operator(reductor_name);
                            }
                            else
                            {
                                udr.set_operator(reductor_id_expr);
                            }
                            udr.set_reduction_type(var_type);

                            if (num_dimensions != 0)
                            {
                                udr.set_is_array_reduction(true);
                                udr.set_num_dimensions(num_dimensions);
                            }

                            ObjectList<Symbol> all_viables;

                            bool found = false;
                            udr = udr.lookup_udr(construct.get_scope(), 
                                    construct.get_scope_link(),
                                    found,
                                    all_viables, 
                                    construct.get_ast().get_file(),
                                    construct.get_ast().get_line());

                            if (found)
                            {
                                ReductionSymbol red_sym(var_sym, udr);
                                sym_list.append(red_sym);

                                if (!udr.is_builtin_operator())
                                {
                                    Symbol op_sym = udr.get_operator_symbols()[0];
                                    Type op_type = op_sym.get_type();
                                    std::cerr << construct.get_ast().get_locus() 
                                        << ": note: reduction of variable '" << var_sym.get_name() << "' solved to '" 
                                        << op_type.get_declaration(construct.get_scope(),
                                                op_sym.get_qualified_name(construct.get_scope())) << "'" 
                                        << std::endl;
                                }
                            }
                            else
                            {
                                // Make this a hard error, otherwise lots of false positives will slip in
                                running_error("%s: error: no suitable reductor operator '%s' was found for reduced variable '%s' of type '%s'",
                                        construct.get_ast().get_locus().c_str(),
                                        reductor_name.c_str(),
                                        var_tree.prettyprint().c_str(),
                                        var_sym.get_type().get_declaration(var_sym.get_scope(), "").c_str());
                            }
                        }
                    }
                }
            }
        }

        struct DataSharingEnvironmentSetter
        {
            private:
                AST_t _ref_tree;
                DataSharingEnvironment& _data_sharing;
                DataSharingAttribute _data_attrib;
            public:
                DataSharingEnvironmentSetter(
                        AST_t ref_tree,
                        DataSharingEnvironment& data_sharing, 
                        DataSharingAttribute data_attrib)
                    : _ref_tree(ref_tree),
                    _data_sharing(data_sharing),
                    _data_attrib(data_attrib) { }

                void operator()(DataReference data_ref)
                {
                    Symbol sym = data_ref.get_base_symbol();

                    if ((_data_sharing.get_data_sharing(sym, /* check_enclosing */ false)
                            == DS_SHARED)
                            && _data_attrib & DS_PRIVATE )
                    {
                        std::cerr << _ref_tree.get_locus() << ": warning: data sharing of '" 
                            << data_ref.prettyprint() 
                            << "' was shared but now it is being overriden as private" 
                            << std::endl;
                    }

                    if (data_ref.is_id_expression())
                    {
                        _data_sharing.set_data_sharing(sym, _data_attrib);
                    }
                    else
                    {
                        _data_sharing.set_data_sharing(sym, _data_attrib, data_ref);
                    }
                }
        };

        struct DataSharingEnvironmentSetterReduction
        {
            private:
                DataSharingEnvironment& _data_sharing;
                DataSharingAttribute _data_attrib;
                std::string _reductor_name;
            public:
                DataSharingEnvironmentSetterReduction(DataSharingEnvironment& data_sharing, DataSharingAttribute data_attrib)
                    : _data_sharing(data_sharing),
                    _data_attrib(data_attrib) { }

                void operator()(ReductionSymbol red_sym)
                {
                    _data_sharing.set_reduction(red_sym);
                }
        };

        void Core::get_data_explicit_attributes(PragmaCustomConstruct construct, 
                DataSharingEnvironment& data_sharing)
        {
            ObjectList<DataReference> shared_references;
            get_clause_symbols(construct.get_clause("shared"), shared_references);
            std::for_each(shared_references.begin(), shared_references.end(), 
                    DataSharingEnvironmentSetter(construct.get_ast(), data_sharing, DS_SHARED));

            ObjectList<DataReference> private_references;
            get_clause_symbols(construct.get_clause("private"), private_references);
            std::for_each(private_references.begin(), private_references.end(), 
                    DataSharingEnvironmentSetter(construct.get_ast(), data_sharing, DS_PRIVATE));

            ObjectList<DataReference> firstprivate_references;
            get_clause_symbols(construct.get_clause("firstprivate"), 
                    firstprivate_references);
            std::for_each(firstprivate_references.begin(), firstprivate_references.end(), 
                    DataSharingEnvironmentSetter(construct.get_ast(), data_sharing, DS_FIRSTPRIVATE));

            ObjectList<DataReference> lastprivate_references;
            get_clause_symbols(construct.get_clause("lastprivate"), lastprivate_references);
            std::for_each(lastprivate_references.begin(), lastprivate_references.end(), 
                    DataSharingEnvironmentSetter(construct.get_ast(), data_sharing, DS_LASTPRIVATE));

            ObjectList<OpenMP::ReductionSymbol> reduction_references;
            get_reduction_symbols(construct, construct.get_clause("reduction"), reduction_references);
            std::for_each(reduction_references.begin(), reduction_references.end(), 
                    DataSharingEnvironmentSetterReduction(data_sharing, DS_REDUCTION));

            ObjectList<DataReference> copyin_references;
            get_clause_symbols(construct.get_clause("copyin"), copyin_references);
            std::for_each(copyin_references.begin(), copyin_references.end(), 
                    DataSharingEnvironmentSetter(construct.get_ast(), data_sharing, DS_COPYIN));

            ObjectList<DataReference> copyprivate_references;
            get_clause_symbols(construct.get_clause("copyprivate"), copyprivate_references);
            std::for_each(copyprivate_references.begin(), copyprivate_references.end(), 
                    DataSharingEnvironmentSetter(construct.get_ast(), data_sharing, DS_COPYPRIVATE));

            // Internal clauses created by fun-tasks phase
            ObjectList<DataReference> fp_input_references;
            get_clause_symbols(construct.get_clause("__fp_input"), fp_input_references, 
                    /* Allow extended references */ true);
            std::for_each(fp_input_references.begin(), fp_input_references.end(), 
                    DataSharingEnvironmentSetter(construct.get_ast(), data_sharing, DS_FIRSTPRIVATE));

            ObjectList<DataReference> fp_output_references;
            get_clause_symbols(construct.get_clause("__fp_output"), fp_output_references, 
                    /* Allow extended references */ true);
            std::for_each(fp_output_references.begin(), fp_output_references.end(), 
                    DataSharingEnvironmentSetter(construct.get_ast(), data_sharing, DS_FIRSTPRIVATE));

            ObjectList<DataReference> fp_inout_references;
            get_clause_symbols(construct.get_clause("__fp_inout"), fp_inout_references, 
                    /* Allow extended references */ true);
            std::for_each(fp_inout_references.begin(), fp_inout_references.end(), 
                    DataSharingEnvironmentSetter(construct.get_ast(), data_sharing, DS_FIRSTPRIVATE));

            ObjectList<DataReference> fp_reduction_references;
            get_clause_symbols(construct.get_clause("__fp_reduction"), fp_reduction_references, 
                    /* Allow extended references */ true);
            std::for_each(fp_reduction_references.begin(), fp_reduction_references.end(), 
                    DataSharingEnvironmentSetter(construct.get_ast(), data_sharing, DS_FIRSTPRIVATE));
        }

        DataSharingAttribute Core::get_default_data_sharing(PragmaCustomConstruct construct,
                DataSharingAttribute fallback_data_sharing)
        {
            PragmaCustomClause default_clause = construct.get_clause("default");

            if (!default_clause.is_defined())
            {
                return fallback_data_sharing;
            }
            else
            {
                ObjectList<std::string> args = default_clause.get_arguments(ExpressionTokenizer());

                struct pairs_t
                {
                    const char* name;
                    DataSharingAttribute data_attr;
                } pairs[] = 
                {
                    { "none", (DataSharingAttribute)DS_NONE },
                    { "shared", (DataSharingAttribute)DS_SHARED },
                    { "firstprivate", (DataSharingAttribute)DS_FIRSTPRIVATE },
                    { NULL, (DataSharingAttribute)DS_UNDEFINED },
                };

                for (unsigned int i = 0; pairs[i].name != NULL; i++)
                {
                    if (std::string(pairs[i].name) == args[0])
                    {
                        return pairs[i].data_attr;
                    }
                }

                std::cerr << default_clause.get_ast().get_locus() 
                    << ": warning: data sharing '" << args[0] << "' is not valid in 'default' clause" << std::endl;
                std::cerr << default_clause.get_ast().get_locus() 
                    << ": warning: assuming 'shared'" << std::endl;

                return DS_SHARED;
            }
        }

        void Core::get_data_implicit_attributes(PragmaCustomConstruct construct, 
                DataSharingAttribute default_data_attr, 
                DataSharingEnvironment& data_sharing)
        {
            Statement statement = construct.get_statement();

            ObjectList<IdExpression> id_expr_list = statement.non_local_symbol_occurrences();
            ObjectList<Symbol> already_nagged;

            for (ObjectList<IdExpression>::iterator it = id_expr_list.begin();
                    it != id_expr_list.end();
                    it++)
            {
                Symbol sym = it->get_symbol();

                if (!sym.is_valid()
                        || !sym.is_variable())
                    continue;

                // We should ignore these ones lest they slipped in because
                // being named in an unqualified manner
                if (sym.is_member()
                        && !sym.is_static())
                    continue;

                DataSharingAttribute data_attr = data_sharing.get_data_sharing(sym);

                // Do nothing with threadprivates
                if ((data_attr & DS_THREADPRIVATE) == DS_THREADPRIVATE)
                    continue;

                data_attr = data_sharing.get_data_sharing(sym, /* check_enclosing */ false);

                if (data_attr == DS_UNDEFINED)
                {
                    if (default_data_attr == DS_NONE)
                    {
                        if (!already_nagged.contains(sym))
                        {
                            std::cerr << it->get_ast().get_locus() 
                                << ": warning: symbol '" << sym.get_qualified_name(sym.get_scope()) 
                                << "' does not have data sharing and 'default(none)' was specified. Assuming shared "
                                << std::endl;

                            // Maybe we do not want to assume always shared?
                            data_sharing.set_data_sharing(sym, DS_SHARED);

                            already_nagged.append(sym);
                        }
                    }
                    else
                    {
                        // Set the symbol as having default data sharing
                        data_sharing.set_data_sharing(sym, (DataSharingAttribute)(default_data_attr | DS_IMPLICIT));
                    }
                }
            }
        }

        void Core::common_parallel_handler(PragmaCustomConstruct construct, DataSharingEnvironment& data_sharing)
        {
            data_sharing.set_is_parallel(true);

            get_target_info(construct, data_sharing);

            get_data_explicit_attributes(construct, data_sharing);

            DataSharingAttribute default_data_attr = get_default_data_sharing(construct, /* fallback */ DS_SHARED);

            get_data_implicit_attributes(construct, default_data_attr, data_sharing);
        }

        void Core::common_sections_handler(PragmaCustomConstruct construct, const std::string& pragma_name)
        {
            Statement stmt = construct.get_statement();
            if (!stmt.is_compound_statement())
            {
                running_error("%s: error: '#pragma omp %s' must be followed by a compound statement\n",
                        construct.get_ast().get_locus().c_str(),
                        pragma_name.c_str());
            }

            ObjectList<Statement> inner_stmt = stmt.get_inner_statements();

            if (inner_stmt.size() > 1)
            {
                if (!is_pragma_custom_construct("omp", "section", 
                            inner_stmt[0].get_ast(), construct.get_scope_link())
                        && !is_pragma_custom_construct("omp", "section", 
                            inner_stmt[1].get_ast(), construct.get_scope_link()))
                {
                    running_error("%s: error: only the first structured-block can have '#pragma omp section' ommitted\n",
                            inner_stmt[1].get_ast().get_locus().c_str());
                }
            }
        }

        void Core::fix_first_section(PragmaCustomConstruct construct)
        {
            Statement stmt = construct.get_statement();
            ERROR_CONDITION(!stmt.is_compound_statement(), "It must be a compound statement", 0);

            ObjectList<Statement> inner_stmt = stmt.get_inner_statements();

            if (!inner_stmt.empty()
                    && !is_pragma_custom_construct("omp", "section", 
                        inner_stmt[0].get_ast(), construct.get_scope_link()))
            {
                Source add_section_src;
                add_section_src
                    << "#pragma omp section\n"
                    <<  inner_stmt[0].prettyprint()
                    ;

                AST_t add_section_tree = add_section_src.parse_statement(inner_stmt[0].get_ast(), construct.get_scope_link());
                inner_stmt[0].get_ast().replace(add_section_tree);
            }
        }

        void Core::common_for_handler(PragmaCustomConstruct construct, DataSharingEnvironment& data_sharing)
        {
            Statement stmt = construct.get_statement();

            if (!ForStatement::predicate(stmt.get_ast()))
            {
                running_error("%s: error: a for-statement is required for '#pragma omp for' and '#pragma omp parallel for'",
                        stmt.get_ast().get_locus().c_str());
            }

            ForStatement for_statement(stmt.get_ast(), stmt.get_scope_link());

            if (for_statement.is_regular_loop())
            {
                IdExpression id_expr = for_statement.get_induction_variable();
                Symbol sym = id_expr.get_symbol();
                data_sharing.set_data_sharing(sym, DS_PRIVATE);
            }
            else
            {
                running_error("%s: error: for-statement in '#pragma omp for' and '#pragma omp parallel for' is not of canonical form",
                        stmt.get_ast().get_locus().c_str());
            }
        }

        void Core::common_workshare_handler(PragmaCustomConstruct construct, DataSharingEnvironment& data_sharing)
        {
            get_target_info(construct, data_sharing);

            get_data_explicit_attributes(construct, data_sharing);

            DataSharingAttribute default_data_attr = get_default_data_sharing(construct, /* fallback */ DS_SHARED);

            get_data_implicit_attributes(construct, default_data_attr, data_sharing);
        }

        // Data sharing computation for tasks.
        //
        // Tasks have slightly different requirements to other OpenMP constructs so their code
        // can't be merged easily
        void Core::get_data_implicit_attributes_task(PragmaCustomConstruct construct,
                DataSharingEnvironment& data_sharing,
                DataSharingAttribute default_data_attr)
        {
            Statement statement = construct.get_statement();

            ObjectList<IdExpression> id_expr_list = statement.non_local_symbol_occurrences();
            ObjectList<Symbol> already_nagged;

            for (ObjectList<IdExpression>::iterator it = id_expr_list.begin();
                    it != id_expr_list.end();
                    it++)
            {
                Symbol sym = it->get_symbol();

                if (!sym.is_valid()
                        || !sym.is_variable())
                    continue;

                DataSharingAttribute data_attr = data_sharing.get_data_sharing(sym);

                // Do nothing with threadprivates
                if ((data_attr & DS_THREADPRIVATE) == DS_THREADPRIVATE)
                    continue;

                data_attr = data_sharing.get_data_sharing(sym, /* check_enclosing */ false);

                if (data_attr == DS_UNDEFINED)
                {
                    if (default_data_attr == DS_NONE)
                    {
                        if (!already_nagged.contains(sym))
                        {
                            std::cerr << it->get_ast().get_locus() 
                                << ": warning: symbol '" << sym.get_qualified_name(sym.get_scope()) 
                                << "' does not have data sharing and 'default(none)' was specified. Assuming firstprivate "
                                << std::endl;

                            data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_FIRSTPRIVATE | DS_IMPLICIT));
                            already_nagged.append(sym);
                        }
                    }
                    else if (default_data_attr == DS_UNDEFINED)
                    {
                        // This is a special case of task
                        bool is_shared = true;
                        DataSharingEnvironment* enclosing = data_sharing.get_enclosing();

                        // If it is a global, it will be always shared
                        if (!(sym.has_namespace_scope()
                                    || (sym.is_member() && sym.is_static())))
                        {
                            while ((enclosing != NULL) && is_shared)
                            {
                                DataSharingAttribute ds = enclosing->get_data_sharing(sym, /* check_enclosing */ false);
                                ds = (DataSharingAttribute)(ds & ~DS_IMPLICIT);
                                is_shared = (is_shared && (ds == DS_SHARED));

                                // Stop once we see the innermost parallel
                                if (enclosing->get_is_parallel())
                                    break;
                                enclosing = enclosing->get_enclosing();
                            }
                        }

                        if (is_shared)
                        {
                            data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT));
                        }
                        else
                        {
                            data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_FIRSTPRIVATE | DS_IMPLICIT));
                        }
                    }
                    else
                    {
                        // Set the symbol as having the default data sharing
                        data_sharing.set_data_sharing(sym, (DataSharingAttribute)(default_data_attr | DS_IMPLICIT));
                    }
                }
            }
        }

        // Handlers
        void Core::parallel_handler_pre(PragmaCustomConstruct construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());
            _openmp_info->push_current_data_sharing(data_sharing);
            common_parallel_handler(construct, data_sharing);
        }

        void Core::parallel_handler_post(PragmaCustomConstruct construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::parallel_for_handler_pre(PragmaCustomConstruct construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());

            if (construct.get_clause("collapse").is_defined())
            {
                // This function _modifies_ construct to reflect the new reality!
                collapse_loop_first(construct);
            }

            _openmp_info->push_current_data_sharing(data_sharing);
            common_parallel_handler(construct, data_sharing);
            common_for_handler(construct, data_sharing);
        }

        void Core::parallel_for_handler_post(PragmaCustomConstruct construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::for_handler_pre(PragmaCustomConstruct construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());

            if (construct.get_clause("collapse").is_defined())
            {
                // This will replace the tree
                collapse_loop_first(construct);
            }

            _openmp_info->push_current_data_sharing(data_sharing);
            common_workshare_handler(construct, data_sharing);
            common_for_handler(construct, data_sharing);
            get_dependences_info(construct, data_sharing);
        }

        void Core::for_handler_post(PragmaCustomConstruct construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::single_handler_pre(PragmaCustomConstruct construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());
            _openmp_info->push_current_data_sharing(data_sharing);
            common_workshare_handler(construct, data_sharing);
        }

        void Core::single_handler_post(PragmaCustomConstruct construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::parallel_sections_handler_pre(PragmaCustomConstruct construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());
            _openmp_info->push_current_data_sharing(data_sharing);
            common_parallel_handler(construct, data_sharing);

            common_sections_handler(construct, "parallel sections");
        }

        void Core::parallel_sections_handler_post(PragmaCustomConstruct construct)
        {
            fix_first_section(construct);
            _openmp_info->pop_current_data_sharing();
        }

        void Core::threadprivate_handler_pre(PragmaCustomConstruct construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_current_data_sharing();

            ObjectList<Expression> expr_list = construct.get_parameter_expressions();

            for (ObjectList<Expression>::iterator it = expr_list.begin();
                    it != expr_list.end();
                    it++)
            {
                Expression& expr(*it);
                if (!expr.is_id_expression())
                {
                    std::cerr << expr.get_ast().get_locus() << ": warning: '" << expr << "' is not an id-expression, skipping" << std::endl;
                }
                else
                {
                    IdExpression id_expr = expr.get_id_expression();
                    Symbol sym = id_expr.get_symbol();

                    if (sym.is_member()
                            && !sym.is_static())
                    {
                        std::cerr << expr.get_ast().get_locus() << ": warning: '" << expr << "' is a nonstatic-member, skipping" << std::endl;
                    }

                    data_sharing.set_data_sharing(sym, DS_THREADPRIVATE);
                }
            }
        }
        void Core::threadprivate_handler_post(PragmaCustomConstruct construct) { }

        void Core::task_handler_pre(PragmaCustomConstruct construct)
        {
			if (construct.get_declaration().is_valid())
            {
                task_function_handler_pre(construct);
            }
			else
			{
				task_inline_handler_pre(construct);
			}
        }

        void Core::task_handler_post(PragmaCustomConstruct construct)
        {
            if (!Statement::predicate(construct.get_declaration()))
            {
                // Do nothing for this case
                return;
            }

            _openmp_info->pop_current_data_sharing();
        }


        void Core::taskwait_handler_pre(PragmaCustomConstruct construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());
            _openmp_info->push_current_data_sharing(data_sharing);

            get_dependences_info_clause(construct.get_clause("on"), data_sharing, DEP_DIR_INPUT);
        }

        void Core::taskwait_handler_post(PragmaCustomConstruct construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::sections_handler_pre(PragmaCustomConstruct construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());
            _openmp_info->push_current_data_sharing(data_sharing);

            common_workshare_handler(construct, data_sharing);

            common_sections_handler(construct, "sections");
        }

        void Core::sections_handler_post(PragmaCustomConstruct construct)
        {
            fix_first_section(construct);
            _openmp_info->pop_current_data_sharing();
        }

#define EMPTY_HANDLERS(_name) \
        void Core::_name##_handler_pre(PragmaCustomConstruct ctr) { ctr.init_clause_info(); } \
        void Core::_name##_handler_post(PragmaCustomConstruct) { }

        EMPTY_HANDLERS(section)
        EMPTY_HANDLERS(barrier)
        EMPTY_HANDLERS(atomic)
        EMPTY_HANDLERS(master)
        EMPTY_HANDLERS(critical)
        EMPTY_HANDLERS(flush)
        EMPTY_HANDLERS(ordered)
#ifdef FORTRAN_SUPPORT
        EMPTY_HANDLERS(parallel_do)
        EMPTY_HANDLERS(do)
#endif

        void openmp_core_run_next_time(DTO& dto)
        {
            // Make openmp core run in the pipeline
            RefPtr<TL::Bool> openmp_core_should_run = RefPtr<TL::Bool>::cast_dynamic(dto["openmp_core_should_run"]);
            *openmp_core_should_run = true;
        }

    }
}

EXPORT_PHASE(TL::OpenMP::Core)
