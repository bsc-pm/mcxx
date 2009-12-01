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
#include "tl-langconstruct.hpp"
#include "tl-source.hpp"
#include "tl-omp-udr.hpp"

#include <algorithm>

namespace TL
{
    namespace OpenMP
    {
        Core::Core()
            : PragmaCustomCompilerPhase("omp")
        {
            set_phase_name("OpenMP Core Analysis");
            set_phase_description("This phase is required for any other phase implementing OpenMP. "
                    "It performs the common analysis part required by OpenMP");
            register_omp_constructs();
        }

        void Core::run(TL::DTO& dto)
        {
            // "openmp_info" should exist
            if (!dto.get_keys().contains("openmp_info"))
            {
                std::cerr << "OpenMP Info was not found in the pipeline" << std::endl;
                set_phase_status(PHASE_STATUS_ERROR);
                return;
            }

            AST_t translation_unit = dto["translation_unit"];
            ScopeLink scope_link = dto["scope_link"];

            Scope global_scope = scope_link.get_scope(translation_unit);

            initialize_builtin_udr_reductions(global_scope);

            PragmaCustomCompilerPhase::run(dto);
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

            if (!dto.get_keys().contains("openmp_task_info"))
            {
                _function_task_set = RefPtr<OpenMP::FunctionTaskSet>(new OpenMP::FunctionTaskSet());
                dto.set_object("openmp_task_info", _function_task_set);
            }
        }

        void Core::register_omp_constructs()
        {
#define OMP_CONSTRUCT(_directive, _name) \
            { \
                register_construct(_directive); \
                on_directive_pre[_directive].connect(functor(&Core::_name##_handler_pre, *this)); \
                on_directive_post[_directive].connect(functor(&Core::_name##_handler_post, *this)); \
            }
#define OMP_DIRECTIVE(_directive, _name) \
            { \
                register_directive(_directive); \
                on_directive_pre[_directive].connect(functor(&Core::_name##_handler_pre, *this)); \
                on_directive_post[_directive].connect(functor(&Core::_name##_handler_post, *this)); \
            }
#include "tl-omp-constructs.def"
#undef OMP_DIRECTIVE
#undef OMP_CONSTRUCT
        }

        void Core::get_clause_symbols(PragmaCustomClause clause, ObjectList<Symbol>& sym_list)
        {
            ObjectList<IdExpression> id_expr_list;
            if (clause.is_defined())
            {
                id_expr_list = clause.id_expressions();

                for (ObjectList<IdExpression>::iterator it = id_expr_list.begin();
                        it != id_expr_list.end(); 
                        it++)
                {
                    Symbol sym = it->get_symbol();
                    if (sym.is_valid())
                    {
                        sym_list.append(sym);
                    }
                    else
                    {
                        std::cerr << it->get_ast().get_locus() << ": warning: identifier '" << (*it) << "' is unknown" << std::endl;
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

                    std::string unqualified_reductor_name = reductor_name;

                    if (!udr_is_builtin_operator(reductor_name))
                    {
                        AST_t reductor_name_tree 
                            = Source(reductor_name).parse_id_expression(construct.get_ast(), construct.get_scope_link());
                        IdExpression reductor_id_expr(reductor_name_tree, clause.get_scope_link());
                        unqualified_reductor_name = reductor_id_expr.get_unqualified_part();
                    }


                    if (!var_sym.is_valid())
                    {
                        running_error("%s: error: variable '%s' in reduction clause is invalid\n",
                                construct.get_ast().get_locus().c_str(),
                                var_tree.prettyprint().c_str());
                    }
                    else if (var_sym.is_dependent_entity())
                    {
                        std::cerr << construct.get_ast().get_locus() << ": warning: symbol "
                            << "'" << var_tree.prettyprint() << "' is dependent, skipping it" << std::endl;
                    }
                    else
                    {

                        UDRInfoScope udr_info_scope(construct.get_scope());

                        UDRInfoItem udr_info_item = udr_info_scope.get_udr(
                                unqualified_reductor_name,
                                reductor_name, 
                                var_type, 
                                construct.get_scope_link(),
                                construct.get_scope(),
                                construct.get_ast().get_file(), 
                                construct.get_ast().get_line());

                        if (udr_info_item.is_valid())
                        {
                            ReductionSymbol red_sym(var_sym, udr_info_item);
                            sym_list.append(red_sym);
                        }
                        else
                        {
                            // Make this a hard error, otherwise lots of false positives will slip in
                            running_error("%s: error: no suitable reductor operator '%s' was found for reduced variable '%s'",
                                    construct.get_ast().get_locus().c_str(),
                                    reductor_name.c_str(),
                                    var_tree.prettyprint().c_str());
                        }
                    }
                }
            }
        }

        struct DataSharingEnvironmentSetter
        {
            private:
                DataSharingEnvironment& _data_sharing;
                DataSharingAttribute _data_attrib;
            public:
                DataSharingEnvironmentSetter(DataSharingEnvironment& data_sharing, DataSharingAttribute data_attrib)
                    : _data_sharing(data_sharing),
                    _data_attrib(data_attrib) { }

                void operator()(Symbol s)
                {
                    _data_sharing.set(s, _data_attrib);
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
            ObjectList<Symbol> shared_references;
            get_clause_symbols(construct.get_clause("shared"), shared_references);
            std::for_each(shared_references.begin(), shared_references.end(), 
                    DataSharingEnvironmentSetter(data_sharing, DS_SHARED));

            ObjectList<Symbol> private_references;
            get_clause_symbols(construct.get_clause("private"), private_references);
            std::for_each(private_references.begin(), private_references.end(), 
                    DataSharingEnvironmentSetter(data_sharing, DS_PRIVATE));

            ObjectList<Symbol> firstprivate_references;
            get_clause_symbols(construct.get_clause("firstprivate"), firstprivate_references);
            std::for_each(firstprivate_references.begin(), firstprivate_references.end(), 
                    DataSharingEnvironmentSetter(data_sharing, DS_FIRSTPRIVATE));

            ObjectList<Symbol> lastprivate_references;
            get_clause_symbols(construct.get_clause("lastprivate"), lastprivate_references);
            std::for_each(lastprivate_references.begin(), lastprivate_references.end(), 
                    DataSharingEnvironmentSetter(data_sharing, DS_LASTPRIVATE));

            ObjectList<OpenMP::ReductionSymbol> reduction_references;
            get_reduction_symbols(construct, construct.get_clause("reduction"), reduction_references);
            std::for_each(reduction_references.begin(), reduction_references.end(), 
                    DataSharingEnvironmentSetterReduction(data_sharing, DS_REDUCTION));

            ObjectList<Symbol> copyin_references;
            get_clause_symbols(construct.get_clause("copyin"), copyin_references);
            std::for_each(copyin_references.begin(), copyin_references.end(), 
                    DataSharingEnvironmentSetter(data_sharing, DS_COPYIN));

            ObjectList<Symbol> copyprivate_references;
            get_clause_symbols(construct.get_clause("copyprivate"), copyprivate_references);
            std::for_each(copyprivate_references.begin(), copyprivate_references.end(), 
                    DataSharingEnvironmentSetter(data_sharing, DS_COPYPRIVATE));
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

                DataSharingAttribute data_attr = data_sharing.get(sym, /* check_enclosing */ false);

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
                            data_sharing.set(sym, DS_SHARED);

                            already_nagged.append(sym);
                        }
                    }
                    else
                    {
                        // Set the symbol as having default data sharing
                        data_sharing.set(sym, (DataSharingAttribute)(default_data_attr | DS_IMPLICIT));
                    }
                }
            }
        }

        void Core::common_parallel_handler(PragmaCustomConstruct construct, DataSharingEnvironment& data_sharing)
        {
            data_sharing.set_is_parallel(true);
            // Analyze things here
            get_data_explicit_attributes(construct, data_sharing);

            DataSharingAttribute default_data_attr = get_default_data_sharing(construct, /* fallback */ DS_SHARED);

            get_data_implicit_attributes(construct, default_data_attr, data_sharing);
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
                data_sharing.set(sym, DS_PRIVATE);
            }
        }

        void Core::common_workshare_handler(PragmaCustomConstruct construct, DataSharingEnvironment& data_sharing)
        {
            get_data_explicit_attributes(construct, data_sharing);

            DataSharingAttribute default_data_attr = get_default_data_sharing(construct, /* fallback */ DS_SHARED);

            get_data_implicit_attributes(construct, default_data_attr, data_sharing);
        }

        // Data sharing computation for tasks.
        //
        // Tasks have slightly different requirements to other OpenMP constructs so their code
        // can't be merged easily

        // get_data_implicit_attributes_task(construct, data_sharing, default_data_attr);
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

                DataSharingAttribute data_attr = data_sharing.get(sym);

                // Do nothing with threadprivates
                if (data_attr == DS_THREADPRIVATE)
                    continue;

                data_attr = data_sharing.get(sym, /* check_enclosing */ false);

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

                            data_sharing.set(sym, (DataSharingAttribute)(DS_FIRSTPRIVATE | DS_IMPLICIT));
                            already_nagged.append(sym);
                        }
                    }
                    else if (default_data_attr == DS_UNDEFINED)
                    {
                        // This is a special case of task
                        bool is_shared = true;
                        DataSharingEnvironment* enclosing = data_sharing.get_enclosing();

                        while ((enclosing != NULL) && is_shared)
                        {
                            is_shared = is_shared && (enclosing->get(sym, /* check_enclosing */ false) == DS_SHARED);
                            // Stop once we see the innermost parallel
                            if (!enclosing->get_is_parallel())
                                break;
                            enclosing = enclosing->get_enclosing();
                        }

                        if (is_shared)
                        {
                            data_sharing.set(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT));
                        }
                        else
                        {
                            data_sharing.set(sym, (DataSharingAttribute)(DS_FIRSTPRIVATE | DS_IMPLICIT));
                        }
                    }
                    else
                    {
                        // Set the symbol as having the default data sharing
                        data_sharing.set(sym, (DataSharingAttribute)(default_data_attr | DS_IMPLICIT));
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
            _openmp_info->push_current_data_sharing(data_sharing);
            common_workshare_handler(construct, data_sharing);
            common_for_handler(construct, data_sharing);
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
        }
        void Core::parallel_sections_handler_post(PragmaCustomConstruct construct)
        {
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

                    data_sharing.set(sym, DS_THREADPRIVATE);
                }
            }
        }
        void Core::threadprivate_handler_post(PragmaCustomConstruct construct) { }

        void Core::task_handler_pre(PragmaCustomConstruct construct)
        {
            if (!Statement::predicate(construct.get_declaration()))
            {
                task_function_handler_pre(construct);
                return;
            }

            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());
            _openmp_info->push_current_data_sharing(data_sharing);

            get_data_explicit_attributes(construct, data_sharing);
            DataSharingAttribute default_data_attr = get_default_data_sharing(construct, /* fallback */ DS_UNDEFINED);

            get_data_implicit_attributes_task(construct, data_sharing, default_data_attr);

            get_dependences_info(construct, data_sharing);
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

#define EMPTY_HANDLERS(_name) \
        void Core::_name##_handler_pre(PragmaCustomConstruct) { } \
        void Core::_name##_handler_post(PragmaCustomConstruct) { }

        EMPTY_HANDLERS(sections)
        EMPTY_HANDLERS(section)
        EMPTY_HANDLERS(barrier)
        EMPTY_HANDLERS(atomic)
        EMPTY_HANDLERS(master)
        EMPTY_HANDLERS(critical)
        EMPTY_HANDLERS(flush)
        EMPTY_HANDLERS(ordered)
    }
}

EXPORT_PHASE(TL::OpenMP::Core)
