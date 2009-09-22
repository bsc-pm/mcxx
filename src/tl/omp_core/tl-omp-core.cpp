/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
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
                DataSharing* root_data_sharing = new DataSharing(NULL);
                _openmp_info = RefPtr<OpenMP::Info>(new OpenMP::Info(root_data_sharing));
                dto.set_object("openmp_info", _openmp_info);
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
            if (!clause.is_defined())
                return;

            ObjectList<ObjectList<std::string> > arguments_unflat = clause.get_arguments_unflattened();

            for (ObjectList<ObjectList<std::string> >::iterator list_it = arguments_unflat.begin();
                    list_it != arguments_unflat.end();
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
                    std::cerr << clause.get_ast().get_locus() << ": warning: 'reduction' clause does not have a valid reductor" << std::endl;
                    std::cerr << clause.get_ast().get_locus() << ": warning: skipping the whole clause" << std::endl;
                    return;
                }

                std::string reductor_name;
                std::copy(first_arg.begin(), split_colon, std::back_inserter(reductor_name));

                std::string remainder_arg;
                std::copy(split_colon + 1, first_arg.end(), std::back_inserter(remainder_arg));

                // Put it back into the arguments array so we do not have to do strange things
                arguments[0] = remainder_arg;

                for (ObjectList<std::string>::iterator it = arguments.begin();
                        it != arguments.end();
                        it++)
                {
                    std::string &arg(*it);
                    Source src;

                    src
                        << "#line " << construct.get_ast().get_line() << " \"" << construct.get_ast().get_file() << "\"\n"
                        << arg
                        ;

                    AST_t expr_tree = src.parse_expression(clause.get_ast(), clause.get_scope_link());

                    Expression expr(expr_tree, clause.get_scope_link());

                    if (!expr.is_id_expression())
                    {
                        std::cerr << clause.get_ast().get_locus() 
                            << ": warning: argument '" 
                            << expr
                            << "' is not an identifier, skipping" 
                            << std::endl;
                    }
                    else
                    {
                        Symbol sym = expr.get_id_expression().get_symbol();

                        if (!sym.is_valid())
                        {
                            std::cerr << clause.get_ast().get_locus()
                                << ": warning: argument '"
                                << expr
                                << "' does not name a valid symbol, skipping"
                                << std::endl
                                ;
                        }

                        Type reduct_type = sym.get_type().advance_over_typedefs().get_unqualified_type();
                        CXX_LANGUAGE()
                        {
                            // Fix the name for the operator
                            if (udr_is_builtin_operator(reductor_name))
                            {
                                reductor_name = "operator " + reductor_name;
                            }
                            // We have to do some more work in C++ to have things working
                            // First do a query in the scope to get all the named entities and then

                            // This is bogus, only neede for the call to solve_udr_name_cxx
                            UDRInfoItem::Associativity assoc = UDRInfoItem::UNDEFINED;

                            Symbol reductor_symbol = solve_udr_name_cxx(construct,
                                    reductor_name,
                                    reduct_type,
                                    assoc);

                            if (reductor_symbol.is_valid())
                            {
                                reductor_name = reductor_symbol.get_qualified_name();
                            }
                            else
                            {
                                // FIXME - solve_udr_name_cxx should have a better interface for this case
                                // This name is dependent, nothing to do here
                                continue;
                            }
                        }

                        ReductionSymbol red_sym(sym, reductor_name, UDRInfoSet(expr.get_scope(), reduct_type));

                        if (red_sym.is_faulty())
                        {
                            running_error("%s: error: user defined reduction for type '%s' and reductor '%s' not declared",
                                    expr.get_ast().get_locus().c_str(),
                                    reduct_type.get_declaration(sym.get_scope(), "").c_str(), 
                                    reductor_name.c_str());
                        }

                        sym_list.append(red_sym);
                    }
                }
            }
        }

        struct DataSharingSetter
        {
            private:
                DataSharing& _data_sharing;
                DataAttribute _data_attrib;
            public:
                DataSharingSetter(DataSharing& data_sharing, DataAttribute data_attrib)
                    : _data_sharing(data_sharing),
                    _data_attrib(data_attrib) { }

                void operator()(Symbol s)
                {
                    _data_sharing.set(s, _data_attrib);
                }
        };

        struct DataSharingSetterReduction
        {
            private:
                DataSharing& _data_sharing;
                DataAttribute _data_attrib;
                std::string _reductor_name;
            public:
                DataSharingSetterReduction(DataSharing& data_sharing, DataAttribute data_attrib)
                    : _data_sharing(data_sharing),
                    _data_attrib(data_attrib) { }

                void operator()(ReductionSymbol red_sym)
                {
                    _data_sharing.set_reduction(red_sym.get_symbol(), red_sym.get_reductor_name());
                }
        };

        void Core::get_data_explicit_attributes(PragmaCustomConstruct construct, 
                DataSharing& data_sharing)
        {
            ObjectList<Symbol> shared_references;
            get_clause_symbols(construct.get_clause("shared"), shared_references);
            std::for_each(shared_references.begin(), shared_references.end(), 
                    DataSharingSetter(data_sharing, DA_SHARED));

            ObjectList<Symbol> private_references;
            get_clause_symbols(construct.get_clause("private"), private_references);
            std::for_each(private_references.begin(), private_references.end(), 
                    DataSharingSetter(data_sharing, DA_PRIVATE));

            ObjectList<Symbol> firstprivate_references;
            get_clause_symbols(construct.get_clause("firstprivate"), firstprivate_references);
            std::for_each(firstprivate_references.begin(), firstprivate_references.end(), 
                    DataSharingSetter(data_sharing, DA_FIRSTPRIVATE));

            ObjectList<Symbol> lastprivate_references;
            get_clause_symbols(construct.get_clause("lastprivate"), lastprivate_references);
            std::for_each(lastprivate_references.begin(), lastprivate_references.end(), 
                    DataSharingSetter(data_sharing, DA_LASTPRIVATE));

            ObjectList<OpenMP::ReductionSymbol> reduction_references;
            std::string reductor_name;
            get_reduction_symbols(construct, construct.get_clause("reduction"), reduction_references);
            std::for_each(reduction_references.begin(), reduction_references.end(), 
                    DataSharingSetterReduction(data_sharing, DA_REDUCTION));

            ObjectList<Symbol> copyin_references;
            get_clause_symbols(construct.get_clause("copyin"), copyin_references);
            std::for_each(copyin_references.begin(), copyin_references.end(), 
                    DataSharingSetter(data_sharing, DA_COPYIN));

            ObjectList<Symbol> copyprivate_references;
            get_clause_symbols(construct.get_clause("copyprivate"), copyprivate_references);
            std::for_each(copyprivate_references.begin(), copyprivate_references.end(), 
                    DataSharingSetter(data_sharing, DA_COPYPRIVATE));
        }

        DataAttribute Core::get_default_data_sharing(PragmaCustomConstruct construct,
                DataAttribute fallback_data_sharing)
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
                    DataAttribute data_attr;
                } pairs[] = 
                {
                    { "none", (DataAttribute)DA_NONE },
                    { "shared", (DataAttribute)DA_SHARED },
                    { "firstprivate", (DataAttribute)DA_FIRSTPRIVATE },
                    { NULL, (DataAttribute)DA_UNDEFINED },
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

                return DA_SHARED;
            }
        }

        void Core::get_data_implicit_attributes(PragmaCustomConstruct construct, 
                DataAttribute default_data_attr, 
                DataSharing& data_sharing)
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

                DataAttribute data_attr = data_sharing.get(sym, /* check_enclosing */ false);

                if (data_attr == DA_UNDEFINED)
                {
                    if (default_data_attr == DA_NONE)
                    {
                        if (!already_nagged.contains(sym))
                        {
                            std::cerr << it->get_ast().get_locus() 
                                << ": warning: symbol '" << sym.get_qualified_name(sym.get_scope()) 
                                << "' does not have data sharing and 'default(none)' was specified. Assuming shared "
                                << std::endl;

                            // Maybe we do not want to assume always shared?
                            data_sharing.set(sym, DA_SHARED);

                            already_nagged.append(sym);
                        }
                    }
                    else
                    {
                        // Set the symbol as having default data sharing
                        data_sharing.set(sym, default_data_attr);
                    }
                }
            }
        }

        void Core::common_parallel_handler(PragmaCustomConstruct construct, DataSharing& data_sharing)
        {
            data_sharing.set_is_parallel(true);
            // Analyze things here
            get_data_explicit_attributes(construct, data_sharing);

            DataAttribute default_data_attr = get_default_data_sharing(construct, /* fallback */ DA_SHARED);

            get_data_implicit_attributes(construct, default_data_attr, data_sharing);
        }

        void Core::common_for_handler(PragmaCustomConstruct construct, DataSharing& data_sharing)
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
                data_sharing.set(sym, DA_PRIVATE);
            }
        }

        void Core::common_workshare_handler(PragmaCustomConstruct construct, DataSharing& data_sharing)
        {
            get_data_explicit_attributes(construct, data_sharing);

            DataAttribute default_data_attr = get_default_data_sharing(construct, /* fallback */ DA_SHARED);

            get_data_implicit_attributes(construct, default_data_attr, data_sharing);
        }

        // Data sharing computation for tasks.
        //
        // Tasks have slightly different requirements to other OpenMP constructs so their code
        // can't be merged easily

        // get_data_implicit_attributes_task(construct, data_sharing, default_data_attr);
        void Core::get_data_implicit_attributes_task(PragmaCustomConstruct construct,
                DataSharing& data_sharing,
                DataAttribute default_data_attr)
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

                DataAttribute data_attr = data_sharing.get(sym);

                // Do nothing with threadprivates
                if (data_attr == DA_THREADPRIVATE)
                    continue;

                data_attr = data_sharing.get(sym, /* check_enclosing */ false);

                if (data_attr == DA_UNDEFINED)
                {
                    if (default_data_attr == DA_NONE)
                    {
                        if (!already_nagged.contains(sym))
                        {
                            std::cerr << it->get_ast().get_locus() 
                                << ": warning: symbol '" << sym.get_qualified_name(sym.get_scope()) 
                                << "' does not have data sharing and 'default(none)' was specified. Assuming firstprivate "
                                << std::endl;

                            data_sharing.set(sym, DA_FIRSTPRIVATE);
                            already_nagged.append(sym);
                        }
                    }
                    else if (default_data_attr == DA_UNDEFINED)
                    {
                        // This is a special case of task
                        bool is_shared = true;
                        DataSharing* enclosing = data_sharing.get_enclosing();

                        while ((enclosing != NULL) && is_shared)
                        {
                            is_shared = is_shared && (enclosing->get(sym, /* check_enclosing */ false) == DA_SHARED);
                            // Stop once we see the innermost parallel
                            if (!enclosing->get_is_parallel())
                                break;
                        }

                        if (is_shared)
                        {
                            data_sharing.set(sym, DA_SHARED);
                        }
                        else
                        {
                            data_sharing.set(sym, DA_FIRSTPRIVATE);
                        }
                    }
                    else
                    {
                        // Set the symbol as having the default data sharing
                        data_sharing.set(sym, default_data_attr);
                    }
                }
            }
        }

        // Handlers
        void Core::parallel_handler_pre(PragmaCustomConstruct construct)
        {
            DataSharing& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());
            _openmp_info->push_current_data_sharing(data_sharing);
            common_parallel_handler(construct, data_sharing);
        }
        void Core::parallel_handler_post(PragmaCustomConstruct construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::parallel_for_handler_pre(PragmaCustomConstruct construct)
        {
            DataSharing& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());
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
            DataSharing& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());
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
            DataSharing& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());
            _openmp_info->push_current_data_sharing(data_sharing);
            common_workshare_handler(construct, data_sharing);
        }
        void Core::single_handler_post(PragmaCustomConstruct construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::parallel_sections_handler_pre(PragmaCustomConstruct construct)
        {
            DataSharing& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());
            _openmp_info->push_current_data_sharing(data_sharing);
            common_parallel_handler(construct, data_sharing);
        }
        void Core::parallel_sections_handler_post(PragmaCustomConstruct construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::threadprivate_handler_pre(PragmaCustomConstruct construct)
        {
            DataSharing& data_sharing = _openmp_info->get_current_data_sharing();

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

                    data_sharing.set(sym, DA_THREADPRIVATE);
                }
            }
        }
        void Core::threadprivate_handler_post(PragmaCustomConstruct construct) { }

        void Core::task_handler_pre(PragmaCustomConstruct construct)
        {
            DataSharing& data_sharing = _openmp_info->get_new_data_sharing(construct.get_ast());
            _openmp_info->push_current_data_sharing(data_sharing);

            get_data_explicit_attributes(construct, data_sharing);
            DataAttribute default_data_attr = get_default_data_sharing(construct, /* fallback */ DA_UNDEFINED);

            get_data_implicit_attributes_task(construct, data_sharing, default_data_attr);
        }

        void Core::task_handler_post(PragmaCustomConstruct construct)
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
        EMPTY_HANDLERS(taskwait)
        EMPTY_HANDLERS(ordered)
    }
}

EXPORT_PHASE(TL::OpenMP::Core)
