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

#include <algorithm>

namespace TL
{
    namespace OpenMP
    {
        Core::Core()
            : PragmaCustomCompilerPhase("omp")
        {
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

            PragmaCustomCompilerPhase::run(dto);
        }

        void Core::pre_run(TL::DTO& dto)
        {
            register_omp_constructs();

            if (!dto.get_keys().contains("openmp_info"))
            {
                DataSharing* root_data_sharing = new DataSharing(NULL);
                /* _openmp_info = */ new OpenMP::Info(root_data_sharing);
                dto.set_object("openmp_info", _openmp_info);
            }

            PragmaCustomCompilerPhase::pre_run(dto);
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

        void Core::get_reduction_symbols(PragmaCustomClause clause, 
                ObjectList<ReductionSymbol>& sym_list)
        {
            if (!clause.is_defined())
                return;

            ObjectList<std::string> arguments = clause.get_arguments();

            if (arguments.empty())
                return;

            // The first argument is special, we have to look for a ':' that is not followed by any other ':'
            // #pragma omp parallel for reduction(A::F : A::d)

            std::string first_arg = arguments[0];
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
                Source src (arg);

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

                    sym_list.append(ReductionSymbol(sym, reductor_name));
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
            get_reduction_symbols(construct.get_clause("reduction"), reduction_references);
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
                ObjectList<std::string> args = default_clause.get_arguments();

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

                if (!sym.is_valid())
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

            // Analyze things here
            get_data_explicit_attributes(construct, data_sharing);

            DataAttribute default_data_attr = get_default_data_sharing(construct, /* fallback */ DA_SHARED);

            get_data_implicit_attributes(construct, default_data_attr, data_sharing);

            _openmp_info->pop_current_data_sharing();
        }

        void Core::common_for_handler(PragmaCustomConstruct construct, DataSharing& data_sharing)
        {
            // FIXME - Set induction variable as private!
        }

        void Core::common_workshare_handler(PragmaCustomConstruct construct, DataSharing& data_sharing)
        {
            // FIXME 
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

#if 0
        void OpenMPTransform::task_compute_implicit_data_sharing(
                OpenMP::Directive &directive,
                ObjectList<Symbol> &captureaddress_references,
                ObjectList<Symbol> &local_references,
                ObjectList<Symbol> &captureprivate_references,
                Scope& function_scope,
                FunctionDefinition &function_definition,
                Statement& construct_body,
                OpenMP::Construct &task_construct)
        {
            // These are used later
            OpenMP::Clause shared_clause = directive.shared_clause();
            ObjectList<IdExpression> captureaddress_references_in_clause = shared_clause.id_expressions();
            OpenMP::Clause captureprivate_clause = directive.firstprivate_clause();
            ObjectList<IdExpression> captureprivate_references_in_clause = captureprivate_clause.id_expressions();
            OpenMP::Clause private_clause = directive.private_clause();
            ObjectList<IdExpression> local_references_in_clause = private_clause.id_expressions();

            OpenMP::CustomClause input_clause = directive.custom_clause("input");
            ObjectList<Expression> input_references_in_clause = input_clause.get_expression_list();
            OpenMP::CustomClause output_clause = directive.custom_clause("output");
            ObjectList<Expression> output_references_in_clause = output_clause.get_expression_list();

            // Default calculus
            OpenMP::DefaultClause default_clause = directive.default_clause();
            enum 
            {
                DK_TASK_INVALID = 0,
                DK_TASK_UNDEFINED,
                DK_TASK_SHARED,
                DK_TASK_FIRSTPRIVATE,
                DK_TASK_PRIVATE,
                DK_TASK_NONE
            } default_task_data_sharing = DK_TASK_INVALID;

            ObjectList<std::string> captureprivate_names;
            captureprivate_names.append("firstprivate");
            if (!default_clause.is_defined())
            {
                // If not given then DK_TASK_UNDEFINED will trigger a more
                // complex calculus of the data sharing involving inherited
                // attributes
                default_task_data_sharing = DK_TASK_UNDEFINED;
            }
            else if (default_clause.is_none())
            {
                default_task_data_sharing = DK_TASK_NONE;
            }
            else if (default_clause.is_private())
            {
                default_task_data_sharing = DK_TASK_PRIVATE;
            }
            else if (default_clause.is_custom(captureprivate_names))
            {
                default_task_data_sharing = DK_TASK_FIRSTPRIVATE;
            }
            else if (default_clause.is_shared())
            {
                default_task_data_sharing = DK_TASK_SHARED;
            }
            else
            {
                std::cerr << default_clause.get_ast().get_locus() << ": warning: unknown default clause '" 
                    << default_clause.prettyprint() << "'. Assuming 'default(firstprivate)'."
                    << std::endl;
                default_task_data_sharing = DK_TASK_FIRSTPRIVATE;
            }

            // Now deal with the references of the body
            {
                // Get all id-expressions in the body construct
                ObjectList<IdExpression> references_body_all
                    = construct_body.non_local_symbol_occurrences(Statement::ONLY_VARIABLES);

                for (ObjectList<IdExpression>::iterator it = references_body_all.begin();
                        it != references_body_all.end();
                        it++)
                {
                    // If the variable has a sharing attribute of 'threadprivate' do not consider
                    // it for anything
                    if ((task_construct.get_data_attribute(it->get_symbol()) & OpenMP::DA_THREADPRIVATE)
                            == OpenMP::DA_THREADPRIVATE)
                    {
                        continue;
                    }


                    // If this symbol appears in any data-sharing clause,
                    // ignore it since it already has an explicit data
                    // sharing attribute
                    //
                    // Note that all captureaddressed things are in
                    // 'captureaddress_references_in_clause',
                    // 'captureaddress_references' might contain less of
                    // them if they are globally accessible
                    Expression expr(it->get_ast(), it->get_scope_link());
                    if (captureaddress_references_in_clause.contains(*it, functor(&IdExpression::get_symbol)) 
                            || captureprivate_references_in_clause.contains(*it, functor(&IdExpression::get_symbol))
                            || local_references_in_clause.contains(*it, functor(&IdExpression::get_symbol))
                            || input_references_in_clause.contains(expr, functor(handle_dep_expr))
                            || output_references_in_clause.contains(expr, functor(handle_dep_expr))
                            )
                        continue;

                    Symbol global_sym = function_scope.get_symbol_from_id_expr(it->get_ast());

                    bool will_be_visible_from_outline = false;
                    bool is_unqualified_member = false;
                    if (global_sym.is_valid()
                            && (global_sym == it->get_symbol()))
                    {
                        // If the function-scope accessible symbol is the same
                        // found then it must be implicitly captureaddress,
                        // instead of capturevalue but since it is accessible
                        // it does not have to be passed
                        //
                        // As an exception, member symbols must be passed as
                        // captureaddress and they will be converted to
                        // "_this->member"
                        will_be_visible_from_outline = true;
                        is_unqualified_member = is_unqualified_member_symbol(it->get_symbol(), function_definition);
                    }

                    switch ((int)default_task_data_sharing)
                    {
                        case DK_TASK_UNDEFINED :
                            {
                                /*
                                 * According to the standard when a variable is
                                 * referenced inside a task construct and no
                                 * default is given and the variable does not
                                 * appear in any data-sharing clause:
                                 *
                                 *  (1) if the task is orphaned and the variable is a
                                 *  parameter then it is 'firstprivate'
                                 *
                                 *  (2) otherwise, if the task is not orphaned and
                                 *  nested inside a parallel and the variable is
                                 *  private in that construct, then it is
                                 *  firstprivate in this task construct
                                 *
                                 *  (3) otherwise, if the task is not nested in a
                                 *  parallel and the variable is private in the
                                 *  enclosing function (this might happen because
                                 *  the induction variable of an enclosing loop or
                                 *  simply because the variable is local)
                                 *
                                 *  (4) otherwise the variable is shared in this
                                 *  task construct
                                 */
                                Symbol sym = it->get_symbol();
                                // This will use the inherited scope if any
                                OpenMP::DataAttribute data_attrib = task_construct.get_data_attribute(sym);

                                if (data_attrib != OpenMP::DA_UNDEFINED)
                                {
                                    // Some enclosing construct defined a data sharing for this attribute
                                    if ((data_attrib & OpenMP::DA_PRIVATE) == OpenMP::DA_PRIVATE)
                                    {
                                        // (2) if an enclosing construct defined private for it (e.g. a 'parallel')
                                        // (3) if an enclosing non-parallel construct defined private for it (e.g. a 'for')
                                        captureprivate_references.insert(sym);
                                        task_construct.add_data_attribute(sym, OpenMP::DA_FIRSTPRIVATE);
                                    }
                                    else if ((data_attrib & OpenMP::DA_SHARED) == OpenMP::DA_SHARED)
                                    {
                                        if (will_be_visible_from_outline)
                                        {
                                            // (4)
                                            // Do nothing, will be shared by scope
                                        }
                                        else 
                                        {
                                            // (4) An enclosing construct (e.g. a 'parallel') defined it shared
                                            captureaddress_references.insert(sym);
                                            task_construct.add_data_attribute(sym, OpenMP::DA_SHARED);
                                        }
                                    }
                                    else
                                    {
                                        // (4) If not shared or private in an
                                        // enclosing scope (but somebody
                                        // defined some data sharing for it) it
                                        // is shared in this task
                                        captureaddress_references.insert(sym);
                                        task_construct.add_data_attribute(sym, OpenMP::DA_SHARED);
                                    }
                                }
                                else
                                {
                                    // No data sharing was set by any enclosing construct
                                    if (will_be_visible_from_outline)
                                    {
                                        // (4) It is shared because it is a global symbol
                                    }
                                    else if (sym.is_static())
                                    {
                                        // (4) It is shared because it is a
                                        // static variable.  Note that this
                                        // static is for local variables, since
                                        // global variables should have
                                        // 'will_be_visible_from_outline' set
                                        // to true
                                        captureaddress_references.insert(sym);
                                        task_construct.add_data_attribute(sym, OpenMP::DA_SHARED);
                                    }
                                    else
                                    {
                                        Type t = sym.get_type();

                                        if (t.is_const()
                                                && (!t.is_class()
                                                    || !t.some_member_is_mutable()))
                                        {
                                            // (3) If it is shared in the enclosing scope (so,
                                            // it is a const variable of a class/struct without
                                            // any mutable member) then it is shared.
                                            captureaddress_references.insert(sym);
                                            task_construct.add_data_attribute(sym, OpenMP::DA_SHARED);
                                        }
                                        else
                                        {
                                            // Otherwise, this includes (1), the symbol is firstprivate
                                            captureprivate_references.insert(sym);
                                            task_construct.add_data_attribute(sym, OpenMP::DA_FIRSTPRIVATE);
                                        }
                                    }
                                }

                                break;
                            }
                        case DK_TASK_NONE :
                            {
                                std::cerr << it->get_ast().get_locus() << ": warning: '" 
                                    << it->prettyprint() << "' does not have a data sharing attribute "
                                    << "and 'default(none)' was specified. " 
                                    << "It will be considered firstprivate." << std::endl;
                                /* Fall through captureprivate */
                            }
                        case DK_TASK_FIRSTPRIVATE :
                            {
                                captureprivate_references.insert(it->get_symbol());
                                task_construct.add_data_attribute(it->get_symbol(), OpenMP::DA_FIRSTPRIVATE);
                                break;
                            }
                        case DK_TASK_SHARED :
                            {
                                // If is not visible from the outline (or
                                // if it is, it is an unqualified member)
                                // then add to the captureaddress
                                if (!will_be_visible_from_outline
                                        || is_unqualified_member)
                                {
                                    captureaddress_references.insert(it->get_symbol());
                                    task_construct.add_data_attribute(it->get_symbol(), OpenMP::DA_SHARED);
                                }
                                break;
                            }
                        case DK_TASK_PRIVATE :
                            {
                                local_references.insert(it->get_symbol());
                                break;
                            }
                        case DK_TASK_INVALID :
                        default:
                            break;
                    }
                }
            }
        }
#endif


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

        EMPTY_HANDLERS(section)
        EMPTY_HANDLERS(barrier)
        EMPTY_HANDLERS(atomic)
        EMPTY_HANDLERS(master)
        EMPTY_HANDLERS(critical)
        EMPTY_HANDLERS(flush)
        EMPTY_HANDLERS(taskwait)
        EMPTY_HANDLERS(ordered)
        EMPTY_HANDLERS(declare_reduction)
    }
}
