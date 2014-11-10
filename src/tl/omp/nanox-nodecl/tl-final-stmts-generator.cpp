/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include "tl-symbol-utils.hpp"
#include "tl-final-stmts-generator.hpp"

namespace TL { namespace Nanox {

    Nodecl::NodeclBase FinalStmtsGenerator::generate_final_stmts(Nodecl::NodeclBase stmts)
    {
        class FinalStatementsPreVisitor : public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                int _num_task_related_pragmas;
                TL::ObjectList<Nodecl::NodeclBase> _function_codes_to_be_duplicated;
                TL::ObjectList<Nodecl::NodeclBase> _already_visited;
                const Nodecl::Utils::SimpleSymbolMap& _function_translation_map;

            public:

                FinalStatementsPreVisitor(const Nodecl::Utils::SimpleSymbolMap& function_tranlation_map)
                    :
                     _num_task_related_pragmas(0),
                     _function_codes_to_be_duplicated(),
                     _already_visited(),
                     _function_translation_map(function_tranlation_map) { }

                void visit(const Nodecl::OpenMP::TaskwaitShallow& taskwait)
                {
                    ++_num_task_related_pragmas;
                    // There is nothing to walk in a taskwait
                }

                void visit(const Nodecl::OpenMP::Task& task)
                {
                    ++_num_task_related_pragmas;
                    walk(task.get_statements());
                }

                void visit(const Nodecl::OpenMP::TaskCall& task_call)
                {
                    ++_num_task_related_pragmas;
                    walk(task_call.get_call());
                }

                void visit(const Nodecl::OpenMP::TaskExpression& task_expr)
                {
                    ++_num_task_related_pragmas;
                    walk(task_expr.get_sequential_code());
                }

                void visit(const Nodecl::FunctionCall &function_call)
                {
                    Nodecl::NodeclBase called = function_call.get_called();
                    if (!called.is<Nodecl::Symbol>())
                        return;

                    TL::Symbol called_sym = called.as<Nodecl::Symbol>().get_symbol();
                    Nodecl::NodeclBase function_code = called_sym.get_function_code();

                    // If the called symbol has not been defined, skip it!
                    if (function_code.is_null())
                        return;

                    // If the current function code has been visited before by this visitor, skip it!
                    if (_already_visited.contains(function_code))
                        return;

                    // If the current function code has been visited before during the
                    // generation of the final statements of another construct, skip it!
                    const std::map<TL::Symbol, TL::Symbol>* map =
                        _function_translation_map.get_simple_symbol_map();
                    if (map->find(called_sym) != map->end())
                        return;

                    _already_visited.append(function_code);

                    int old_num_tasks_detected = _num_task_related_pragmas;
                    walk(function_code);

                    if (old_num_tasks_detected != _num_task_related_pragmas)
                        _function_codes_to_be_duplicated.append(function_code);
                }

                TL::ObjectList<Nodecl::NodeclBase>& get_function_codes_to_be_duplicated()
                {
                    return _function_codes_to_be_duplicated;
                }
        };

        class FinalStatementsGenerator : public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                Nodecl::Utils::SimpleSymbolMap& _function_translation_map;
                RefPtr<OpenMP::FunctionTaskSet> _function_task_set;
                const TL::ObjectList<Nodecl::NodeclBase>& _function_codes_to_be_duplicated;

            public:

                FinalStatementsGenerator(
                        Nodecl::Utils::SimpleSymbolMap& function_tranlation_map,
                        RefPtr<OpenMP::FunctionTaskSet>& function_task_set,
                        const TL::ObjectList<Nodecl::NodeclBase>& function_codes_to_be_duplicated)
                    :
                        _function_translation_map(function_tranlation_map),
                        _function_task_set(function_task_set),
                        _function_codes_to_be_duplicated(function_codes_to_be_duplicated) { }

                void visit(const Nodecl::OpenMP::TaskwaitShallow& taskwait)
                {
                    Nodecl::Utils::remove_from_enclosing_list(taskwait);
                }

                void visit(const Nodecl::OpenMP::Task& task)
                {
                    task.replace(task.get_statements());
                    walk(task);
                }

                void visit(const Nodecl::OpenMP::TaskCall& task_call)
                {
                    task_call.replace(task_call.get_call());
                    walk(task_call);
                }

                void visit(const Nodecl::OpenMP::TaskExpression& task_expr)
                {
                    Nodecl::NodeclBase seq_code = task_expr.get_sequential_code();
                    ERROR_CONDITION(!seq_code.is<Nodecl::ExpressionStatement>(), "Unreachable code\n", 0);
                    task_expr.replace(seq_code.as<Nodecl::ExpressionStatement>().get_nest());
                    walk(task_expr);
                }

                void visit(const Nodecl::FunctionCall& function_call)
                {
                    Nodecl::NodeclBase called = function_call.get_called();
                    if (!called.is<Nodecl::Symbol>())
                        return;

                    TL::Symbol called_sym = called.as<Nodecl::Symbol>().get_symbol();
                    Nodecl::NodeclBase function_code = called_sym.get_function_code();
                    if (!function_code.is_null())
                    {
                        const std::map<TL::Symbol, TL::Symbol>* map =
                            _function_translation_map.get_simple_symbol_map();

                        bool has_been_duplicated = map->find(called_sym) != map->end();

                        if (// If the current function code has to be duplicated
                            _function_codes_to_be_duplicated.contains(function_code)
                                // And it has not been duplicated before
                                && !has_been_duplicated)
                        {
                            TL::Symbol new_function_sym = SymbolUtils::new_function_symbol_for_deep_copy(
                                    called_sym,
                                    called_sym.get_name() + "_mcc_serial");

                            has_been_duplicated = true;
                            _function_translation_map.add_map(called_sym, new_function_sym);

                            Nodecl::NodeclBase new_function_code = Nodecl::Utils::deep_copy(
                                    function_code,
                                    function_code,
                                    _function_translation_map);

                            // Make it member if the enclosing function is member
                            if (called_sym.is_member())
                            {
                                ::class_type_add_member(
                                        symbol_entity_specs_get_class_type(
                                            new_function_sym.get_internal_symbol()),
                                        new_function_sym.get_internal_symbol(),
                                        /* is_definition */ 1);
                            }

                            Nodecl::Utils::prepend_items_before(function_code, new_function_code);
                            walk(new_function_code);
                        }

                        if (has_been_duplicated)
                        {
                            Nodecl::NodeclBase new_function_call = Nodecl::Utils::deep_copy(
                                    function_call,
                                    function_call,
                                    _function_translation_map);

                            function_call.replace(new_function_call);
                        }
                    }
                }
        };

        Nodecl::NodeclBase new_stmts = stmts.shallow_copy();

        FinalStatementsPreVisitor pre_visitor(_function_translation_map);
        pre_visitor.walk(new_stmts);

        FinalStatementsGenerator generator(
                _function_translation_map,
                _function_task_set,
                pre_visitor.get_function_codes_to_be_duplicated());

        generator.walk(new_stmts);

        return new_stmts;
    }


    FinalStmtsGenerator::FinalStmtsGenerator(RefPtr<OpenMP::FunctionTaskSet> function_task_set)
        : _function_task_set(function_task_set),
          _final_stmts_map(),
          _function_translation_map() { }

    void FinalStmtsGenerator::visit(const Nodecl::OpenMP::Task& task)
    {
        walk(task.get_statements());

        //std::cerr << "task: " << task.get_locus_str() << std::endl;
        Nodecl::NodeclBase final_stmts = generate_final_stmts(task.get_statements());
        _final_stmts_map.insert(std::make_pair(task, final_stmts));
    }

    void FinalStmtsGenerator::visit(const Nodecl::OpenMP::TaskCall& task_call)
    {
        // Note that we need to walk over the function call
        // because its arguments may be TaskExpressions
        walk(task_call.get_call());

        //std::cerr << "task call: " << task_call.get_locus_str() << std::endl;
        Nodecl::NodeclBase final_stmts = generate_final_stmts(task_call.get_call());
        _final_stmts_map.insert(std::make_pair(task_call, final_stmts));
    }

    void FinalStmtsGenerator::visit(const Nodecl::OpenMP::TaskExpression& task_expr)
    {
        walk(task_expr.get_sequential_code());

        //std::cerr << "task expression: " << task_expr.get_locus_str() << std::endl;
        Nodecl::NodeclBase final_stmts = generate_final_stmts(task_expr.get_sequential_code());
        _final_stmts_map.insert(std::make_pair(task_expr, final_stmts));
    }

    std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& FinalStmtsGenerator::get_final_stmts()
    {
        return _final_stmts_map;
    }
}}
