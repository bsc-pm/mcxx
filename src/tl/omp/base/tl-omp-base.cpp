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

#include "tl-omp-base.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-diagnostic.h"
#include "cxx-cexpr.h"
#include "fortran03-scope.h"
#include "tl-predicateutils.hpp"

namespace TL { namespace OpenMP {

    template <typename T,
             typename List>
    static void make_dependency_list(
            List& dependences,
            DependencyDirection kind,
            const locus_t* locus,
            ObjectList<Nodecl::NodeclBase>& result_list)
    {
        TL::ObjectList<Nodecl::NodeclBase> data_ref_list;
        for (typename List::iterator it = dependences.begin();
                it != dependences.end();
                it++)
        {
            if (it->get_kind() != kind)
                continue;

            data_ref_list.append(it->get_dependency_expression().shallow_copy());
        }

        if (!data_ref_list.empty())
        {
            result_list.append(T::make(Nodecl::List::make(data_ref_list), locus));
        }
    }

    template <typename T,
             typename List>
    static void make_copy_list(
            List& dependences,
            CopyDirection kind,
            const locus_t* locus,
            ObjectList<Nodecl::NodeclBase>& result_list)
    {
        TL::ObjectList<Nodecl::NodeclBase> data_ref_list;
        for (typename List::iterator it = dependences.begin();
                it != dependences.end();
                it++)
        {
            if (it->get_kind() != kind)
                continue;

            data_ref_list.append(it->get_copy_expression().shallow_copy());
        }

        if (!data_ref_list.empty())
        {
            result_list.append(T::make(Nodecl::List::make(data_ref_list), locus));
        }
    }

    class FunctionCallVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            RefPtr<FunctionTaskSet> _function_task_set;

            typedef std::set<Symbol> module_function_tasks_set_t;
            module_function_tasks_set_t _module_function_tasks;

            // This information is computed by the TransformNonVoidFunctionCalls visitor
            const std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& _funct_call_to_enclosing_stmt_map;
            const std::map<Nodecl::NodeclBase, std::set<TL::Symbol> >& _enclosing_stmt_to_return_vars_map;

            std::map<Nodecl::NodeclBase, TL::ObjectList<Nodecl::NodeclBase> > _enclosing_stmt_to_task_calls_map;

        public:

            FunctionCallVisitor(RefPtr<FunctionTaskSet> function_task_set,
                    const std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& funct_call_to_enclosing_stmt_map,
                    const std::map<Nodecl::NodeclBase, std::set<TL::Symbol> >& enclosing_stmt_to_return_vars_map)
                :
                    _function_task_set(function_task_set),
                    _funct_call_to_enclosing_stmt_map(funct_call_to_enclosing_stmt_map),
                    _enclosing_stmt_to_return_vars_map(enclosing_stmt_to_return_vars_map),
                    _enclosing_stmt_to_task_calls_map()
            {
            }

            virtual void visit(const Nodecl::FunctionCall& call)
            {
                Nodecl::NodeclBase called = call.get_called();

                if (called.is<Nodecl::Symbol>())
                {
                    Symbol sym = called.as<Nodecl::Symbol>().get_symbol();

                    if (sym.is_from_module())
                    {
                        // This symbol comes from a module
                        TL::Symbol module = sym.from_module();
                        sym = sym.aliased_from_module();

                        // Check if we already saw this module
                        module_function_tasks_set_t::iterator it = _module_function_tasks.find(module);
                        if (it == _module_function_tasks.end())
                        {
                            // Not seen before, load
                            _function_task_set->load_from_module(module);

                            _module_function_tasks.insert(module);
                        }
                    }

                    if (_function_task_set->is_function_task(sym))
                    {
                        // Nodecl::NodeclBase exec_env = compute_
                        FunctionTaskInfo& task_info = _function_task_set->get_function_task(sym);

                        Nodecl::NodeclBase exec_env = this->make_exec_environment(call, sym, task_info);

                        Nodecl::OpenMP::TaskCall task_call = Nodecl::OpenMP::TaskCall::make(
                                exec_env,
                                // Do we need to copy this?
                                call.shallow_copy(),
                                call.get_locus());

                        std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>::const_iterator it =
                            _funct_call_to_enclosing_stmt_map.find(call);

                        if (it == _funct_call_to_enclosing_stmt_map.end())
                        {
                            call.replace(task_call);
                        }
                        else
                        {
                            // This was a nonvoid task call!
                            Nodecl::NodeclBase enclosing_stmt = it->second;

                            _enclosing_stmt_to_task_calls_map[enclosing_stmt].append(Nodecl::ExpressionStatement::make(task_call));

                            // Remove from the enclosing list the expression statement which contains the task call
                            Nodecl::Utils::remove_from_enclosing_list(call.get_parent());
                        }
                    }
                }
            }

            void build_all_needed_task_expressions()
            {
                for (std::map<Nodecl::NodeclBase, TL::ObjectList<Nodecl::NodeclBase> >::iterator it = _enclosing_stmt_to_task_calls_map.begin();
                        it != _enclosing_stmt_to_task_calls_map.end();
                        ++it)
                {
                    Nodecl::NodeclBase enclosing_stmt = it->first;
                    TL::ObjectList<Nodecl::NodeclBase> task_calls = it->second;
                    std::set<TL::Symbol> return_arguments = _enclosing_stmt_to_return_vars_map.find(enclosing_stmt)->second;

                    ERROR_CONDITION(!enclosing_stmt.is<Nodecl::ExpressionStatement>(),
                            "Unexpected '%s' node",
                            ast_print_node_type(enclosing_stmt.get_kind()));

                    Nodecl::NodeclBase inline_task;
                    {
                        // The inline tasks are always SMP tasks
                        Nodecl::List exec_environment;
                        exec_environment.append(Nodecl::OpenMP::Target::make(
                                    Nodecl::List::make(Nodecl::Text::make("smp", make_locus("", 0, 0))),
                                    nodecl_null(),
                                    make_locus("", 0, 0)));


                        TL::ObjectList<Nodecl::Symbol> nonlocal_symbols;

                        TL::ObjectList<Nodecl::NodeclBase> input_alloca_dependence,
                            inout_dependences, output_dependences, assumed_firstprivates, alloca_exprs;

                        Nodecl::ExpressionStatement expr_stmt = enclosing_stmt.as<Nodecl::ExpressionStatement>();
                        if (expr_stmt.get_nest().is<Nodecl::AddAssignment>()
                                || expr_stmt.get_nest().is<Nodecl::ArithmeticShrAssignment>()
                                || expr_stmt.get_nest().is<Nodecl::Assignment>()
                                || expr_stmt.get_nest().is<Nodecl::BitwiseAndAssignment>()
                                || expr_stmt.get_nest().is<Nodecl::BitwiseOrAssignment>()
                                || expr_stmt.get_nest().is<Nodecl::BitwiseShlAssignment>()
                                || expr_stmt.get_nest().is<Nodecl::BitwiseShrAssignment>()
                                || expr_stmt.get_nest().is<Nodecl::BitwiseXorAssignment>()
                                || expr_stmt.get_nest().is<Nodecl::DivAssignment>()
                                || expr_stmt.get_nest().is<Nodecl::MinusAssignment>()
                                || expr_stmt.get_nest().is<Nodecl::ModAssignment>()
                                || expr_stmt.get_nest().is<Nodecl::MulAssignment>())
                        {
                            Nodecl::Assignment assignment = enclosing_stmt.as<Nodecl::ExpressionStatement>().get_nest().as<Nodecl::Assignment>();
                            Nodecl::NodeclBase lhs_expr = assignment.get_lhs();
                            Nodecl::NodeclBase rhs_expr = assignment.get_rhs();

                            // Create the output depedence
                            if (expr_stmt.get_nest().is<Nodecl::Assignment>())
                            {
                                output_dependences.append(lhs_expr.shallow_copy());
                            }
                            else
                            {
                                inout_dependences.append(lhs_expr.shallow_copy());
                            }

                            // Obtain the nonlocal symbols from the right expression
                            nonlocal_symbols = Nodecl::Utils::get_nonlocal_symbols_first_occurrence(rhs_expr);
                        }
                        else
                        {
                            nonlocal_symbols = Nodecl::Utils::get_nonlocal_symbols_first_occurrence(enclosing_stmt);
                        }



                        for (TL::ObjectList<Nodecl::Symbol>::iterator it2 = nonlocal_symbols.begin();
                                it2 != nonlocal_symbols.end();
                                ++it2)
                        {
                            TL::Symbol sym = it2->get_symbol();

                            if (!sym.is_variable()
                                    || (sym.is_member()
                                        && !sym.is_static()))
                                continue;

                            std::set<TL::Symbol>::iterator it_sym = return_arguments.find(sym);
                            if (it_sym == return_arguments.end())
                            {
                                // The expressions that are not return arguments are passed as firstprivates
                                assumed_firstprivates.append((*it2).shallow_copy());
                            }
                            else
                            {
                                // The return arguments present in the enclosing statement are added as alloca input dependences
                                Nodecl::NodeclBase sym_nodecl = Nodecl::Symbol::make(sym, make_locus("", 0, 0));
                                sym_nodecl.set_type(sym.get_type());

                                input_alloca_dependence.append(
                                        Nodecl::Dereference::make(
                                            sym_nodecl,
                                            sym.get_type().points_to().get_lvalue_reference_to(),
                                            make_locus("", 0, 0)));

                                // Remove this item from the return arguments set!
                                return_arguments.erase(it_sym);
                            }
                        }

                        // The resting return arguments are added as alloca expressions (i. e. they need to be allocated in the
                        // join task but they should not generate any dependence)
                        for (std::set<TL::Symbol>::iterator it2 = return_arguments.begin();
                                it2 != return_arguments.end();
                                ++it2)
                        {
                            TL::Symbol sym = *it2;

                            Nodecl::NodeclBase sym_nodecl = Nodecl::Symbol::make(sym, make_locus("", 0, 0));
                            sym_nodecl.set_type(sym.get_type());

                            alloca_exprs.append(
                                    Nodecl::Dereference::make(
                                        sym_nodecl,
                                        sym.get_type().points_to().get_lvalue_reference_to(),
                                        make_locus("", 0, 0)));
                        }

                        if (!assumed_firstprivates.empty())
                        {
                            exec_environment.append(
                                    Nodecl::OpenMP::Firstprivate::make(
                                        Nodecl::List::make(assumed_firstprivates),
                                        make_locus("", 0, 0)));
                        }

                        if (!alloca_exprs.empty())
                        {
                            exec_environment.append(
                                    Nodecl::OpenMP::Alloca::make(
                                        Nodecl::List::make(alloca_exprs),
                                        make_locus("", 0, 0)));
                        }

                        if (!input_alloca_dependence.empty())
                        {
                            exec_environment.append(
                                    Nodecl::OpenMP::DepInAlloca::make(
                                        Nodecl::List::make(input_alloca_dependence),
                                        make_locus("", 0, 0)));
                        }

                        if (!inout_dependences.empty())
                        {
                            exec_environment.append(
                                    Nodecl::OpenMP::DepOut::make(
                                        Nodecl::List::make(inout_dependences),
                                        make_locus("", 0, 0)));
                        }

                        if (!output_dependences.empty())
                        {
                            exec_environment.append(
                                    Nodecl::OpenMP::DepOut::make(
                                        Nodecl::List::make(output_dependences),
                                        make_locus("", 0, 0)));
                        }

                        inline_task = Nodecl::OpenMP::Task::make(
                                exec_environment,
                                Nodecl::List::make(enclosing_stmt.shallow_copy()),
                                make_locus("", 0 ,0));
                    }

                    Nodecl::NodeclBase task_expr = Nodecl::ExpressionStatement::make(
                            Nodecl::OpenMP::TaskExpression::make(
                                inline_task,
                                Nodecl::List::make(task_calls),
                                make_locus("", 0, 0)));

                    Nodecl::Utils::prepend_items_before(enclosing_stmt, task_expr);
                    Nodecl::Utils::remove_from_enclosing_list(enclosing_stmt);
                }
            }

        private:
            Nodecl::NodeclBase make_exec_environment(const Nodecl::FunctionCall &call,
                    TL::Symbol function_sym,
                    FunctionTaskInfo& function_task_info)
            {
                const locus_t* locus = call.get_locus();

                TL::ObjectList<Nodecl::NodeclBase> result_list;

                TL::ObjectList<FunctionTaskDependency> task_dependences = function_task_info.get_parameter_info();

                make_dependency_list<Nodecl::OpenMP::DepIn>(
                        task_dependences,
                        OpenMP::DEP_DIR_IN,
                        locus,
                        result_list);

                make_dependency_list<Nodecl::OpenMP::DepInValue>(
                        task_dependences,
                        OpenMP::DEP_DIR_IN_VALUE,
                        locus,
                        result_list);

                make_dependency_list<Nodecl::OpenMP::DepOut>(
                        task_dependences,
                        OpenMP::DEP_DIR_OUT,
                        locus,
                        result_list);

                make_dependency_list<Nodecl::OpenMP::DepInout>(
                        task_dependences,
                        OpenMP::DEP_DIR_INOUT,
                        locus,
                        result_list);

                make_dependency_list<Nodecl::OpenMP::Concurrent>(
                        task_dependences,
                        OpenMP::DEP_CONCURRENT,
                        locus,
                        result_list);

                make_dependency_list<Nodecl::OpenMP::Commutative>(
                        task_dependences,
                        OpenMP::DEP_COMMUTATIVE,
                        locus,
                        result_list);

                // Make sure the remaining symbols are firstprivate
                std::vector<bool> has_dep(function_sym.get_type().parameters().size(), false);

                for (TL::ObjectList<FunctionTaskDependency>::iterator it = task_dependences.begin();
                        it != task_dependences.end();
                        it++)
                {
                    TL::DataReference data_ref = it->get_data_reference();
                    TL::Symbol base_sym = data_ref.get_base_symbol();

                    if (base_sym.is_parameter_of(function_sym))
                    {
                        has_dep[base_sym.get_parameter_position_in(function_sym)] = true;
                    }
                }

                TL::ObjectList<TL::Symbol> parameters = function_sym.get_related_symbols();

                TL::ObjectList<Nodecl::NodeclBase> assumed_firstprivates, assumed_shareds;

                int i = 0;
                for (TL::ObjectList<TL::Symbol>::iterator it = parameters.begin();
                        it != parameters.end();
                        it++, i++)
                {
                    ERROR_CONDITION(i >= (signed int)has_dep.size(), "Mismatch between parameters and related symbols", 0);
                    if (!has_dep[i])
                    {
                        if (!IS_FORTRAN_LANGUAGE
                                || !it->get_type().is_any_reference())
                        {
                            Nodecl::Symbol symbol_ref =
                                Nodecl::Symbol::make(*it, locus);
                            symbol_ref.set_type(lvalue_ref(it->get_type().get_internal_type()));

                            assumed_firstprivates.append(symbol_ref);
                        }
                        else if (IS_FORTRAN_LANGUAGE)
                        {
                            Nodecl::Symbol symbol_ref =
                                Nodecl::Symbol::make(*it, locus);
                            symbol_ref.set_type(lvalue_ref(it->get_type().get_internal_type()));

                            warn_printf("%s: warning assuming dummy argument '%s' of function task '%s' "
                                    "is SHARED because it does not have VALUE attribute\n",
                                    function_sym.get_locus_str().c_str(),
                                    it->get_name().c_str(),
                                    function_sym.get_name().c_str());

                            assumed_shareds.append(symbol_ref);
                        }
                    }
                }

                if (!assumed_firstprivates.empty())
                {
                    result_list.append(
                            Nodecl::OpenMP::Firstprivate::make(
                                Nodecl::List::make(assumed_firstprivates),
                                locus));
                }

                if (!assumed_shareds.empty())
                {
                    result_list.append(
                            Nodecl::OpenMP::Shared::make(
                                Nodecl::List::make(assumed_shareds),
                                locus));
                }

                // Build the tree which contains the target information
                TargetInfo target_info = function_task_info.get_target_info();

                TL::ObjectList<Nodecl::NodeclBase> devices;
                TL::ObjectList<Nodecl::NodeclBase> target_items;

                ObjectList<std::string> device_list = target_info.get_device_list();
                for (TL::ObjectList<std::string>::iterator it = device_list.begin(); it != device_list.end(); ++it)
                {
                    devices.append(Nodecl::Text::make(strtolower(it->c_str()), locus));
                }

                ObjectList<CopyItem> copy_in = target_info.get_copy_in();
                make_copy_list<Nodecl::OpenMP::CopyIn>(
                        copy_in,
                        OpenMP::COPY_DIR_IN,
                        locus,
                        target_items);

                ObjectList<CopyItem> copy_out = target_info.get_copy_out();
                make_copy_list<Nodecl::OpenMP::CopyOut>(
                        copy_out,
                        OpenMP::COPY_DIR_OUT,
                        locus,
                        target_items);

                ObjectList<CopyItem> copy_inout = target_info.get_copy_inout();
                make_copy_list<Nodecl::OpenMP::CopyInout>(
                        copy_inout,
                        OpenMP::COPY_DIR_INOUT,
                        locus,
                        target_items);

                ObjectList<Nodecl::NodeclBase> ndrange_exprs = target_info.get_shallow_copy_of_ndrange();
                if (!ndrange_exprs.empty())
                {
                    target_items.append(
                            Nodecl::OpenMP::NDRange::make(
                                Nodecl::List::make(ndrange_exprs),
                                Nodecl::Symbol::make(target_info.get_target_symbol(), locus),
                                locus));
                }

                ObjectList<Nodecl::NodeclBase> onto_exprs = target_info.get_shallow_copy_of_onto();
                if (!onto_exprs.empty())
                {
                    target_items.append(
                            Nodecl::OpenMP::Onto::make(
                                Nodecl::List::make(onto_exprs),
                                Nodecl::Symbol::make(target_info.get_target_symbol(), locus),
                                locus));
                }

                std::string file = target_info.get_file();
                if (!file.empty())
                {
                    target_items.append(
                            Nodecl::OpenMP::File::make(
                                Nodecl::Text::make(file),
                                Nodecl::Symbol::make(target_info.get_target_symbol(), locus),
                                locus));
                }

                std::string name = target_info.get_name();
                if (!name.empty())
                {
                    target_items.append(
                            Nodecl::OpenMP::Name::make(
                                Nodecl::Text::make(name),
                                Nodecl::Symbol::make(target_info.get_target_symbol(), locus),
                                locus));
                }

                ObjectList<FunctionTaskInfo::implementation_pair_t> implementation_table =
                    function_task_info.get_devices_with_implementation();
                for (ObjectList<FunctionTaskInfo::implementation_pair_t>::iterator it = implementation_table.begin();
                        it != implementation_table.end(); ++it)
                {
                    target_items.append(
                            Nodecl::OpenMP::Implements::make(
                                Nodecl::Text::make(it->first),
                                Nodecl::Symbol::make(it->second, locus),
                                locus));
                }

                result_list.append(
                        Nodecl::OpenMP::Target::make(
                            Nodecl::List::make(devices),
                            Nodecl::List::make(target_items),
                            locus));

                if (function_task_info.get_untied())
                {
                    result_list.append(
                            Nodecl::OpenMP::Untied::make(locus));
                }

                if (!function_task_info.get_if_clause_conditional_expression().is_null())
                {
                    result_list.append(
                        Nodecl::OpenMP::If::make(function_task_info.get_if_clause_conditional_expression().shallow_copy())
                       );
                }

                if (!function_task_info.get_priority_clause_expression().is_null())
                {
                    result_list.append(
                        Nodecl::OpenMP::Priority::make(function_task_info.get_priority_clause_expression().shallow_copy())
                        );
                }

                if (!function_task_info.get_task_label().is_null())
                {
                    result_list.append(
                            Nodecl::OpenMP::TaskLabel::make(
                                function_task_info.get_task_label().get_text()));
                }

                return Nodecl::List::make(result_list);
            }
    };


    // This visitor transforms a nonvoid function task into a void function task
    // Example:
    //
    //      #pragma omp task in(x)
    //      int foo(float x)
    //      {
    //          if (x > 0.0)
    //              return -1;
    //          return 1;
    //      }
    //
    // This code is transformed into:
    //
    //      #pragma omp task in(x) out(*output)
    //      void foo__(float x, int* output)
    //      {
    //          if (x > 0.0)
    //          {
    //              *output = -1;
    //              return;
    //          }
    //          *output = 1;
    //          return;
    //      }
    //
    // Note 1: this visitor does not transform any function task call. For this
    // reason, the original nonvoid function task cannot be removed from the
    // function task set at this point.
    //
    // Note 2: the new void function task is added to the task set
    //
    // Note 3: Some nonvoid function calls may be transformed into void
    // function calls. This only happens in recursive nonvoid function tasks:
    //
    //      #pragma omp task in(x)
    //      int bar(int x)
    //      {
    //          if (x < 0) return 1;
    //          return bar(x-1);
    //      }
    //
    // This code is transformed into:
    //
    //      #pragma omp task in(x) out(*output)
    //      void bar__(int x, int*output)
    //      {
    //          if (x < 0) { *output = 1; return; }
    //          *output = bar__(x-1);
    //          return;
    //      }
    //
    // This is because when we are copying the body of the function, we replace
    // the 'bar' symbol by the 'bar__' symbol. The TransformNonVoidFunctionCalls
    // visitor should take care of this case.
    //
    class TransformNonVoidFunctionTasks : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            RefPtr<FunctionTaskSet> _function_task_set;
            std::map<TL::Symbol, TL::Symbol> _transformed_task_map;

        public:
            TransformNonVoidFunctionTasks(RefPtr<FunctionTaskSet> function_task_set)
                :
                _function_task_set(function_task_set),
                _transformed_task_map()
            {
            }

            virtual void visit(const Nodecl::FunctionCall& func_call)
            {
                Nodecl::NodeclBase arguments = func_call.get_arguments();
                // First of all, visit the arguments of the current function call
                walk(arguments);

                Nodecl::NodeclBase called = func_call.get_called();
                if (!called.is<Nodecl::Symbol>())
                    return;

                TL::Symbol function_called = called.as<Nodecl::Symbol>().get_symbol();

                if (!_function_task_set->is_function_task(function_called))
                    return;

                if (function_called.get_type().returns().is_void())
                    return;

                // Already handled
                if (_transformed_task_map.find(function_called) != _transformed_task_map.end())
                    return;

                // Create the void function task with one parameter more than the original function
                std::string new_function_name = function_called.get_name() + "__";

                TL::ObjectList<std::string> parameter_names;
                TL::ObjectList<TL::Type> parameter_types;

                parameter_types = function_called.get_type().parameters();
                parameter_types.append(function_called.get_type().returns().get_pointer_to());

                TL::ObjectList<TL::Symbol> function_called_related_symbols = function_called.get_related_symbols();
                for (TL::ObjectList<TL::Symbol>::iterator it = function_called_related_symbols.begin();
                        it != function_called_related_symbols.end();
                        it++)
                {
                    parameter_names.append(it->get_name());
                }
                parameter_names.append("output");

                TL::Symbol new_function = new_function_symbol(
                        function_called,
                        new_function_name,
                        TL::Type::get_void_type(),
                        parameter_names,
                        parameter_types);

                Nodecl::NodeclBase new_function_code,new_function_body;
                build_empty_body_for_function(new_function,
                        new_function_code,
                        new_function_body);

                new_function.get_internal_symbol()->entity_specs.function_code = new_function_code.get_internal_nodecl();

                // Create the code of the new void function task
                Nodecl::NodeclBase function_called_code = function_called.get_function_code();

                ERROR_CONDITION(function_called_code.is_null(), "The '%s' function should be defined",
                        function_called.get_name().c_str());

                Nodecl::NodeclBase function_called_stmts;
                if (function_called_code.is<Nodecl::FunctionCode>())
                {
                    function_called_stmts = function_called_code.as<Nodecl::FunctionCode>().get_statements();
                }
                else if (function_called_code.is<Nodecl::TemplateFunctionCode>())
                {
                    function_called_stmts = function_called_code.as<Nodecl::TemplateFunctionCode>().get_statements();
                }
                else
                {
                    internal_error("Unexpected node '%s'\n", ast_print_node_type(function_called_code.get_kind()));
                }

                Nodecl::Utils::SimpleSymbolMap translation_map;
                TL::ObjectList<TL::Symbol> new_funcion_related_symbols = new_function.get_related_symbols();
                for (unsigned int i = 0; i < function_called_related_symbols.size(); ++i)
                {
                    translation_map.add_map(function_called_related_symbols[i], new_funcion_related_symbols[i]);
                }
                translation_map.add_map(function_called, new_function);

                Nodecl::NodeclBase updated_function_stmts = Nodecl::Utils::deep_copy(
                        function_called_stmts,
                        new_function.get_related_scope(),
                        translation_map);

                ReplaceReturnStatementsByAssignments visitor(
                        new_funcion_related_symbols[new_funcion_related_symbols.size()-1]);
                visitor.walk(updated_function_stmts);

                new_function_body.replace(updated_function_stmts);

                // Remove the nonvoid function code from the tree and append the new function code
                Nodecl::Utils::remove_from_enclosing_list(function_called_code);
                Nodecl::Utils::append_to_top_level_nodecl(new_function_code);

                // Copy the function task info from the original function to
                // the new one. Note that we need to add a new dependence: the
                // output dependence over the new argument
                FunctionTaskInfo function_task_info = _function_task_set->get_function_task(function_called);

                FunctionTaskInfo copied_function_task_info(function_task_info, translation_map, new_function);

                TL::Symbol return_argument = new_funcion_related_symbols[new_funcion_related_symbols.size()-1];
                Nodecl::NodeclBase return_argument_nodecl = Nodecl::Symbol::make(
                        return_argument,
                        return_argument.get_locus());
                return_argument_nodecl.set_type(return_argument.get_type());

                TL::DataReference data_ref_dep(
                        Nodecl::Dereference::make(
                            return_argument_nodecl,
                            return_argument_nodecl.get_type().points_to(),
                            return_argument.get_locus()));

                FunctionTaskDependency result_dependence(data_ref_dep, TL::OpenMP::DEP_DIR_OUT);

                copied_function_task_info.add_function_task_dependency(result_dependence);
                _function_task_set->add_function_task(new_function, copied_function_task_info);

                // Update the map of transformed tasks
                _transformed_task_map.insert(std::make_pair(function_called, new_function));
            }

            std::map<TL::Symbol, TL::Symbol>& get_transformed_task_map()
            {
                return _transformed_task_map;
            }

       private:

            TL::Symbol new_function_symbol(
                    TL::Symbol current_function,
                    const std::string& name,
                    TL::Type return_type,
                    ObjectList<std::string> parameter_names,
                    ObjectList<TL::Type> parameter_types)
            {
                if (IS_FORTRAN_LANGUAGE && current_function.is_nested_function())
                {
                    // Get the enclosing function
                    current_function = current_function.get_scope().get_related_symbol();
                }

                decl_context_t decl_context = current_function.get_scope().get_decl_context();

                ERROR_CONDITION(parameter_names.size() != parameter_types.size(), "Mismatch between names and types", 0);

                decl_context_t function_context;
                if (IS_FORTRAN_LANGUAGE)
                {
                    function_context = new_program_unit_context(decl_context);
                }
                else
                {
                    function_context = new_function_context(decl_context);
                    function_context = new_block_context(function_context);
                }

                // Build the function type
                int num_parameters = 0;
                scope_entry_t** parameter_list = NULL;

                parameter_info_t* p_types = new parameter_info_t[parameter_types.size()];
                parameter_info_t* it_ptypes = &(p_types[0]);
                ObjectList<TL::Type>::iterator type_it = parameter_types.begin();
                for (ObjectList<std::string>::iterator it = parameter_names.begin();
                        it != parameter_names.end();
                        it++, it_ptypes++, type_it++)
                {
                    scope_entry_t* param = new_symbol(function_context, function_context.current_scope, it->c_str());
                    param->entity_specs.is_user_declared = 1;
                    param->kind = SK_VARIABLE;
                    param->locus = make_locus("", 0, 0);

                    param->defined = 1;

                    param->type_information = get_unqualified_type(type_it->get_internal_type());

                    P_LIST_ADD(parameter_list, num_parameters, param);

                    it_ptypes->is_ellipsis = 0;
                    it_ptypes->nonadjusted_type_info = NULL;
                    it_ptypes->type_info = get_indirect_type(param);
                }

                type_t *function_type = get_new_function_type(
                        return_type.get_internal_type(),
                        p_types,
                        parameter_types.size());

                delete[] p_types;

                // Now, we can create the new function symbol
                scope_entry_t* new_function_sym = NULL;
                if (!current_function.get_type().is_template_specialized_type())
                {
                    new_function_sym = new_symbol(decl_context, decl_context.current_scope, name.c_str());
                    new_function_sym->entity_specs.is_user_declared = 1;
                    new_function_sym->kind = SK_FUNCTION;
                    new_function_sym->locus = make_locus("", 0, 0);
                    new_function_sym->type_information = function_type;
                }
                else
                {
                    scope_entry_t* new_template_sym = new_symbol(
                            decl_context, decl_context.current_scope, name.c_str());
                    new_template_sym->kind = SK_TEMPLATE;
                    new_template_sym->locus = make_locus("", 0, 0);

                    new_template_sym->type_information = get_new_template_type(
                            decl_context.template_parameters,
                            function_type,
                            uniquestr(name.c_str()),
                            decl_context, make_locus("", 0, 0));

                    template_type_set_related_symbol(new_template_sym->type_information, new_template_sym);

                    // The new function is the primary template specialization
                    new_function_sym = named_type_get_symbol(
                            template_type_get_primary_type(
                                new_template_sym->type_information));
                }

                function_context.function_scope->related_entry = new_function_sym;
                function_context.block_scope->related_entry = new_function_sym;

                new_function_sym->related_decl_context = function_context;

                new_function_sym->entity_specs.related_symbols = parameter_list;
                new_function_sym->entity_specs.num_related_symbols = num_parameters;
                for (int i = 0; i < new_function_sym->entity_specs.num_related_symbols; ++i)
                {
                    symbol_set_as_parameter_of_function(
                            new_function_sym->entity_specs.related_symbols[i], new_function_sym, /* parameter position */ i);
                }

                // Make it static
                new_function_sym->entity_specs.is_static = 1;

                // Make it member if the enclosing function is member
                if (current_function.is_member())
                {
                    new_function_sym->entity_specs.is_member = 1;
                    new_function_sym->entity_specs.class_type = current_function.get_class_type().get_internal_type();

                    new_function_sym->entity_specs.access = AS_PUBLIC;

                    ::class_type_add_member(new_function_sym->entity_specs.class_type, new_function_sym);
                }
                return new_function_sym;
            }

            void build_empty_body_for_function(
                    TL::Symbol function_symbol,
                    Nodecl::NodeclBase &function_code,
                    Nodecl::NodeclBase &empty_stmt)
            {
                empty_stmt = Nodecl::EmptyStatement::make(make_locus("", 0, 0));
                Nodecl::List stmt_list = Nodecl::List::make(empty_stmt);

                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                {
                    Nodecl::CompoundStatement compound_statement =
                        Nodecl::CompoundStatement::make(stmt_list,
                                /* destructors */ Nodecl::NodeclBase::null(),
                                make_locus("", 0, 0));
                    stmt_list = Nodecl::List::make(compound_statement);
                }

                Nodecl::NodeclBase context = Nodecl::Context::make(
                        stmt_list,
                        function_symbol.get_related_scope(), make_locus("", 0, 0));

                function_symbol.get_internal_symbol()->defined = 1;

                if (function_symbol.is_dependent_function())
                {
                    function_code = Nodecl::TemplateFunctionCode::make(context,
                            // Initializers
                            Nodecl::NodeclBase::null(),
                            // Internal functions
                            Nodecl::NodeclBase::null(),
                            function_symbol,
                            make_locus("", 0, 0));
                }
                else
                {
                    function_code = Nodecl::FunctionCode::make(context,
                            // Initializers
                            Nodecl::NodeclBase::null(),
                            // Internal functions
                            Nodecl::NodeclBase::null(),
                            function_symbol,
                            make_locus("", 0, 0));
                }
            }

        class ReplaceReturnStatementsByAssignments : public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                TL::Symbol _output_symbol;

            public:

            ReplaceReturnStatementsByAssignments(TL::Symbol output_symbol)
                : _output_symbol(output_symbol)
            {
            }

            virtual void visit(const Nodecl::ReturnStatement& ret_stmt)
            {
                Nodecl::NodeclBase return_expr = ret_stmt.get_value();
                if (!return_expr.is_null())
                {
                    Nodecl::NodeclBase output_symbol_nodecl = Nodecl::Symbol::make(_output_symbol, _output_symbol.get_locus());
                    output_symbol_nodecl.set_type(_output_symbol.get_type().get_lvalue_reference_to());

                    TL::ObjectList<Nodecl::NodeclBase> stmt_list;
                    stmt_list.append(
                            Nodecl::ExpressionStatement::make(
                                Nodecl::Assignment::make(
                                    Nodecl::Dereference::make(
                                        output_symbol_nodecl,
                                        _output_symbol.get_type().no_ref().points_to().get_lvalue_reference_to(),
                                        _output_symbol.get_locus()),
                                    return_expr,
                                    _output_symbol.get_type(),
                                    ret_stmt.get_locus())));

                      stmt_list.append(Nodecl::ReturnStatement::make(/* statements */ nodecl_null()));

                      ret_stmt.replace(Nodecl::CompoundStatement::make(Nodecl::List::make(stmt_list), nodecl_null(), ret_stmt.get_locus()));
                }
            }
        };

    };


    // This visitor transforms a nonvoid function task call into a void function task call.
    // Example:
    //
    //      NOTE: This function has been transformed by TransformNonVoidFunctionTasks visitor
    //      #pragma omp task in(x) out(*output)
    //      void foo(float x, int* output)
    //      {
    //          if (x > 0.0)
    //          {
    //              *output = -1;
    //              return;
    //          }
    //          *output = 1;
    //          return;
    //      }
    //
    //      int main()
    //      {
    //          int x;
    //          x = foo(2.4);
    //      }
    //
    // This code is transformed into:
    //
    //      NOTE: This function has been transformed by TransformNonVoidFunctionTasks visitor
    //      #pragma omp task in(x) out(*output)
    //      void foo(float x, int* output)
    //      {
    //          if (x > 0.0)
    //          {
    //              *output = -1;
    //              return;
    //          }
    //          *output = 1;
    //          return;
    //      }
    //
    //      int main()
    //      {
    //          int x;
    //          int* mcc_ret_0;
    //          foo__(2.4, mcc_ret_0);
    //          x = *mcc_ret_0;
    //      }
    //
    // Note 1: Some original nonvoid function calls may be transformed into
    // void function calls in the TransformNonVoidFunctionTasks visitor. This
    // only happens in recursive functions, for more information read the Note 3
    // of the TransformNonVoidFunctionTasks visitor.
    //
    // Note 2: After all function calls are visited, we can remove safetly the
    // nonvoid function task from the function task set (It has been already
    // removed from the tree)
    //
    class TransformNonVoidFunctionCalls : public Nodecl::ExhaustiveVisitor<void>
    {
        private:

            int _counter;

            RefPtr<FunctionTaskSet> _function_task_set;
            const std::map<TL::Symbol, TL::Symbol>& _transformed_task_map;
            TL::ObjectList<TL::Symbol> _transformed_tasks;

            std::map<Nodecl::NodeclBase, Nodecl::NodeclBase> _funct_call_to_enclosing_stmt_map;
            std::map<Nodecl::NodeclBase, std::set<TL::Symbol> > _enclosing_stmt_to_return_vars_map;

        public:

            TransformNonVoidFunctionCalls(
                    RefPtr<FunctionTaskSet> function_task_Set,
                    const std::map<TL::Symbol, TL::Symbol>& transformed_tasks)
                :
                    _counter(0),
                    _function_task_set(function_task_Set),
                    _transformed_task_map(transformed_tasks),
                    _funct_call_to_enclosing_stmt_map(),
                    _enclosing_stmt_to_return_vars_map()
            {
                for (std::map<TL::Symbol, TL::Symbol>::const_iterator it = _transformed_task_map.begin();
                        it != _transformed_task_map.end();
                        it++)
                {
                    _transformed_tasks.insert(it->second);
                }

            }

            // virtual void visit(const Nodecl::ObjectInit& object_init)
            // {
            //     TL::Symbol sym = object_init.get_symbol();
            //     if (sym.get_value().is_null())
            //         return;

            //     walk(sym.get_value());
            // }

            virtual void visit(const Nodecl::FunctionCall& func_call)
            {
                Nodecl::NodeclBase arguments = func_call.get_arguments();
                // First of all, visit the arguments of the current function call
                walk(arguments);

                Nodecl::NodeclBase called = func_call.get_called();
                if (!called.is<Nodecl::Symbol>())
                    return;

                TL::Symbol function_called = called.as<Nodecl::Symbol>().get_symbol();

                if (!_function_task_set->is_function_task(function_called))
                    return;

                if (function_called.get_type().returns().is_void()
                        // was this function a nonvoid funcion call transformed into a void function call by the deep_copy in the
                        // TransformNonVoidFunctionTasks visitor? (for more information read Note 1)
                        && !_transformed_tasks.contains(function_called))
                    return;

                TL::Symbol transformed_task;
                TL::Type return_type;
                if (!_transformed_tasks.contains(function_called))
                {
                    std::map<TL::Symbol, TL::Symbol>::const_iterator it_transformed_task = _transformed_task_map.find(function_called);
                    ERROR_CONDITION(it_transformed_task == _transformed_task_map.end(), "Unreachable code", 0);

                    transformed_task = it_transformed_task->second;
                    return_type = function_called.get_type().returns().get_pointer_to();
                }
                else
                {
                    ERROR_CONDITION(!function_called.get_type().returns().is_void(), "Unreachable code", 0);
                    transformed_task = function_called;
                    TL::ObjectList<TL::Symbol> params =  function_called.get_related_symbols();
                    return_type = params[params.size() - 1].get_type();
                }

                // Create a new function call to the new void function. Replace the original
                // function call by the variable used to store the result value

                // 1. Create the new called entity
                Nodecl::NodeclBase called_entity = Nodecl::Symbol::make(
                        transformed_task,
                        func_call.get_locus());

                called_entity.set_type(transformed_task.get_type());

                // 2. Declare a new variable which represents the return of the original function as an argument
                Scope scope = func_call.retrieve_context();

                std::stringstream ss;
                ss << "mcc_ret_" << _counter;
                TL::Symbol return_arg_sym = scope.new_symbol(ss.str());
                _counter++;

                return_arg_sym.get_internal_symbol()->kind = SK_VARIABLE;
                return_arg_sym.get_internal_symbol()->type_information = return_type.get_internal_type();
                return_arg_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;

                // 3. Extend the list of arguments adding the output argument
                Nodecl::NodeclBase return_arg_nodecl = Nodecl::Symbol::make(
                        return_arg_sym,
                        func_call.get_locus());

                return_arg_nodecl.set_type(return_arg_sym.get_type().get_lvalue_reference_to());

                Nodecl::NodeclBase new_arguments = func_call.get_arguments();
                if (!new_arguments.is_null())
                {
                    new_arguments.as<Nodecl::List>().append(return_arg_nodecl);
                }
                else
                {
                    new_arguments = Nodecl::List::make(return_arg_nodecl);
                }

                // 4. Create the new function call and encapsulate it in a ExpressionStatement
                Nodecl::NodeclBase new_function_call =
                    Nodecl::FunctionCall::make(
                            called_entity,
                            new_arguments,
                            /* alternate_name */ nodecl_null(),
                            /* function_form */ nodecl_null(),
                            TL::Type::get_void_type(),
                            func_call.get_locus());

                Nodecl::NodeclBase expression_stmt =
                    Nodecl::ExpressionStatement::make(new_function_call);

                // 5. Prepend the new function call before the enclosing statement
                // of the original function call
                Nodecl::NodeclBase enclosing_stmt = func_call;
                while (!enclosing_stmt.is_null()
                        && !enclosing_stmt.is<Nodecl::ExpressionStatement>())
                {
                    enclosing_stmt = enclosing_stmt.get_parent();
                }

                ERROR_CONDITION(enclosing_stmt.is_null(),
                        "This node should be a Nodecl::ExpressionStatement", 0);

                // Update the map between function calls and their enclosing statement
                _funct_call_to_enclosing_stmt_map.insert(std::make_pair(new_function_call, enclosing_stmt));

                // Add the definition of the return variables (only for C++)
                CXX_LANGUAGE()
                {
                    Nodecl::Utils::prepend_items_before(
                            enclosing_stmt,
                            Nodecl::CxxDef::make(
                                /* context */ nodecl_null(),
                                return_arg_sym,
                                func_call.get_locus()));
                }

                Nodecl::Utils::prepend_items_before(enclosing_stmt, expression_stmt);

                // 6. Replace the original function call by the variable
                Nodecl::NodeclBase dereference_return =
                    Nodecl::Dereference::make(
                            return_arg_nodecl,
                            return_arg_sym.get_type().points_to().get_lvalue_reference_to(),
                            func_call.get_locus());

                func_call.replace(dereference_return);

                // Update the map between enclosing statement and their return arguments
                _enclosing_stmt_to_return_vars_map[enclosing_stmt].insert(return_arg_sym);
            }

            void remove_nonvoid_function_task_from_function_task_set()
            {
                for (std::map<TL::Symbol, TL::Symbol>::const_iterator it = _transformed_task_map.begin();
                        it != _transformed_task_map.end();
                        it++)
                {
                    TL::Symbol function_called = it->first;

                    FunctionTaskInfo function_task_info = _function_task_set->get_function_task(function_called);
                    _function_task_set->remove_function_task(function_called);
                }
            }

            std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& get_function_call_to_enclosing_stmt_map()
            {
                return _funct_call_to_enclosing_stmt_map;
            }

            std::map<Nodecl::NodeclBase, std::set<TL::Symbol> >& get_enclosing_stmt_to_return_variables_map()
            {
                return _enclosing_stmt_to_return_vars_map;
            }
    };

    Base::Base()
        : PragmaCustomCompilerPhase("omp"), _core(), _simd_enabled(false)
    {
        set_phase_name("OpenMP directive to parallel IR");
        set_phase_description("This phase lowers the semantics of OpenMP into the parallel IR of Mercurium");

        register_parameter("omp_dry_run",
                "Disables OpenMP transformation",
                _openmp_dry_run,
                "0");

        register_parameter("discard_unused_data_sharings",
                "Discards unused data sharings in the body of the construct. "
                "This behaviour may cause wrong code be emitted, use at your own risk",
                _discard_unused_data_sharings_str,
                "0").connect(functor(&Base::set_discard_unused_data_sharings, *this));

        register_parameter("simd_enabled",
                "If set to '1' enables simd constructs, otherwise it is disabled",
                _simd_enabled_str,
                "0").connect(functor(&Base::set_simd, *this));

        register_parameter("allow_shared_without_copies",
                "If set to '1' allows shared without any copy directionality, otherwise they are set to copy_inout",
                _allow_shared_without_copies_str,
                "0").connect(functor(&Base::set_allow_shared_without_copies, *this));

#define OMP_DIRECTIVE(_directive, _name, _pred) \
                if (_pred) { \
                    std::string directive_name = remove_separators_of_directive(_directive); \
                    dispatcher().directive.pre[directive_name].connect(functor(&Base::_name##_handler_pre, *this)); \
                    dispatcher().directive.post[directive_name].connect(functor(&Base::_name##_handler_post, *this)); \
                }
#define OMP_CONSTRUCT_COMMON(_directive, _name, _noend, _pred) \
                if (_pred) { \
                    std::string directive_name = remove_separators_of_directive(_directive); \
                    dispatcher().declaration.pre[directive_name].connect(functor((void (Base::*)(TL::PragmaCustomDeclaration))&Base::_name##_handler_pre, *this)); \
                    dispatcher().declaration.post[directive_name].connect(functor((void (Base::*)(TL::PragmaCustomDeclaration))&Base::_name##_handler_post, *this)); \
                    dispatcher().statement.pre[directive_name].connect(functor((void (Base::*)(TL::PragmaCustomStatement))&Base::_name##_handler_pre, *this)); \
                    dispatcher().statement.post[directive_name].connect(functor((void (Base::*)(TL::PragmaCustomStatement))&Base::_name##_handler_post, *this)); \
                }
#define OMP_CONSTRUCT(_directive, _name, _pred) OMP_CONSTRUCT_COMMON(_directive, _name, false, _pred)
#define OMP_CONSTRUCT_NOEND(_directive, _name, _pred) OMP_CONSTRUCT_COMMON(_directive, _name, true, _pred)
#include "tl-omp-constructs.def"
#undef OMP_DIRECTIVE
#undef OMP_CONSTRUCT_COMMON
#undef OMP_CONSTRUCT
#undef OMP_CONSTRUCT_NOEND
    }

    void Base::pre_run(TL::DTO& dto)
    {
        _core.pre_run(dto);

        // Do nothing once we have analyzed everything
        if (_openmp_dry_run != "0")
            return;

        this->PragmaCustomCompilerPhase::pre_run(dto);
    }

    void Base::run(TL::DTO& dto)
    {
        _core.run(dto);

        // Do nothing once we have analyzed everything
        if (_openmp_dry_run != "0")
            return;

        this->PragmaCustomCompilerPhase::run(dto);

        RefPtr<FunctionTaskSet> function_task_set = RefPtr<FunctionTaskSet>::cast_static(dto["openmp_task_info"]);

        Nodecl::NodeclBase translation_unit = dto["nodecl"];

        TransformNonVoidFunctionTasks transform_nonvoid_tasks(function_task_set);
        transform_nonvoid_tasks.walk(translation_unit);
        const std::map<TL::Symbol, TL::Symbol>& transformed_tasks =
            transform_nonvoid_tasks.get_transformed_task_map();

        TransformNonVoidFunctionCalls transform_nonvoid_task_calls(function_task_set, transformed_tasks);
        transform_nonvoid_task_calls.walk(translation_unit);
        transform_nonvoid_task_calls.remove_nonvoid_function_task_from_function_task_set();
        const std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& funct_call_to_enclosing_stmt_map =
            transform_nonvoid_task_calls.get_function_call_to_enclosing_stmt_map();
        const std::map<Nodecl::NodeclBase, std::set<TL::Symbol> >& enclosing_stmt_to_return_vars_map =
            transform_nonvoid_task_calls.get_enclosing_stmt_to_return_variables_map();

        FunctionCallVisitor function_call_visitor(
                function_task_set,
                funct_call_to_enclosing_stmt_map,
                enclosing_stmt_to_return_vars_map);
        function_call_visitor.walk(translation_unit);
        function_call_visitor.build_all_needed_task_expressions();
    }

#define INVALID_STATEMENT_HANDLER(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomStatement ctr) { \
            error_printf("%s: error: invalid '#pragma %s %s'\n",  \
                    ctr.get_locus_str().c_str(), \
                    ctr.get_text().c_str(), \
                    ctr.get_pragma_line().get_text().c_str()); \
        } \
        void Base::_name##_handler_post(TL::PragmaCustomStatement) { }

#define INVALID_DECLARATION_HANDLER(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomDeclaration ctr) { \
            error_printf("%s: error: invalid '#pragma %s %s'\n",  \
                    ctr.get_locus_str().c_str(), \
                    ctr.get_text().c_str(), \
                    ctr.get_pragma_line().get_text().c_str()); \
        } \
        void Base::_name##_handler_post(TL::PragmaCustomDeclaration) { }

        INVALID_DECLARATION_HANDLER(parallel)
        INVALID_DECLARATION_HANDLER(parallel_for)
        INVALID_DECLARATION_HANDLER(parallel_simd_for)
        INVALID_DECLARATION_HANDLER(parallel_do)
        INVALID_DECLARATION_HANDLER(for)
        INVALID_DECLARATION_HANDLER(simd_for)
        INVALID_DECLARATION_HANDLER(do)
        INVALID_DECLARATION_HANDLER(parallel_sections)
        INVALID_DECLARATION_HANDLER(sections)
        INVALID_DECLARATION_HANDLER(single)
        INVALID_DECLARATION_HANDLER(critical)
        INVALID_DECLARATION_HANDLER(atomic)
        INVALID_DECLARATION_HANDLER(master)

#define EMPTY_HANDLERS_CONSTRUCT(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomStatement) { } \
        void Base::_name##_handler_post(TL::PragmaCustomStatement) { } \
        void Base::_name##_handler_pre(TL::PragmaCustomDeclaration) { } \
        void Base::_name##_handler_post(TL::PragmaCustomDeclaration) { } \

#define EMPTY_HANDLERS_DIRECTIVE(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomDirective) { } \
        void Base::_name##_handler_post(TL::PragmaCustomDirective) { }

        EMPTY_HANDLERS_CONSTRUCT(ordered)

        EMPTY_HANDLERS_DIRECTIVE(section)

        EMPTY_HANDLERS_DIRECTIVE(taskyield)

    void Base::set_simd(const std::string &simd_enabled_str)
    {
        parse_boolean_option("simd_enabled",
                simd_enabled_str,
                _simd_enabled,
                "Assuming false");
    }

    void Base::set_allow_shared_without_copies(const std::string &allow_shared_without_copies_str)
    {
        bool b = false;
        parse_boolean_option("allow_shared_without_copies",
                allow_shared_without_copies_str, b, "Assuming false");
        _core.set_allow_shared_without_copies(b);
    }

    void Base::set_discard_unused_data_sharings(const std::string& str)
    {
        bool b = false;
        parse_boolean_option("discard_unused_data_sharings",
                str,
                b,
                "Assuming false");
        _core.set_discard_unused_data_sharings(b);
    }

    void Base::atomic_handler_pre(TL::PragmaCustomStatement) { }
    void Base::atomic_handler_post(TL::PragmaCustomStatement directive)
    {
        TL::PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = Nodecl::List::make(
                Nodecl::OpenMP::FlushAtEntry::make(
                        directive.get_locus()),
                Nodecl::OpenMP::FlushAtExit::make(
                        directive.get_locus())
        );

        Nodecl::OpenMP::Atomic atomic =
            Nodecl::OpenMP::Atomic::make(
                    execution_environment,
                    directive.get_statements().shallow_copy(),
                    directive.get_locus());

        pragma_line.diagnostic_unused_clauses();
        directive.replace(atomic);
    }

    void Base::critical_handler_pre(TL::PragmaCustomStatement) { }
    void Base::critical_handler_post(TL::PragmaCustomStatement directive)
    {
        TL::PragmaCustomLine pragma_line = directive.get_pragma_line();

        TL::PragmaCustomParameter param = pragma_line.get_parameter();

        Nodecl::List execution_environment;

        Nodecl::OpenMP::FlushAtEntry entry_flush =
            Nodecl::OpenMP::FlushAtEntry::make(
                    directive.get_locus()
            );

        Nodecl::OpenMP::FlushAtExit exit_flush =
            Nodecl::OpenMP::FlushAtExit::make(
                    directive.get_locus()
            );

        if (param.is_defined())
        {
            ObjectList<std::string> critical_name = param.get_tokenized_arguments();

            execution_environment = Nodecl::List::make(
                    Nodecl::OpenMP::CriticalName::make(critical_name[0],
                        directive.get_locus()),
                    entry_flush, exit_flush);
        }
        else
        {
            execution_environment = Nodecl::List::make(entry_flush, exit_flush);
        }

        pragma_line.diagnostic_unused_clauses();
        directive.replace(
                Nodecl::OpenMP::Critical::make(
                        execution_environment,
                        directive.get_statements().shallow_copy(),
                        directive.get_locus())
                );
    }

    void Base::barrier_handler_pre(TL::PragmaCustomDirective) { }
    void Base::barrier_handler_post(TL::PragmaCustomDirective directive)
    {
        TL::PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = Nodecl::List::make(
                Nodecl::OpenMP::FlushAtEntry::make(
                    directive.get_locus()),
                Nodecl::OpenMP::FlushAtExit::make(
                    directive.get_locus())
                );

        pragma_line.diagnostic_unused_clauses();
        directive.replace(
                Nodecl::OpenMP::BarrierFull::make(
                    execution_environment,
                    directive.get_locus())
                );
    }

    void Base::flush_handler_pre(TL::PragmaCustomDirective) { }
    void Base::flush_handler_post(TL::PragmaCustomDirective directive)
    {
        TL::PragmaCustomLine pragma_line = directive.get_pragma_line();
        PragmaClauseArgList parameter = directive.get_pragma_line().get_parameter();

        TL::ObjectList<Nodecl::NodeclBase> expr_list;
        if (!parameter.is_null())
        {
            expr_list = parameter.get_arguments_as_expressions();
        }

        pragma_line.diagnostic_unused_clauses();
        directive.replace(
                Nodecl::OpenMP::FlushMemory::make(
                    Nodecl::List::make(expr_list),
                    directive.get_locus())
                );
    }

    void Base::master_handler_pre(TL::PragmaCustomStatement) { }
    void Base::master_handler_post(TL::PragmaCustomStatement directive)
    {
        TL::PragmaCustomLine pragma_line = directive.get_pragma_line();
        pragma_line.diagnostic_unused_clauses();
        directive.replace(
                Nodecl::OpenMP::Master::make(
                    directive.get_statements().shallow_copy(),
                    directive.get_locus())
                );
    }

    void Base::taskwait_handler_pre(TL::PragmaCustomDirective) { }
    void Base::taskwait_handler_post(TL::PragmaCustomDirective directive)
    {
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        PragmaCustomClause on_clause = pragma_line.get_clause("on");
        TL::ObjectList<Nodecl::NodeclBase> expr_list;
        if (on_clause.is_defined())
        {
            expr_list = on_clause.get_arguments_as_expressions();
        }

        Nodecl::List environment;
        PragmaCustomClause noflush_clause = pragma_line.get_clause("noflush");

        if (noflush_clause.is_defined())
        {
            environment.append(
                    Nodecl::OpenMP::NoFlush::make(directive.get_locus()));
        }

        pragma_line.diagnostic_unused_clauses();
        if (!expr_list.empty())
        {
            Nodecl::OpenMP::DepInout dep_inout = Nodecl::OpenMP::DepInout::make(
                    Nodecl::List::make(expr_list),
                    directive.get_locus());

            environment.append(dep_inout);

            directive.replace(
                    Nodecl::OpenMP::WaitOnDependences::make(
                        environment,
                        directive.get_locus())
                    );
        }
        else
        {
            directive.replace(
                    Nodecl::OpenMP::TaskwaitShallow::make(
                        environment,
                        directive.get_locus())
                    );
        }
    }

    // Inline tasks
    void Base::task_handler_pre(TL::PragmaCustomStatement) { }
    void Base::task_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment(ds, pragma_line);

        PragmaCustomClause untied = pragma_line.get_clause("untied");
        if (untied.is_defined())
        {
            execution_environment.append(
                    Nodecl::OpenMP::Untied::make(
                        directive.get_locus()));
        }

        PragmaCustomClause priority = pragma_line.get_clause("priority");
        if (priority.is_defined())
        {
            TL::ObjectList<Nodecl::NodeclBase> expr_list = priority.get_arguments_as_expressions(directive);

            if (expr_list.size() != 1)
            {
                warn_printf("%s: warning: ignoring invalid 'priority' clause in 'task' construct\n",
                        directive.get_locus_str().c_str());
            }
            else
            {
                execution_environment.append(
                        Nodecl::OpenMP::Priority::make(
                            expr_list[0],
                            directive.get_locus()));
            }
        }

        // Attach the implicit flushes at the entry and exit of the task (for analysis purposes)
        execution_environment.append(
                Nodecl::OpenMP::FlushAtEntry::make(
                    directive.get_locus())
        );
        execution_environment.append(
                Nodecl::OpenMP::FlushAtExit::make(
                    directive.get_locus())
        );

        // Label task (this is used only for instrumentation)
        PragmaCustomClause label_clause = pragma_line.get_clause("label");
        if (label_clause.is_defined())
        {
            TL::ObjectList<std::string> str_list = label_clause.get_tokenized_arguments();

            if (str_list.size() != 1)
            {
                warn_printf("%s: warning: ignoring invalid 'label' clause in 'task' construct\n",
                        directive.get_locus_str().c_str());
            }
            else
            {
                execution_environment.append(
                        Nodecl::OpenMP::TaskLabel::make(
                            str_list[0],
                            directive.get_locus()));
            }
        }

        Nodecl::NodeclBase async_code =
                    Nodecl::OpenMP::Task::make(execution_environment,
                        directive.get_statements().shallow_copy(),
                        directive.get_locus());

        async_code = honour_if_clause(pragma_line, directive, async_code);

        pragma_line.diagnostic_unused_clauses();
        directive.replace(async_code);
    }

    void Base::parallel_handler_pre(TL::PragmaCustomStatement) { }
    void Base::parallel_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment(ds, pragma_line);

        Nodecl::NodeclBase num_threads;
        PragmaCustomClause clause = pragma_line.get_clause("num_threads");
        if (clause.is_defined())
        {
            ObjectList<Nodecl::NodeclBase> args = clause.get_arguments_as_expressions();

            // Let core check this for us
            ERROR_CONDITION (args.size() != 1, "num_threads wrong clause", 0);

            num_threads = args[0];
        }

        // Since the parallel construct implies a barrier at its end,
        // there is no need of adding a flush at end, because the barrier implies also a flush
        execution_environment.append(
                Nodecl::OpenMP::FlushAtEntry::make(
                    directive.get_locus())
        );
        execution_environment.append(
                Nodecl::OpenMP::FlushAtExit::make(
                    directive.get_locus())
        );

        Nodecl::NodeclBase parallel_code = Nodecl::OpenMP::Parallel::make(
                    execution_environment,
                    num_threads,
                    directive.get_statements().shallow_copy(),
                    directive.get_locus());

        parallel_code = honour_if_clause(pragma_line, directive, parallel_code);

        pragma_line.diagnostic_unused_clauses();
        directive.replace(parallel_code);
    }

    void Base::single_handler_pre(TL::PragmaCustomStatement) { }
    void Base::single_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment(ds, pragma_line);

        if (!pragma_line.get_clause("nowait").is_defined())
        {
            execution_environment.append(
                    Nodecl::OpenMP::FlushAtExit::make(
                        directive.get_locus())
            );

            execution_environment.append(
                    Nodecl::OpenMP::BarrierAtEnd::make(
                        directive.get_locus()));
        }

        Nodecl::List code;
        code.append(
                Nodecl::OpenMP::Single::make(
                    execution_environment,
                    directive.get_statements().shallow_copy(),
                    directive.get_locus()));

        pragma_line.diagnostic_unused_clauses();
        directive.replace(code);
    }


    void Base::for_handler_pre(TL::PragmaCustomStatement) { }
    void Base::for_handler_post(TL::PragmaCustomStatement directive)
    {
        Nodecl::NodeclBase statement = directive.get_statements();
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();
        ERROR_CONDITION(!statement.is<Nodecl::Context>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::Context>().get_in_context();
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();

        PragmaCustomLine pragma_line = directive.get_pragma_line();
        bool barrier_at_end = !pragma_line.get_clause("nowait").is_defined();

        Nodecl::NodeclBase code = loop_handler_post(directive, statement, barrier_at_end, /* is_combined_worksharing */ false);
        pragma_line.diagnostic_unused_clauses();
        directive.replace(code);
    }

    Nodecl::NodeclBase Base::sections_handler_common(
            TL::PragmaCustomStatement directive,
            Nodecl::NodeclBase statements,
            bool barrier_at_end,
            bool is_combined_worksharing)
    {
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();
        Nodecl::List execution_environment = this->make_execution_environment(ds, pragma_line);

        // Set the implicit OpenMP flush / barrier nodes to the environment
        if (barrier_at_end)
        {
            execution_environment.append(
                    Nodecl::OpenMP::FlushAtExit::make(
                        directive.get_locus())
            );
            execution_environment.append(
                    Nodecl::OpenMP::BarrierAtEnd::make(
                        directive.get_locus()));
        }

        if (is_combined_worksharing)
        {
            execution_environment.append(
                    Nodecl::OpenMP::CombinedWorksharing::make(
                        directive.get_locus()));
        }

        ERROR_CONDITION(!statements.is<Nodecl::List>(), "This is not a list!", 0);
        Nodecl::List tasks = statements.as<Nodecl::List>();

        // There is an extra compound statement right after #pragma omp sections
        ERROR_CONDITION(!tasks[0].is<Nodecl::CompoundStatement>(), "Expecting a compound statement here", 0);
        tasks = tasks[0].as<Nodecl::CompoundStatement>().get_statements().as<Nodecl::List>();

        Nodecl::List section_list;

        for (Nodecl::List::iterator it = tasks.begin(); it != tasks.end(); it++)
        {
            ERROR_CONDITION(!it->is<Nodecl::PragmaCustomStatement>(), "Unexpected node '%s'\n",
                    ast_print_node_type(it->get_kind()));

            Nodecl::PragmaCustomStatement p = it->as<Nodecl::PragmaCustomStatement>();

            section_list.append(
                    Nodecl::OpenMP::Section::make(
                        p.get_statements().shallow_copy(),
                        p.get_locus()));
        }

        Nodecl::OpenMP::Sections sections =
            Nodecl::OpenMP::Sections::make(
                    execution_environment,
                    section_list,
                    directive.get_locus());

        Nodecl::NodeclBase code = Nodecl::List::make(sections);

        return code;
    }

    Nodecl::NodeclBase Base::loop_handler_post(
            TL::PragmaCustomStatement directive,
            Nodecl::NodeclBase statement,
            bool barrier_at_end,
            bool is_combined_worksharing)
    {
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();
        Nodecl::List execution_environment = this->make_execution_environment(ds, pragma_line);

        if (pragma_line.get_clause("schedule").is_defined())
        {
            PragmaCustomClause clause = pragma_line.get_clause("schedule");

            ObjectList<std::string> arguments = clause.get_tokenized_arguments();

            Nodecl::NodeclBase chunk;

            std::string schedule = arguments[0];
            schedule = strtolower(schedule.c_str());

            if (arguments.size() == 1)
            {
                if (schedule == "static" || schedule == "ompss_static")
                {
                    chunk = const_value_to_nodecl(const_value_get_signed_int(0));
                }
                else
                {
                    chunk = const_value_to_nodecl(const_value_get_signed_int(1));
                }
            }
            else if (arguments.size() == 2)
            {
                chunk = Source(arguments[1]).parse_expression(directive);
            }
            else
            {
                // Core should have checked this
                internal_error("Invalid values in schedule clause", 0);
            }

            std::string checked_schedule_name = schedule;

            // Allow OpenMP schedules be prefixed with ompss_
            std::string ompss_prefix = "ompss_";
            if (checked_schedule_name.substr(0, ompss_prefix.size()) == ompss_prefix)
            {
                checked_schedule_name = checked_schedule_name.substr(ompss_prefix.size());
            }

            if (checked_schedule_name == "static"
                    || checked_schedule_name == "dynamic"
                    || checked_schedule_name == "guided"
                    || checked_schedule_name == "runtime"
                    || checked_schedule_name == "auto")
            {
                execution_environment.append(
                        Nodecl::OpenMP::Schedule::make(
                            chunk,
                            schedule,
                            directive.get_locus()));
            }
            else
            {
                internal_error("Invalid schedule '%s' for schedule clause\n",
                        schedule.c_str());
            }
        }
        else
        {
            // def-sched-var is STATIC in our implementation
            execution_environment.append(
                    Nodecl::OpenMP::Schedule::make(
                        ::const_value_to_nodecl(const_value_get_signed_int(0)),
                        "static",
                        directive.get_locus()));
        }

        if (barrier_at_end)
        {
            execution_environment.append(
                    Nodecl::OpenMP::FlushAtExit::make(
                        directive.get_locus())
            );

            execution_environment.append(
                    Nodecl::OpenMP::BarrierAtEnd::make(
                        directive.get_locus()));
        }

        if (is_combined_worksharing)
        {
            execution_environment.append(
                    Nodecl::OpenMP::CombinedWorksharing::make(
                        directive.get_locus()));
        }

        ERROR_CONDITION (!statement.is<Nodecl::ForStatement>(), "Invalid tree of kind '%s'", ast_print_node_type(statement.get_kind()));
        TL::ForStatement for_statement(statement.as<Nodecl::ForStatement>());

        Nodecl::OpenMP::For distribute =
            Nodecl::OpenMP::For::make(
                    execution_environment,
                    // This is a list because of multidimensional distribution
                    Nodecl::List::make(
                        Nodecl::OpenMP::ForRange::make(
                            for_statement.get_lower_bound(),
                            for_statement.get_upper_bound(),
                            for_statement.get_step(),
                            for_statement.get_induction_variable(),
                            for_statement.get_locus())),
                    for_statement.get_statement(),
                    directive.get_locus());

        Nodecl::NodeclBase code = Nodecl::List::make(distribute);

        return code;
    }

    void Base::do_handler_pre(TL::PragmaCustomStatement directive) { }
    void Base::do_handler_post(TL::PragmaCustomStatement directive)
    {
        Nodecl::NodeclBase statement = directive.get_statements();
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();

        PragmaCustomLine pragma_line = directive.get_pragma_line();
        bool barrier_at_end = !pragma_line.get_clause("nowait").is_defined();

        Nodecl::NodeclBase code = loop_handler_post(directive, statement, barrier_at_end, /* is_combined_worksharing */ false);
        pragma_line.diagnostic_unused_clauses();
        directive.replace(code);
    }

    void Base::parallel_do_handler_pre(TL::PragmaCustomStatement directive) { }
    void Base::parallel_do_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();
        Nodecl::List execution_environment = this->make_execution_environment_for_combined_worksharings(ds, pragma_line);

        Nodecl::NodeclBase statement = directive.get_statements();
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();

        Nodecl::NodeclBase num_threads;
        PragmaCustomClause clause = pragma_line.get_clause("num_threads");
        if (clause.is_defined())
        {
            ObjectList<Nodecl::NodeclBase> args = clause.get_arguments_as_expressions();

            // Let core check this for us
            ERROR_CONDITION (args.size() != 1, "num_threads wrong clause", 0);

            num_threads = args[0];
        }

        // Set implicit flushes at the entry and exit of the combined worksharing
        execution_environment.append(
                Nodecl::OpenMP::FlushAtEntry::make(
                    directive.get_locus())
        );
        execution_environment.append(
                Nodecl::OpenMP::FlushAtExit::make(
                    directive.get_locus())
        );

        Nodecl::NodeclBase code = loop_handler_post(directive, statement, /* barrier_at_end */ false, /* is_combined_worksharing */ true);

        Nodecl::NodeclBase parallel_code
            = Nodecl::OpenMP::Parallel::make(
                execution_environment,
                num_threads,
                code,
                directive.get_locus());

        parallel_code = honour_if_clause(pragma_line, directive, parallel_code);

        pragma_line.diagnostic_unused_clauses();
        directive.replace(parallel_code);
    }

    // Function tasks
    void Base::task_handler_pre(TL::PragmaCustomDeclaration declaration) { }
    void Base::task_handler_post(TL::PragmaCustomDeclaration decl)
    {
        TL::PragmaCustomLine pragma_line = decl.get_pragma_line();
        pragma_line.diagnostic_unused_clauses();
        Nodecl::Utils::remove_from_enclosing_list(decl);
    }

    void Base::target_handler_pre(TL::PragmaCustomStatement stmt)   { }
    void Base::target_handler_pre(TL::PragmaCustomDeclaration decl) { }

    void Base::target_handler_post(TL::PragmaCustomStatement stmt)
    {
        TL::PragmaCustomLine pragma_line = stmt.get_pragma_line();
        pragma_line.diagnostic_unused_clauses();
        stmt.replace(stmt.get_statements());
    }

    void Base::target_handler_post(TL::PragmaCustomDeclaration decl)
    {
        TL::PragmaCustomLine pragma_line = decl.get_pragma_line();
        if (decl.get_nested_pragma().is_null())
        {
            Nodecl::NodeclBase result;
            ObjectList<Nodecl::NodeclBase> devices;
            ObjectList<Nodecl::NodeclBase> symbols;

            const locus_t* locus = decl.get_locus();

            PragmaCustomClause device_clause = pragma_line.get_clause("device");
            if (device_clause.is_defined())
            {
                ObjectList<std::string> device_names = device_clause.get_tokenized_arguments();
                for (ObjectList<std::string>::iterator it = device_names.begin();
                        it != device_names.end();
                        ++it)
                {
                    devices.append(Nodecl::Text::make(*it, locus));
                }
            }

            ERROR_CONDITION(!decl.has_symbol(),
                    "%s: expecting a function declaration or definition", decl.get_locus_str().c_str());

            Symbol sym = decl.get_symbol();
            symbols.append(Nodecl::Symbol::make(sym, locus));

            result = Nodecl::OpenMP::TargetDeclaration::make(
                    Nodecl::List::make(devices),
                    Nodecl::List::make(symbols),
                    locus);

            pragma_line.diagnostic_unused_clauses();
            decl.replace(result);
        }
        else
        {
            pragma_line.diagnostic_unused_clauses();
            Nodecl::Utils::remove_from_enclosing_list(decl);
        }
    }

    // SIMD For Statement
    void Base::simd_handler_pre(TL::PragmaCustomStatement) { }
    void Base::simd_handler_post(TL::PragmaCustomStatement stmt)
    {
        TL::PragmaCustomLine pragma_line = stmt.get_pragma_line();
        // Skipping AST_LIST_NODE
        Nodecl::NodeclBase statements = stmt.get_statements();

        if (_simd_enabled)
        {
            ERROR_CONDITION(!statements.is<Nodecl::List>(),
                    "'pragma omp simd' Expecting a AST_LIST_NODE (1)", 0);
            Nodecl::List ast_list_node = statements.as<Nodecl::List>();
            ERROR_CONDITION(ast_list_node.size() != 1,
                    "AST_LIST_NODE after '#pragma omp simd' must be equal to 1 (1)", 0);

            // Skipping NODECL_CONTEXT
            Nodecl::NodeclBase context = ast_list_node.front();
            ERROR_CONDITION(!context.is<Nodecl::Context>(),
                    "'pragma omp simd' Expecting a NODECL_CONTEXT", 0);

            // Skipping AST_LIST_NODE
            Nodecl::NodeclBase in_context = context.as<Nodecl::Context>().get_in_context();
            ERROR_CONDITION(!in_context.is<Nodecl::List>(),
                    "'pragma omp simd' Expecting a AST_LIST_NODE (2)", 0);
            Nodecl::List ast_list_node2 = in_context.as<Nodecl::List>();
            ERROR_CONDITION(ast_list_node2.size() != 1,
                    "AST_LIST_NODE after '#pragma omp simd' must be equal to 1 (2)", 0);

            Nodecl::NodeclBase for_statement = ast_list_node2.front();
            ERROR_CONDITION(!for_statement.is<Nodecl::ForStatement>(),
                    "Unexpected node %s. Expecting a ForStatement after '#pragma omp simd'",
                    ast_print_node_type(for_statement.get_kind()));

            Nodecl::OpenMP::Simd omp_simd_node =
               Nodecl::OpenMP::Simd::make(
                       for_statement.shallow_copy(),
                       for_statement.get_locus());

            pragma_line.diagnostic_unused_clauses();
            stmt.replace(Nodecl::List::make(omp_simd_node));
        }
    }

    // SIMD Functions
    void Base::simd_handler_pre(TL::PragmaCustomDeclaration decl) { }
    void Base::simd_handler_post(TL::PragmaCustomDeclaration decl)
    {
        TL::PragmaCustomLine pragma_line = decl.get_pragma_line();
        if (_simd_enabled)
        {
            ERROR_CONDITION(!decl.has_symbol(), "Expecting a function definition here (1)", 0);

            TL::Symbol sym = decl.get_symbol();
            ERROR_CONDITION(!sym.is_function(), "Expecting a function definition here (2)", 0);

            Nodecl::NodeclBase node = sym.get_function_code();
            ERROR_CONDITION(!node.is<Nodecl::FunctionCode>(), "Expecting a function definition here (3)", 0);

            Nodecl::OpenMP::SimdFunction simd_func =
                Nodecl::OpenMP::SimdFunction::make(
                        node.shallow_copy(),
                        node.get_locus());

            node.replace(simd_func);

            pragma_line.diagnostic_unused_clauses();
            // Remove #pragma
            Nodecl::Utils::remove_from_enclosing_list(decl);
        }
    }

    void Base::simd_for_handler_pre(TL::PragmaCustomStatement) { }
    void Base::simd_for_handler_post(TL::PragmaCustomStatement stmt)
    {
        TL::PragmaCustomLine pragma_line = stmt.get_pragma_line();
        // Skipping AST_LIST_NODE
        Nodecl::NodeclBase statements = stmt.get_statements();

        if (_simd_enabled)
        {
            /*
            ERROR_CONDITION(!statements.is<Nodecl::List>(),
                    "'pragma omp simd' Expecting a AST_LIST_NODE (1)", 0);
            Nodecl::List ast_list_node = statements.as<Nodecl::List>();
            ERROR_CONDITION(ast_list_node.size() != 1,
                    "AST_LIST_NODE after '#pragma omp simd' must be equal to 1 (1)", 0);

            // Skipping NODECL_CONTEXT
            Nodecl::NodeclBase context = ast_list_node.front();
            ERROR_CONDITION(!context.is<Nodecl::Context>(),
                    "'pragma omp simd' Expecting a NODECL_CONTEXT", 0);

            // Skipping AST_LIST_NODE
            Nodecl::NodeclBase in_context = context.as<Nodecl::Context>().get_in_context();
            ERROR_CONDITION(!in_context.is<Nodecl::List>(),
                    "'pragma omp simd' Expecting a AST_LIST_NODE (2)", 0);
            Nodecl::List ast_list_node2 = in_context.as<Nodecl::List>();
            ERROR_CONDITION(ast_list_node2.size() != 1,
                    "AST_LIST_NODE after '#pragma omp simd' must be equal to 1 (2)", 0);

            Nodecl::NodeclBase node = ast_list_node2.front();
            ERROR_CONDITION(!node.is<Nodecl::ForStatement>(),
                    "Unexpected node %s. Expecting a ForStatement after '#pragma omp simd'",
                    ast_print_node_type(node.get_kind()));

            // Vectorize for
            Nodecl::NodeclBase epilog =
                _vectorizer.vectorize(node.as<Nodecl::ForStatement>(),
                        "smp", 16, NULL);

            // Add epilog
            if (!epilog.is_null())
            {
                //node.append_sibling(epilog);
            }

            // for_handler_post
            PragmaCustomLine pragma_line = stmt.get_pragma_line();
            bool barrier_at_end = !pragma_line.get_clause("nowait").is_defined();

            Nodecl::NodeclBase code = loop_handler_post(
                    stmt, node, barrier_at_end, false);

            // Removing #pragma
            pragma_line.diagnostic_unused_clauses();
            stmt.replace(code);
            */
        }
        else
        {
            // Remove #pragma
            pragma_line.diagnostic_unused_clauses();
            stmt.replace(statements);
        }
    }

    void Base::parallel_simd_for_handler_pre(TL::PragmaCustomStatement) { }
    void Base::parallel_simd_for_handler_post(TL::PragmaCustomStatement stmt)
    {
        TL::PragmaCustomLine pragma_line = stmt.get_pragma_line();
        pragma_line.diagnostic_unused_clauses();
        // FIXME - What is supposed to happen here?
    }

    void Base::sections_handler_pre(TL::PragmaCustomStatement) { }
    void Base::sections_handler_post(TL::PragmaCustomStatement directive)
    {
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        bool barrier_at_end = !pragma_line.get_clause("nowait").is_defined();

        Nodecl::NodeclBase sections_code = sections_handler_common(directive,
                directive.get_statements(),
                barrier_at_end,
                /* is_combined_worksharing */ false);
        pragma_line.diagnostic_unused_clauses();
        directive.replace(sections_code);
    }

    Nodecl::NodeclBase Base::honour_if_clause(
            TL::PragmaCustomLine pragma_line,
            TL::PragmaCustomStatement directive,
            Nodecl::NodeclBase openmp_construct)
    {
        PragmaCustomClause if_clause = pragma_line.get_clause("if");

        if (if_clause.is_defined())
        {
            TL::ObjectList<Nodecl::NodeclBase> expr_list = if_clause.get_arguments_as_expressions(directive);
            if (expr_list.size() != 1)
            {
                warn_printf("%s: warning: ignoring invalid 'if' clause in 'task' construct\n",
                        directive.get_locus_str().c_str());
            }
            else
            {
                openmp_construct = Nodecl::IfElseStatement::make(
                        expr_list[0],
                        Nodecl::List::make(openmp_construct),
                        directive.get_statements().shallow_copy(),
                        directive.get_locus());
            }
        }

        return openmp_construct;
    }

    void Base::parallel_sections_handler_pre(TL::PragmaCustomStatement) { }
    void Base::parallel_sections_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment_for_combined_worksharings(ds, pragma_line);

        Nodecl::NodeclBase sections_code = sections_handler_common(directive,
                directive.get_statements(),
                /* barrier_at_end */ false,
                /* is_combined_worksharing */ true);

        Nodecl::NodeclBase num_threads;
        PragmaCustomClause clause = pragma_line.get_clause("num_threads");
        if (clause.is_defined())
        {
            ObjectList<Nodecl::NodeclBase> args = clause.get_arguments_as_expressions();

            // Let core check this for us
            ERROR_CONDITION (args.size() != 1, "num_threads wrong clause", 0);

            num_threads = args[0];
        }

        // Set implicit flushes at the entry and exit of the combined worksharing
        execution_environment.append(
                Nodecl::OpenMP::FlushAtEntry::make(
                    directive.get_locus())
        );
        execution_environment.append(
                Nodecl::OpenMP::FlushAtExit::make(
                    directive.get_locus())
        );

        Nodecl::NodeclBase parallel_code
            = Nodecl::OpenMP::Parallel::make(
                execution_environment,
                num_threads,
                sections_code,
                directive.get_locus());

        parallel_code = honour_if_clause(pragma_line, directive, parallel_code);

        pragma_line.diagnostic_unused_clauses();
        directive.replace(parallel_code);
    }

    void Base::parallel_for_handler_pre(TL::PragmaCustomStatement) { }

    void Base::parallel_for_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();
        Nodecl::List execution_environment = this->make_execution_environment_for_combined_worksharings(ds, pragma_line);

        Nodecl::NodeclBase statement = directive.get_statements();
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();
        ERROR_CONDITION(!statement.is<Nodecl::Context>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::Context>().get_in_context();
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();

        Nodecl::NodeclBase num_threads;
        PragmaCustomClause clause = pragma_line.get_clause("num_threads");
        if (clause.is_defined())
        {
            ObjectList<Nodecl::NodeclBase> args = clause.get_arguments_as_expressions();

            // Let core check this for us
            ERROR_CONDITION (args.size() != 1, "num_threads wrong clause", 0);

            num_threads = args[0];
        }

        // Set implicit flushes at the entry and exit of the combined worksharing
        execution_environment.append(
                Nodecl::OpenMP::FlushAtEntry::make(
                    directive.get_locus())
        );
        execution_environment.append(
                Nodecl::OpenMP::FlushAtExit::make(
                    directive.get_locus())
        );

        Nodecl::NodeclBase code = loop_handler_post(directive, statement, /* barrier_at_end */ false, /* is_combined_worksharing */ true);

        Nodecl::NodeclBase parallel_code
            = Nodecl::OpenMP::Parallel::make(
                    execution_environment,
                    num_threads,
                    code,
                    directive.get_locus());

        parallel_code = honour_if_clause(pragma_line, directive, parallel_code);

        pragma_line.diagnostic_unused_clauses();
        directive.replace(parallel_code);
    }

    void Base::threadprivate_handler_pre(TL::PragmaCustomDirective) { }
    void Base::threadprivate_handler_post(TL::PragmaCustomDirective directive)
    {
        TL::PragmaCustomLine pragma_line = directive.get_pragma_line();
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);

        TL::ObjectList<Symbol> threadprivate_symbols;
        ds.get_all_symbols(OpenMP::DS_THREADPRIVATE, threadprivate_symbols);

        for (TL::ObjectList<Symbol>::iterator it = threadprivate_symbols.begin();
                it != threadprivate_symbols.end();
                it++)
        {
            TL::Symbol &sym(*it);

            // Mark as __thread
            scope_entry_t* entry = sym.get_internal_symbol();

            entry->entity_specs.is_thread = 1;
        }

        pragma_line.diagnostic_unused_clauses();
        Nodecl::Utils::remove_from_enclosing_list(directive);
    }

    void Base::declare_reduction_handler_pre(TL::PragmaCustomDirective) { }
    void Base::declare_reduction_handler_post(TL::PragmaCustomDirective directive)
    {
        TL::PragmaCustomLine pragma_line = directive.get_pragma_line();
        // Remove
        pragma_line.diagnostic_unused_clauses();
        Nodecl::Utils::remove_from_enclosing_list(directive);
    }

    struct SymbolBuilder : TL::Functor<Nodecl::NodeclBase, Symbol>
    {
        private:
            const locus_t* _locus;

        public:
            SymbolBuilder(const locus_t* locus)
                : _locus(locus)
            {
            }

            virtual Nodecl::NodeclBase do_(ArgType arg) const
            {
                return Nodecl::Symbol::make(arg, _locus);
            }
    };

    struct ReductionSymbolBuilder : TL::Functor<Nodecl::NodeclBase, ReductionSymbol>
    {
        private:
            const locus_t* _locus;

        public:
            ReductionSymbolBuilder(const locus_t* locus)
                : _locus(locus)
            {
            }

            virtual Nodecl::NodeclBase do_(ArgType arg) const
            {
                return Nodecl::OpenMP::ReductionItem::make(
                        /* reductor */ Nodecl::Symbol::make(arg.get_reduction()->get_symbol(), _locus),
                        /* reduced symbol */ Nodecl::Symbol::make(arg.get_symbol(), _locus),
                        _locus);
            }
    };

    template <typename T>
    static void make_data_sharing_list(
            OpenMP::DataSharingEnvironment &data_sharing_env,
            OpenMP::DataSharingAttribute data_attr,
            const locus_t* locus,
            ObjectList<Nodecl::NodeclBase>& result_list)
    {
        TL::ObjectList<Symbol> symbols;
        data_sharing_env.get_all_symbols(data_attr, symbols);

        // Get the symbols in dependences
        TL::ObjectList<DependencyItem> all_dependences;
        data_sharing_env.get_all_dependences(all_dependences);
        TL::ObjectList<DataReference> dependences_in_symbols
            = all_dependences.map(functor(&DependencyItem::get_dependency_expression));
        TL::ObjectList<Symbol> symbols_in_dependences
            = dependences_in_symbols.map(functor(&DataReference::get_base_symbol));

        // Remove all symbols appearing in dependences
        symbols = symbols.filter(not_in_set(symbols_in_dependences));

        if (!symbols.empty())
        {
            TL::ObjectList<Nodecl::NodeclBase> nodecl_symbols = symbols.map(SymbolBuilder(locus));

            result_list.append(T::make(Nodecl::List::make(nodecl_symbols), locus));
        }
    }

    Nodecl::List Base::make_execution_environment_for_combined_worksharings(OpenMP::DataSharingEnvironment &data_sharing_env, PragmaCustomLine pragma_line)
    {
        const locus_t* locus = pragma_line.get_locus();

        TL::ObjectList<Nodecl::NodeclBase> result_list;

        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_SHARED,
                locus,
                result_list);
        // Everything should go transparent here
        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_PRIVATE,
                locus,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_FIRSTPRIVATE,
                locus,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_LASTPRIVATE,
                locus,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_FIRSTLASTPRIVATE,
                locus,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_AUTO,
                locus,
                result_list);

        TL::ObjectList<ReductionSymbol> reductions;
        data_sharing_env.get_all_reduction_symbols(reductions);
        TL::ObjectList<Symbol> reduction_symbols = reductions.map(functor(&ReductionSymbol::get_symbol));
        if (!reduction_symbols.empty())
        {
            TL::ObjectList<Nodecl::NodeclBase> nodecl_symbols =
                reduction_symbols.map(SymbolBuilder(locus));

            result_list.append(Nodecl::OpenMP::Shared::make(Nodecl::List::make(nodecl_symbols),
                        locus));
        }


        // Build the tree which contains the target information
        TargetInfo target_info = data_sharing_env.get_target_info();

        TL::ObjectList<Nodecl::NodeclBase> devices;
        TL::ObjectList<Nodecl::NodeclBase> target_items;

        ObjectList<Nodecl::NodeclBase> ndrange_exprs = target_info.get_shallow_copy_of_ndrange();
        if (!ndrange_exprs.empty())
        {
            target_items.append(
                    Nodecl::OpenMP::NDRange::make(
                        Nodecl::List::make(ndrange_exprs),
                        Nodecl::Symbol::make(target_info.get_target_symbol(), locus),
                        locus));
        }

        ObjectList<Nodecl::NodeclBase> onto_exprs = target_info.get_shallow_copy_of_onto();
        if (!onto_exprs.empty())
        {
            target_items.append(
                    Nodecl::OpenMP::Onto::make(
                        Nodecl::List::make(onto_exprs),
                        Nodecl::Symbol::make(target_info.get_target_symbol(), locus),
                        locus));
        }

        ObjectList<std::string> device_list = target_info.get_device_list();
        for (TL::ObjectList<std::string>::iterator it = device_list.begin(); it != device_list.end(); ++it)
        {
            devices.append(Nodecl::Text::make(*it, locus));
        }

        result_list.append(
                Nodecl::OpenMP::Target::make(
                    Nodecl::List::make(devices),
                    Nodecl::List::make(target_items),
                    locus));


        // FIXME - Dependences and copies for combined worksharings???
        //
        // TL::ObjectList<OpenMP::DependencyItem> dependences;
        // data_sharing_env.get_all_dependences(dependences);

        // make_dependency_list<Nodecl::OpenMP::DepIn>(
        //         dependences,
        //         OpenMP::DEP_DIR_IN,
        //         pragma_line.get_locus(),
        //         result_list);

        // make_dependency_list<Nodecl::OpenMP::DepOut>(
        //         dependences,
        //         OpenMP::DEP_DIR_OUT,
        //         pragma_line.get_locus(),
        //         result_list);

        // make_dependency_list<Nodecl::OpenMP::DepInout>(
        //         dependences, OpenMP::DEP_DIR_INOUT,
        //         pragma_line.get_locus(),
        //         result_list);

        // TL::ObjectList<OpenMP::CopyItem> copies;
        // data_sharing_env.get_all_copies(copies);

        // make_copy_list<Nodecl::OpenMP::CopyIn>(
        //         copies,
        //         OpenMP::COPY_DIR_IN,
        //         pragma_line.get_locus(),
        //         result_list);

        // make_copy_list<Nodecl::OpenMP::CopyOut>(
        //         copies,
        //         OpenMP::COPY_DIR_IN,
        //         pragma_line.get_locus(),
        //         result_list);

        // make_copy_list<Nodecl::OpenMP::CopyInout>(
        //         copies,
        //         OpenMP::COPY_DIR_INOUT,
        //         pragma_line.get_locus(),
        //         result_list);

        return Nodecl::List::make(result_list);
    }

    Nodecl::List Base::make_execution_environment(OpenMP::DataSharingEnvironment &data_sharing_env, PragmaCustomLine pragma_line)
    {
        const locus_t* locus = pragma_line.get_locus();

        TL::ObjectList<Nodecl::NodeclBase> result_list;

        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_SHARED,
                locus,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Private>(
                data_sharing_env, OpenMP::DS_PRIVATE,
                locus,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Firstprivate>(
                data_sharing_env, OpenMP::DS_FIRSTPRIVATE,
                locus,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Lastprivate>(
                data_sharing_env, OpenMP::DS_LASTPRIVATE,
                locus,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::FirstLastprivate>(
                data_sharing_env, OpenMP::DS_FIRSTLASTPRIVATE,
                locus,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Auto>(
                data_sharing_env, OpenMP::DS_AUTO,
                locus,
                result_list);

        TL::ObjectList<ReductionSymbol> reductions;
        data_sharing_env.get_all_reduction_symbols(reductions);
        if (!reductions.empty())
        {
            TL::ObjectList<Nodecl::NodeclBase> reduction_nodes = reductions.map(ReductionSymbolBuilder(locus));

            result_list.append(
                    Nodecl::OpenMP::Reduction::make(Nodecl::List::make(reduction_nodes),
                        locus)
                    );
        }

        TL::ObjectList<OpenMP::DependencyItem> dependences;
        data_sharing_env.get_all_dependences(dependences);

        make_dependency_list<Nodecl::OpenMP::DepIn>(
                dependences,
                OpenMP::DEP_DIR_IN,
                locus,
                result_list);

        make_dependency_list<Nodecl::OpenMP::DepOut>(
                dependences,
                OpenMP::DEP_DIR_OUT,
                locus,
                result_list);

        make_dependency_list<Nodecl::OpenMP::DepInout>(
                dependences, OpenMP::DEP_DIR_INOUT,
                locus,
                result_list);

        make_dependency_list<Nodecl::OpenMP::Concurrent>(
                dependences, OpenMP::DEP_CONCURRENT,
                locus,
                result_list);

        make_dependency_list<Nodecl::OpenMP::Commutative>(
                dependences, OpenMP::DEP_COMMUTATIVE,
                locus,
                result_list);

        // Build the tree which contains the target information
        TargetInfo target_info = data_sharing_env.get_target_info();

        TL::ObjectList<Nodecl::NodeclBase> devices;
        TL::ObjectList<Nodecl::NodeclBase> target_items;

        ObjectList<std::string> device_list = target_info.get_device_list();
        for (TL::ObjectList<std::string>::iterator it = device_list.begin(); it != device_list.end(); ++it)
        {
            devices.append(Nodecl::Text::make(*it, locus));
        }

        ObjectList<CopyItem> copy_in = target_info.get_copy_in();
        make_copy_list<Nodecl::OpenMP::CopyIn>(
                copy_in,
                OpenMP::COPY_DIR_IN,
                locus,
                target_items);

        ObjectList<CopyItem> copy_out = target_info.get_copy_out();
        make_copy_list<Nodecl::OpenMP::CopyOut>(
                copy_out,
                OpenMP::COPY_DIR_OUT,
                locus,
                target_items);

        ObjectList<CopyItem> copy_inout = target_info.get_copy_inout();
        make_copy_list<Nodecl::OpenMP::CopyInout>(
                copy_inout,
                OpenMP::COPY_DIR_INOUT,
                locus,
                target_items);

        ObjectList<Nodecl::NodeclBase> ndrange_exprs = target_info.get_shallow_copy_of_ndrange();
        if (!ndrange_exprs.empty())
        {
            target_items.append(
                    Nodecl::OpenMP::NDRange::make(
                        Nodecl::List::make(ndrange_exprs),
                        //Build symbol from enclosing function, since it's the one which we use to identify inline tasks
                        Nodecl::Symbol::make(Nodecl::Utils::get_enclosing_function(pragma_line), locus),
                        locus));
        }

        ObjectList<Nodecl::NodeclBase> onto_exprs = target_info.get_shallow_copy_of_onto();
        if (!onto_exprs.empty())
        {
            target_items.append(
                    Nodecl::OpenMP::Onto::make(
                        Nodecl::List::make(onto_exprs),
                        //Build symbol from enclosing function, since it's the one which we use to identify inline tasks
                        Nodecl::Symbol::make(Nodecl::Utils::get_enclosing_function(pragma_line), locus),
                        locus));
        }

        std::string file = target_info.get_file();
        if (!file.empty())
        {
            target_items.append(
                    Nodecl::OpenMP::File::make(
                        Nodecl::Text::make(file),
                        //Build symbol from enclosing function, since it's the one which we use to identify inline tasks
                        Nodecl::Symbol::make(Nodecl::Utils::get_enclosing_function(pragma_line), locus),
                        locus));
        }

        std::string name = target_info.get_name();
        if (!name.empty())
        {
            target_items.append(
                    Nodecl::OpenMP::Name::make(
                        Nodecl::Text::make(name),
                        Nodecl::Symbol::make(target_info.get_target_symbol(), locus),
                        locus));
        }

        result_list.append(
                Nodecl::OpenMP::Target::make(
                    Nodecl::List::make(devices),
                    Nodecl::List::make(target_items),
                    locus));

        return Nodecl::List::make(result_list);
    }

    } }

EXPORT_PHASE(TL::OpenMP::Base)
