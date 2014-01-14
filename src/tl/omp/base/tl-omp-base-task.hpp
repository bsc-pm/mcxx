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

#ifndef TL_OMP_BASE_TASK_HPP
#define TL_OMP_BASE_TASK_HPP

#include"tl-nodecl-visitor.hpp"
#include "tl-omp-core.hpp"

namespace TL { namespace OpenMP {

    // Some bits have been shamelessly copied from tl-lower-task-call.cpp
    typedef std::map<TL::Symbol, Nodecl::NodeclBase> sym_to_argument_expr_t;

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
                    const std::map<Nodecl::NodeclBase, std::set<TL::Symbol> >& enclosing_stmt_to_return_vars_map);

            virtual void visit(const Nodecl::FunctionCall& call);

            void build_all_needed_task_expressions();
        private:

            Nodecl::NodeclBase make_exec_environment(const Nodecl::FunctionCall &call,
                    TL::Symbol function_sym,
                    FunctionTaskInfo& function_task_info);

            template < typename T>
                void get_assignment_expressions(
                        Nodecl::NodeclBase expr,
                        Nodecl::NodeclBase& lhs_expr,
                        Nodecl::NodeclBase& rhs_expr);

            void fill_map_parameters_to_arguments(
                    TL::Symbol function,
                    Nodecl::List arguments,
                    sym_to_argument_expr_t& param_to_arg_expr);

            Nodecl::NodeclBase instantiate_exec_env(Nodecl::NodeclBase exec_env, Nodecl::FunctionCall call);

            Nodecl::NodeclBase update_join_task(const Nodecl::NodeclBase& enclosing_stmt);

            Nodecl::OpenMP::Task generate_join_task(const Nodecl::NodeclBase& enclosing_stmt);

            Nodecl::List generate_sequential_code(
                    const Nodecl::NodeclBase& enclosing_stmt,
                    const TL::ObjectList<Nodecl::NodeclBase>& task_calls,
                    const Nodecl::NodeclBase& join_task);
    };

    class TransformNonVoidFunctionCalls : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            int _counter;
            Nodecl::NodeclBase _enclosing_stmt;

            RefPtr<FunctionTaskSet> _function_task_set;

            std::map<TL::Symbol, TL::Symbol> _transformed_task_map;

            TL::ObjectList<Nodecl::FunctionCall> _ignore_these_function_calls;
            std::map<Nodecl::NodeclBase, Nodecl::NodeclBase> _funct_call_to_enclosing_stmt_map;
            std::map<Nodecl::NodeclBase, std::set<TL::Symbol> > _enclosing_stmt_to_return_vars_map;

        public:

            TransformNonVoidFunctionCalls(RefPtr<FunctionTaskSet> function_task_Set);

            virtual void visit(const Nodecl::ObjectInit& object_init);
            virtual void visit(const Nodecl::ReturnStatement& return_stmt);
            virtual void visit(const Nodecl::FunctionCall& func_call);

            std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& get_function_call_to_enclosing_stmt_map();
            std::map<Nodecl::NodeclBase, std::set<TL::Symbol> >& get_enclosing_stmt_to_return_variables_map();

            void remove_nonvoid_function_tasks_from_function_task_set();

        private:

            Nodecl::NodeclBase get_enclosing_stmt(Nodecl::NodeclBase function_call);

            void transform_object_init(Nodecl::NodeclBase enclosing_stmt, const locus_t* locus);

            void transform_return_statement(Nodecl::NodeclBase enclosing_stmt, const locus_t* locus);

            void add_the_new_task_to_the_function_task_set(
                    TL::Symbol function_called,
                    TL::Symbol new_function,
                    const FunctionTaskInfo& original_function_task_info,
                    Nodecl::NodeclBase func_call);

            Nodecl::NodeclBase create_a_wrapper_to_the_function_called(
                    TL::Symbol original_function,
                    TL::Symbol new_function);
    };
}}
#endif //TL_OMP_BASE_TASK_HPP
