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
#include "tl-source.hpp"
#include "tl-omp-reduction.hpp"
#include "tl-builtin.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-diagnostic.h"

namespace TL { namespace OpenMP {

    struct LocalSymbolsInDependences
    {
        private:
            TL::Symbol _function;
            TL::ObjectList<TL::Symbol> &_seen_local_symbols;

            bool is_local_symbol(TL::Symbol sym) const
            {
                return sym.get_scope().is_block_scope()
                    && (sym.get_scope().get_related_symbol() == _function)
                    && !sym.is_parameter_of(_function)
                    && (!IS_CXX_LANGUAGE || (sym.get_name() != "this"))
                    // In Fortran saved expressions are only created for
                    // explicit size arrays with nonconstant size dependent
                    // of dummy arguments (i.e. the equivalent of a VLA
                    // parameter in the Fortran world)
                    && (!IS_FORTRAN_LANGUAGE || !sym.is_saved_expression());
            }

        public:
            LocalSymbolsInDependences(
                    TL::Symbol function,
                    TL::ObjectList<TL::Symbol>& seen_local_symbols)
                : _function(function),
                _seen_local_symbols(seen_local_symbols)
        {
        }

            bool operator()(Nodecl::NodeclBase expr) const
            {
                TL::ObjectList<TL::Symbol> local_symbols;
                local_symbols.insert(
                        Nodecl::Utils::get_all_symbols(expr)
                        .filter(std::bind(
                                &LocalSymbolsInDependences::is_local_symbol,
                                this,
                                std::placeholders::_1))
                        );

                for (TL::ObjectList<TL::Symbol>::iterator it = local_symbols.begin();
                        it != local_symbols.end();
                        it++)
                {
                    if (!_seen_local_symbols.contains(*it))
                    {
                        error_printf_at(
                                expr.get_locus(),
                                "cannot reference local variable '%s' in dependence\n",
                                it->get_name().c_str());
                    }
                }

                _seen_local_symbols.insert(local_symbols);

                return !local_symbols.empty();
            }
    };

    struct IsUselessDependence
    {

        private:
            TL::OpenMP::DependencyDirection _direction;
        public:
            IsUselessDependence(TL::OpenMP::DependencyDirection &direction)
                : _direction(direction) { }

            // This function returns whether the current expression is a useless dependence or not
            bool operator()(Nodecl::NodeclBase expr) const
            {
                if (expr.is<Nodecl::Symbol>())
                {
                    Symbol sym = expr.get_symbol();
                    if (sym.is_parameter()
                            && !sym.get_type().is_any_reference())
                    {
                        // Copy semantics of values in C/C++ lead to this fact
                        // If the dependence is output (or inout) this should
                        // be regarded as an error
                        if ((_direction & TL::OpenMP::DEP_DIR_OUT) == TL::OpenMP::DEP_DIR_OUT
                                || (_direction & TL::OpenMP::DEP_OMPSS_WEAK_OUT) == TL::OpenMP::DEP_OMPSS_WEAK_OUT)
                        {
                            error_printf_at(
                                    expr.get_locus(),
                                    "skipping useless dependence %s(%s) since it only names a parameter."
                                    "The value of a parameter is never copied out of a function "
                                    "so it cannot generate an output dependence\n",
                                    get_dependency_direction_name(_direction).c_str(),
                                    expr.prettyprint().c_str());
                            return true;
                        }
                        else
                        {
                            warn_printf_at(
                                    expr.get_locus(),
                                    "skipping useless dependence %s(%s) since it only names a parameter."
                                    "The value of a parameter is always copied in and will never define such dependence\n",
                                    get_dependency_direction_name(_direction).c_str(),
                                    expr.prettyprint().c_str());
                            return true;
                        }
                    }
                }
                return false;
            }
    };

    static void dependence_list_check(
            ObjectList<Nodecl::NodeclBase>& expression_list,
            TL::OpenMP::DependencyDirection direction,
            TL::Symbol function)
    {
        IsUselessDependence is_useless_dependence(direction);
        // Remove useless dependences
        expression_list.erase(
                std::remove_if(expression_list.begin(), expression_list.end(), is_useless_dependence),
                expression_list.end());

        TL::ObjectList<TL::Symbol> seen_local_symbols;
        LocalSymbolsInDependences there_are_local_symbols_in_dependence(function, seen_local_symbols);
        // Remove wrong dependences because they reference local symbols
        expression_list.erase(
                std::remove_if(expression_list.begin(), expression_list.end(),
                    there_are_local_symbols_in_dependence),
                expression_list.end());
    }

    // This visitor constructs a set with all the symbols of the tree
    class GetAllSymbolsVisitor : public Nodecl::NodeclVisitor<void>
    {
        private:
            std::set<TL::Symbol> _symbols;

        public:
            GetAllSymbolsVisitor() {}

            // Any node
            void unhandled_node(const Nodecl::NodeclBase& node)
            {
                if (node.get_symbol().is_valid())
                {
                    _symbols.insert(node.get_symbol());
                }
                if (node.get_type().is_valid())
                {
                    walk_types(node.get_type());
                }

                Nodecl::NodeclBase::Children children = node.children();
                for (Nodecl::NodeclBase::Children::iterator it = children.begin();
                        it != children.end();
                        it++)
                {
                    walk(*it);
                }
            }

            const std::set<TL::Symbol>& get_all_symbols()
            {
                return _symbols;
            }

            void walk_types(TL::Type t)
            {
                if (t.is_array())
                {
                    walk(t.array_get_size());
                }
                else if (t.is_pointer())
                {
                    walk_types(t.points_to());
                }
                else if (t.is_any_reference())
                {
                    walk_types(t.references_to());
                }
            }
    };



    // We should update the clauses of the function task because They may be
    // written in terms of the function declaration. Example:
    //
    //  #pragma omp task input([10]a)
    //  void foo(int* a);
    //
    //  void foo(int* c) { }
    //
    static ObjectList<Nodecl::NodeclBase> update_clauses(const ObjectList<Nodecl::NodeclBase>& clauses,
            Symbol function_symbol)
    {
        ObjectList<Nodecl::NodeclBase> updated_clauses;
        ObjectList<Symbol> function_parameters = function_symbol.get_function_parameters();

        for (ObjectList<Nodecl::NodeclBase>::const_iterator it = clauses.begin();
                it != clauses.end();
                ++it)
        {
            Nodecl::NodeclBase current_clause = *it;

            GetAllSymbolsVisitor visitor;
            visitor.walk(current_clause);

            Nodecl::Utils::SimpleSymbolMap symbol_map;
            const std::set<TL::Symbol>& used_symbols = visitor.get_all_symbols();
            for (std::set<TL::Symbol>::const_iterator it2 = used_symbols.begin();
                    it2 != used_symbols.end();
                    ++it2)
            {
                TL::Symbol current_symbol = *it2;
                if (current_symbol.is_parameter()
                        && current_symbol.is_parameter_of(function_symbol))
                {
                    int param_position = current_symbol.get_parameter_position();
                    symbol_map.add_map(current_symbol, function_parameters[param_position]);
                }
            }

            Nodecl::NodeclBase update_current_clause = Nodecl::Utils::deep_copy(
                    current_clause,
                    function_symbol.get_scope(),
                    symbol_map);

            updated_clauses.insert(update_current_clause);

        }
        return updated_clauses;
    }

    struct FunctionTaskDependencyGenerator
    {
        private:
            TL::OpenMP::DependencyDirection _direction;
        public:
            FunctionTaskDependencyGenerator(TL::OpenMP::DependencyDirection direction)
                : _direction(direction)
            {
            }

            TL::OmpSs::FunctionTaskDependency operator()(Nodecl::NodeclBase nodecl) const
            {
                DataReference expr(nodecl);

                if (!expr.is_valid())
                {
                    std::string dep_str = get_dependency_direction_name(_direction);
                    warn_printf_at(
                            nodecl.get_locus(),
                            "invalid dependency expression '%s(%s)', skipping\n",
                            dep_str.c_str(),
                            expr.prettyprint().c_str());
                }

                return TL::OmpSs::FunctionTaskDependency(expr, _direction);
            }
    };

    struct FunctionCopyItemGenerator
    {
        private:
            TL::OmpSs::CopyDirection _copy_direction;

        public:
            FunctionCopyItemGenerator(TL::OmpSs::CopyDirection copy_direction)
                : _copy_direction(copy_direction)
            {
            }

            TL::OmpSs::CopyItem operator()(Nodecl::NodeclBase node) const
            {
                DataReference data_ref(node);

                return TL::OmpSs::CopyItem(data_ref, _copy_direction);
            }
    };


    void Core::task_function_handler_pre(TL::PragmaCustomDeclaration construct)
    {
        TL::Scope parsing_scope = construct.get_context_of_parameters().retrieve_context();

        TL::PragmaCustomLine pragma_line = construct.get_pragma_line();

        Symbol function_sym = construct.get_symbol();

        if (!function_sym.is_function())
        {
            warn_printf_at(construct.get_locus(),
                    "'#pragma omp task' cannot be applied to this declaration "
                    "since it does not declare a function, skipping\n");
            return;
        }

        ObjectList<Nodecl::NodeclBase> input_arguments;
        PragmaCustomClause input_clause = pragma_line.get_clause("in", /* deprecated name */ "input");
        if (input_clause.is_defined())
        {
            input_arguments = parse_dependences_ompss_clause(input_clause, parsing_scope);
            input_arguments = update_clauses(input_arguments, function_sym);
        }

        ObjectList<Nodecl::NodeclBase> weakinput_arguments;
        PragmaCustomClause weakinput_clause = pragma_line.get_clause("weakin");
        if (weakinput_clause.is_defined())
        {
            weakinput_arguments = parse_dependences_ompss_clause(weakinput_clause, parsing_scope);
            weakinput_arguments = update_clauses(weakinput_arguments, function_sym);
        }

        TL::ObjectList<std::string> input_private_names;
        input_private_names.append("inprivate");
        PragmaCustomClause input_private_clause = pragma_line.get_clause(input_private_names);
        ObjectList<Nodecl::NodeclBase> input_private_arguments;
        if (input_private_clause.is_defined())
        {
            input_private_arguments = parse_dependences_ompss_clause(input_private_clause, parsing_scope);
            input_private_arguments = update_clauses(input_private_arguments, function_sym);
        }

        PragmaCustomClause output_clause = pragma_line.get_clause("out",
                /* deprecated name */ "output");
        ObjectList<Nodecl::NodeclBase> output_arguments;
        if (output_clause.is_defined())
        {
            output_arguments = parse_dependences_ompss_clause(output_clause, parsing_scope);
            output_arguments = update_clauses(output_arguments, function_sym);
        }

        ObjectList<Nodecl::NodeclBase> weakoutput_arguments;
        PragmaCustomClause weakoutput_clause = pragma_line.get_clause("weakout");
        if (weakoutput_clause.is_defined())
        {
            weakoutput_arguments = parse_dependences_ompss_clause(weakoutput_clause, parsing_scope);
            weakoutput_arguments = update_clauses(weakoutput_arguments, function_sym);
        }

        PragmaCustomClause inout_clause = pragma_line.get_clause("inout");
        ObjectList<Nodecl::NodeclBase> inout_arguments;
        if (inout_clause.is_defined())
        {
            inout_arguments = parse_dependences_ompss_clause(inout_clause, parsing_scope);
            inout_arguments = update_clauses(inout_arguments, function_sym);
        }

        ObjectList<Nodecl::NodeclBase> weakinout_arguments;
        PragmaCustomClause weakinout_clause = pragma_line.get_clause("weakinout");
        if (weakinout_clause.is_defined())
        {
            weakinout_arguments = parse_dependences_ompss_clause(weakinout_clause, parsing_scope);
            weakinout_arguments = update_clauses(weakinout_arguments, function_sym);
        }

        {
            // OpenMP standard clauses
            TL::ObjectList<Nodecl::NodeclBase> std_in, std_out, std_inout;
            parse_dependences_openmp_clause(
                    parsing_scope,
                    pragma_line.get_clause("depend"),
                    std_in, std_out, std_inout,
                    pragma_line.get_locus());
            input_arguments.append(std_in);
            output_arguments.append(std_out);
            inout_arguments.append(std_inout);
        }

        PragmaCustomClause concurrent_clause = pragma_line.get_clause("concurrent");
        ObjectList<Nodecl::NodeclBase> concurrent_arguments;
        if (concurrent_clause.is_defined())
        {
            concurrent_arguments = parse_dependences_ompss_clause(concurrent_clause, parsing_scope);
            concurrent_arguments = update_clauses(concurrent_arguments, function_sym);
        }

        PragmaCustomClause commutative_clause = pragma_line.get_clause("commutative");
        ObjectList<Nodecl::NodeclBase> commutative_arguments;
        if (commutative_clause.is_defined())
        {
            commutative_arguments = parse_dependences_ompss_clause(commutative_clause, parsing_scope);
            commutative_arguments = update_clauses(commutative_arguments, function_sym);
        }

        TL::ObjectList<TL::Symbol> shared_vars;
        // Free variables of nested functions
        if (function_sym.is_nested_function())
        {
            Nodecl::NodeclBase function_code = function_sym.get_function_code();
            if (function_code.is_null())
            {
                warn_printf_at(construct.get_locus(),
                        "nested function '%s' has not been defined\n",
                        function_sym.get_name().c_str());
            }
            else
            {
                struct FreeVariablesOfNestedVisitor : public Nodecl::ExhaustiveVisitor<void>
                {
                    TL::Symbol _current_function;
                    TL::ObjectList<TL::Symbol> &_freevars;

                    FreeVariablesOfNestedVisitor(TL::Symbol current_function,
                            TL::ObjectList<TL::Symbol> &freevars)
                        : _current_function(current_function),
                        _freevars(freevars)
                    {
                    }

                    virtual void visit(const Nodecl::Symbol& n)
                    {
                        TL::Symbol sym = n.get_symbol();
                        if (sym.is_variable()
                                && sym.get_scope().is_block_scope()
                                && sym.get_scope().get_related_symbol() != _current_function)
                        {
                            _freevars.insert(sym);
                        }
                    }
                };

                TL::ObjectList<TL::Symbol> freevars;
                FreeVariablesOfNestedVisitor v(function_sym, freevars);

                v.walk(function_code);

                // Now add freevars as ""shared""
                shared_vars.insert(freevars);
            }
        }

        Type function_type = function_sym.get_type();

        bool has_ellipsis = false;
        function_type.parameters(has_ellipsis);

        if (has_ellipsis)
        {
            warn_printf_at(construct.get_locus(),
                    "'#pragma omp task' cannot be applied to functions "
                    "declarations with ellipsis, skipping\n");
            return;
        }

        if (!function_type.returns().is_void()
                && (IS_FORTRAN_LANGUAGE || !_enable_nonvoid_function_tasks))
        {
            error_printf_at(construct.get_locus(),
                    "non-void tasks are not supported, skipping\n");
            return;
        }

        ObjectList<TL::OmpSs::FunctionTaskDependency> dependence_list;

        dependence_list_check(input_arguments, DEP_DIR_IN, function_sym);
        dependence_list.append(input_arguments
                .map<TL::OmpSs::FunctionTaskDependency>(FunctionTaskDependencyGenerator(DEP_DIR_IN))
                .filter(&TL::OmpSs::FunctionTaskDependency::is_valid));

        dependence_list_check(weakinput_arguments, DEP_OMPSS_WEAK_IN, function_sym);
        dependence_list.append(weakinput_arguments
                .map<TL::OmpSs::FunctionTaskDependency>(FunctionTaskDependencyGenerator(DEP_OMPSS_WEAK_IN))
                .filter(&TL::OmpSs::FunctionTaskDependency::is_valid));

        dependence_list_check(input_private_arguments, DEP_OMPSS_DIR_IN_PRIVATE, function_sym);
        dependence_list.append(input_private_arguments
                .map<TL::OmpSs::FunctionTaskDependency>(FunctionTaskDependencyGenerator(DEP_OMPSS_DIR_IN_PRIVATE))
                .filter(&TL::OmpSs::FunctionTaskDependency::is_valid));

        dependence_list_check(output_arguments, DEP_DIR_OUT, function_sym);
        dependence_list.append(output_arguments
                .map<TL::OmpSs::FunctionTaskDependency>(FunctionTaskDependencyGenerator(DEP_DIR_OUT))
                .filter(&TL::OmpSs::FunctionTaskDependency::is_valid));

        dependence_list_check(weakoutput_arguments, DEP_OMPSS_WEAK_OUT, function_sym);
        dependence_list.append(weakoutput_arguments
                .map<TL::OmpSs::FunctionTaskDependency>(FunctionTaskDependencyGenerator(DEP_OMPSS_WEAK_OUT))
                .filter(&TL::OmpSs::FunctionTaskDependency::is_valid));

        dependence_list_check(inout_arguments, DEP_DIR_INOUT, function_sym);
        dependence_list.append(inout_arguments
                .map<TL::OmpSs::FunctionTaskDependency>(FunctionTaskDependencyGenerator(DEP_DIR_INOUT))
                .filter(&TL::OmpSs::FunctionTaskDependency::is_valid));

        dependence_list_check(weakinout_arguments, DEP_OMPSS_WEAK_INOUT, function_sym);
        dependence_list.append(weakinout_arguments
                .map<TL::OmpSs::FunctionTaskDependency>(FunctionTaskDependencyGenerator(DEP_OMPSS_WEAK_INOUT))
                .filter(&TL::OmpSs::FunctionTaskDependency::is_valid));

        dependence_list_check(concurrent_arguments, DEP_OMPSS_CONCURRENT, function_sym);
        dependence_list.append(concurrent_arguments
                .map<TL::OmpSs::FunctionTaskDependency>(FunctionTaskDependencyGenerator(DEP_OMPSS_CONCURRENT))
                .filter(&TL::OmpSs::FunctionTaskDependency::is_valid));

        dependence_list_check(commutative_arguments, DEP_OMPSS_COMMUTATIVE, function_sym);
        dependence_list.append(commutative_arguments
                .map<TL::OmpSs::FunctionTaskDependency>(FunctionTaskDependencyGenerator(DEP_OMPSS_COMMUTATIVE))
                .filter(&TL::OmpSs::FunctionTaskDependency::is_valid));

        // Target-style clauses
        if (_target_context.empty())
        {
            // Create an implicit target for this one
            _target_context.push(OmpSs::TargetContext());
            _target_context.top().is_implicit = true;

            ompss_common_target_handler_pre(pragma_line,
                    _target_context.top(),
                    parsing_scope,
                    /* is_pragma_task */ true);

            ompss_handle_implements_clause(
                    _target_context.top(),
                    function_sym,
                    construct.get_locus());

        }
        ERROR_CONDITION(_target_context.empty(), "This cannot be empty", 0);

        TL::OmpSs::FunctionTaskInfo task_info(function_sym, dependence_list);
        task_info.set_shared_closure(shared_vars);

        // Now gather target information
        TL::OmpSs::TargetInfo target_info;
        {
            target_info.set_target_symbol(function_sym);
            OmpSs::TargetContext& target_context = _target_context.top();

            TL::ObjectList<Nodecl::NodeclBase> target_ctx_copy_in = update_clauses(target_context.copy_in, function_sym);
            TL::ObjectList<Nodecl::NodeclBase> target_ctx_copy_out = update_clauses(target_context.copy_out, function_sym);
            TL::ObjectList<Nodecl::NodeclBase> target_ctx_copy_inout = update_clauses(target_context.copy_inout, function_sym);

            if (target_context.copy_deps == OmpSs::TargetContext::COPY_DEPS)
            {
                // Honour copy deps but first remove useless dependences
                target_ctx_copy_in.append(input_arguments);
                target_ctx_copy_out.append(output_arguments);
                target_ctx_copy_inout.append(inout_arguments);

                // Concurrent/Commutative deps with target attribute 'copy_deps'
                // should generate copy_inout information
                target_ctx_copy_inout.append(concurrent_arguments);
                target_ctx_copy_inout.append(commutative_arguments);
            }

            ObjectList<TL::OmpSs::CopyItem> copy_in =
                target_ctx_copy_in.map<TL::OmpSs::CopyItem>(
                    FunctionCopyItemGenerator(TL::OmpSs::COPY_DIR_IN));
            target_info.append_to_copy_in(copy_in);

            ObjectList<TL::OmpSs::CopyItem> copy_out =
                target_ctx_copy_out.map<TL::OmpSs::CopyItem>(
                    FunctionCopyItemGenerator(TL::OmpSs::COPY_DIR_OUT));
            target_info.append_to_copy_out(copy_out);

            ObjectList<TL::OmpSs::CopyItem> copy_inout =
                target_ctx_copy_inout.map<TL::OmpSs::CopyItem>(
                    FunctionCopyItemGenerator(TL::OmpSs::COPY_DIR_INOUT));
            target_info.append_to_copy_inout(copy_inout);

            target_info.set_file(target_context.file);
            target_info.set_name(target_context.name);
            target_info.append_to_ndrange(update_clauses(target_context.ndrange, function_sym));
            target_info.append_to_shmem(update_clauses(target_context.shmem, function_sym));
            target_info.append_to_onto(update_clauses(target_context.onto, function_sym));

            target_info.append_to_device_list(target_context.device_list);

            target_info.set_copy_deps(target_context.copy_deps == OmpSs::TargetContext::COPY_DEPS);
        }

        // Store the target information in the current function task
        task_info.set_target_info(target_info);

        // Support if clause
        PragmaCustomClause if_clause = pragma_line.get_clause("if");
        if (if_clause.is_defined())
        {
            ObjectList<Nodecl::NodeclBase> expr_list = if_clause.get_arguments_as_expressions(parsing_scope);
            if (expr_list.size() != 1)
            {
                error_printf_at(construct.get_locus(), "clause 'if' requires just one argument\n");
            }
            else
            {
                task_info.set_if_clause_conditional_expression(update_clauses(expr_list, function_sym)[0]);
            }
        }

        // Support final clause
        PragmaCustomClause final_clause = pragma_line.get_clause("final");
        if (final_clause.is_defined())
        {
            ObjectList<Nodecl::NodeclBase> expr_list = final_clause.get_arguments_as_expressions(parsing_scope);
            if (expr_list.size() != 1)
            {
                error_printf_at(construct.get_locus(), "clause 'final' requires just one argument\n");
            }
            else
            {
                task_info.set_final_clause_conditional_expression(update_clauses(expr_list, function_sym)[0]);
            }
        }

        // Support priority clause
        PragmaCustomClause priority_clause = pragma_line.get_clause("priority");
        if (priority_clause.is_defined())
        {
            ObjectList<Nodecl::NodeclBase> expr_list = priority_clause.get_arguments_as_expressions(parsing_scope);
            if (expr_list.size() != 1)
            {
                error_printf_at(construct.get_locus(), "clause 'priority' requires just one argument\n");
            }
            else
            {
                task_info.set_priority_clause_expression(update_clauses(expr_list, function_sym)[0]);
            }
        }

        PragmaCustomClause cost_clause = pragma_line.get_clause("cost");
        if (cost_clause.is_defined())
        {
            ObjectList<Nodecl::NodeclBase> expr_list = cost_clause.get_arguments_as_expressions(parsing_scope);
            if (expr_list.size() != 1)
            {
                error_printf_at(construct.get_locus(), "clause 'cost' requires just one argument\n");
            }
            else
            {
                task_info.set_cost_clause_expression(update_clauses(expr_list, function_sym)[0]);
            }
        }

        PragmaCustomClause tied_clause = pragma_line.get_clause("tied");
        PragmaCustomClause untied_clause = pragma_line.get_clause("untied");

        bool is_untied_task = untied_clause.is_defined()
            // The tasks are untied by default and the current task has not defined the 'tied' clause
            || (_untied_tasks_by_default && !tied_clause.is_defined());

        task_info.set_untied(is_untied_task);

        PragmaCustomClause label_clause = pragma_line.get_clause("label");
        if (label_clause.is_defined())
        {
            TL::ObjectList<std::string> str_list = label_clause.get_tokenized_arguments();

            if (str_list.size() != 1)
            {
                warn_printf_at(
                        construct.get_locus(),
                        "ignoring invalid 'label' clause in 'task' construct\n");
            }
            else
            {
                task_info.set_task_label(
                        Nodecl::OmpSs::TaskLabel::make(
                            str_list[0]));
            }
        }

        task_info.set_parsing_scope(parsing_scope);
        task_info.set_locus(construct.get_locus());

        const char* devices_diagnostic = "";
        if (!task_info.get_target_info().get_device_list().empty())
        {
            TL::ObjectList<std::string> devices = task_info.get_target_info().get_device_list();
            if (devices.size() == 1)
            {
                uniquestr_sprintf(&devices_diagnostic, " for device '%s'", devices[0].c_str());
            }
            else
            {
                for (TL::ObjectList<std::string>::iterator it = devices.begin();
                        it != devices.end();
                        it++)
                {
                    if (it + 1 == devices.end())
                    {
                        uniquestr_sprintf(&devices_diagnostic, "%s and '%s'", devices_diagnostic, it->c_str());
                    }
                    else
                    {
                        if (it != devices.begin())
                            uniquestr_sprintf(&devices_diagnostic, "%s, '%s'", devices_diagnostic, it->c_str());
                        else
                            uniquestr_sprintf(&devices_diagnostic, "'%s'", it->c_str());
                    }
                }

                uniquestr_sprintf(&devices_diagnostic, " for devices %s", devices_diagnostic);
            }
        }

        info_printf_at(
                construct.get_locus(),
                "adding task function '%s'%s\n",
                function_sym.get_name().c_str(),
                devices_diagnostic);
        _function_task_set->add_function_task(function_sym, task_info);

        FORTRAN_LANGUAGE()
        {
            static bool already_nagged = false;
            const decl_context_t* decl_context = function_sym.get_scope().get_decl_context();

            if (decl_context->current_scope == decl_context->global_scope)
            {
                warn_printf_at(
                        construct.get_locus(),
                        "!$OMP TASK at top level only applies to calls in the current file\n");

                if (!already_nagged)
                {
                    info_printf_at(
                            construct.get_locus(),
                            "use INTERFACE blocks or MODULE PROCEDUREs when using tasks between files\n");
                    already_nagged = true;
                }
            }
        }

        // The target context has been fully consumed by this function task,
        // this prevents from it leaking to other tasks (see ticket #2564)
        //
        // Recall that std::stack does not have a clear operation so we assign
        // to it a new std::stack
        _target_context = std::stack<TL::OmpSs::TargetContext>();
    }

    void Core::task_inline_handler_pre(TL::PragmaCustomStatement construct)
    {
        TL::PragmaCustomLine pragma_line = construct.get_pragma_line();

        DataEnvironment& data_environment =
            _openmp_info->get_new_data_environment(construct);
        _openmp_info->push_current_data_environment(data_environment);

        TL::Scope scope = construct.retrieve_context();

        ObjectList<Symbol> extra_symbols;
        get_data_explicit_attributes(pragma_line, construct.get_statements(),
                data_environment,
                extra_symbols);

        bool there_is_default_clause = false;
        DataSharingAttribute default_data_attr = get_default_data_sharing(pragma_line,
                /* fallback */ DS_UNDEFINED,
                there_is_default_clause,
                /*allow_default_auto*/ true);

        handle_task_dependences(
                pragma_line, /* parsing_scope */ pragma_line,
                default_data_attr, data_environment, extra_symbols);

        handle_implicit_dependences_of_task_reductions(
                pragma_line, default_data_attr,
                data_environment, extra_symbols);

        if (_target_context.empty())
        {
            // Create an implicit target for this one
            _target_context.push(OmpSs::TargetContext());
            _target_context.top().is_implicit = true;
        }

        ompss_common_target_handler_pre(pragma_line,
                _target_context.top(),
                scope,
                /* is_pragma_task */ true);

        // Target info applies after
        ompss_get_target_info(pragma_line, data_environment);

        get_data_implicit_attributes_task(construct, data_environment, default_data_attr, there_is_default_clause);
        get_data_extra_symbols(data_environment, extra_symbols);

        // The target context has been fully consumed by this inline task,
        // this prevents from it leaking to nested tasks (see ticket #2500)
        //
        // Recall that std::stack does not have a clear operation so we assign
        // to it a new std::stack
        _target_context = std::stack<TL::OmpSs::TargetContext>();
    }

} }
