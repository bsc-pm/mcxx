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

#include "tl-ompss-base-task.hpp"

#include "cxx-diagnostic.h"
#include "cxx-exprtype.h"
#include "cxx-instantiation.h"

#include "fortran03-buildscope.h"

#include "tl-omp-base-utils.hpp"
#include "tl-symbol-utils.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-analysis-interface.hpp"

namespace TL { namespace OmpSs {

    class InstantiateExecEnvironment : public Nodecl::NodeclVisitor<void>
    {
        private:
            sym_to_argument_expr_t& _sym_to_arg;

        public:
            InstantiateExecEnvironment(sym_to_argument_expr_t& sym_to_arg)
                : _sym_to_arg(sym_to_arg)
            {
            }

            void visit(const Nodecl::Symbol& n)
            {
                sym_to_argument_expr_t::iterator it = _sym_to_arg.find(n.get_symbol());
                if (it == _sym_to_arg.end()
                        || it->second.is<Nodecl::FortranNotPresent>())
                    return;

                n.replace(it->second.shallow_copy());
                // Crude
                const_cast<Nodecl::Symbol&>(n).set_type(rewrite_type(n.get_type()));
            }

            // Do not store environments involving only name-lists
            void visit(const Nodecl::OpenMP::Shared& n) { Nodecl::Utils::remove_from_enclosing_list(n); }
            void visit(const Nodecl::OpenMP::Firstprivate& n) { Nodecl::Utils::remove_from_enclosing_list(n); }
            void visit(const Nodecl::OpenMP::Private& n) { Nodecl::Utils::remove_from_enclosing_list(n); }
            void visit(const Nodecl::OpenMP::Lastprivate& n) { Nodecl::Utils::remove_from_enclosing_list(n); }
            void visit(const Nodecl::OpenMP::FirstLastprivate& n) { Nodecl::Utils::remove_from_enclosing_list(n); }

            void unhandled_node(const Nodecl::NodeclBase & n)
            {
                Nodecl::NodeclBase::Children children = n.children();
                for (Nodecl::NodeclBase::Children::iterator it = children.begin();
                        it != children.end();
                        it++)
                {
                    walk(*it);
                }
                // Crude
                const_cast<Nodecl::NodeclBase&>(n).set_type(rewrite_type(n.get_type()));
            }

            TL::Type rewrite_type(TL::Type t)
            {
                if (!t.is_valid())
                    return t;

                if (t.is_lvalue_reference())
                {
                    return rewrite_type(t.references_to()).get_lvalue_reference_to();
                }
                else if (t.is_pointer())
                {
                    return (rewrite_type(t.points_to())).get_pointer_to();
                }
                else if (t.is_array())
                {
                    TL::Type element_type = rewrite_type(t.array_element());

                    Nodecl::NodeclBase lower_bound, upper_bound;
                    t.array_get_bounds(lower_bound, upper_bound);

                    lower_bound = lower_bound.shallow_copy();
                    this->walk(lower_bound);

                    upper_bound = upper_bound.shallow_copy();
                    this->walk(upper_bound);

                    if (!t.array_is_region())
                    {
                        return element_type.get_array_to(lower_bound, upper_bound,
                                CURRENT_COMPILED_FILE->global_decl_context);
                    }
                    else
                    {
                        Nodecl::NodeclBase region_lower_bound, region_upper_bound;
                        t.array_get_region_bounds(region_lower_bound, region_upper_bound);

                        region_lower_bound = region_lower_bound.shallow_copy();
                        this->walk(region_lower_bound);

                        region_upper_bound = region_upper_bound.shallow_copy();
                        this->walk(region_upper_bound);

                        return element_type.get_array_to_with_region(
                                lower_bound, upper_bound,
                                region_lower_bound, region_upper_bound,
                                CURRENT_COMPILED_FILE->global_decl_context);
                    }
                }
                else
                {
                    // Best effort
                    return t;
                }
            }
    };

    class SimplifyExecEnvironment : public Nodecl::ExhaustiveVisitor<void>
    {
        public:

            virtual void visit(const Nodecl::Reference& node)
            {
                walk(node.get_rhs());
                if (node.get_rhs().is<Nodecl::Dereference>())
                {
                    // &*a => a
                    node.replace(node.get_rhs().as<Nodecl::Dereference>().get_rhs());
                }
            }

            virtual void visit(const Nodecl::Dereference& node)
            {
                walk(node.get_rhs());
                if (node.get_rhs().is<Nodecl::Reference>())
                {
                    // *&a => a
                    node.replace(node.get_rhs().as<Nodecl::Reference>().get_rhs());
                }
            }
    };

    FunctionCallVisitor::FunctionCallVisitor(std::shared_ptr<TL::OmpSs::FunctionTaskSet> function_task_set,
            const std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& funct_call_to_enclosing_stmt_map,
            const std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& enclosing_stmt_to_original_stmt_map,
            const std::map<Nodecl::NodeclBase, std::set<TL::Symbol> >& enclosing_stmt_to_return_vars_map,
            OpenMP::Base *base,
            bool ignore_template_functions)
        :
            _function_task_set(function_task_set),
            _funct_call_to_enclosing_stmt_map(funct_call_to_enclosing_stmt_map),
            _enclosing_stmt_to_original_stmt_map(enclosing_stmt_to_original_stmt_map),
            _enclosing_stmt_to_return_vars_map(enclosing_stmt_to_return_vars_map),
            _enclosing_stmt_to_task_calls_map(),
            _base(base),
            _ignore_template_functions(ignore_template_functions)
    {
    }

    void FunctionCallVisitor::visit(const Nodecl::FunctionCode& node)
    {
        if (IS_CXX_LANGUAGE
                && _ignore_template_functions
                && node.get_symbol().is_member()
                && node.get_symbol().get_class_type().is_dependent())
            return;

        this->Nodecl::ExhaustiveVisitor<void>::visit(node);
    }

    void FunctionCallVisitor::visit(const Nodecl::TemplateFunctionCode& node)
    {
        if (_ignore_template_functions)
            return;

        this->Nodecl::ExhaustiveVisitor<void>::visit(node);
    }

    void FunctionCallVisitor::visit(const Nodecl::FunctionCall& call)
    {
        Nodecl::NodeclBase called = call.get_called();

        if (!called.is<Nodecl::Symbol>())
            return;

        Symbol sym = called.as<Nodecl::Symbol>().get_symbol();

        if (sym.is_from_module())
        {
            // This symbol comes from a module, but use
            // the ultimate one otherwise we may be loading
            // the wrong information
            sym = sym.aliased_from_module();
            TL::Symbol module = sym.in_module();

            ERROR_CONDITION(!module.is_valid(), "Invalid module symbol", 0);
            if (!module.is_intrinsic())
            {
                // Make sure the module has been loaded
                ::fortran_load_module(module.get_name().c_str(), /* intrinsic */ 0, call.get_locus());

                // Check if we already saw this module
                module_function_tasks_set_t::iterator it = _module_function_tasks.find(module);
                if (it == _module_function_tasks.end())
                {
                    // Not seen before, load
                    _function_task_set->load_from_module(module);

                    _module_function_tasks.insert(module);
                }
            }
        }

        TL::OmpSs::FunctionTaskInfo task_info;
        bool valid_task_info = false;
        if (_function_task_set->is_function_task(sym))
        {
            // Usual case
            task_info = _function_task_set->get_function_task(sym);
            valid_task_info = true;
        }
        else if ( // if the type of the current function symbol is a template specialized type
                sym.get_type().is_template_specialized_type()
                // and the primary template is registered as a function task
                && _function_task_set->is_function_task(
                    sym.get_type().get_related_template_type().get_primary_template().get_symbol()))
        {
            // Note that the pragma of the current task is written in terms of the primary symbol
            TL::Symbol primary_sym  = sym.get_type().get_related_template_type().get_primary_template().get_symbol();

            // This map will be used to instantiate the function task info
            instantiation_symbol_map_t* instantiation_symbol_map =
                instantiation_symbol_map_push(/* parent */ NULL);

            TL::Scope prototype_scope = new_prototype_context(sym.get_scope().get_decl_context());
            prototype_scope.get_decl_context()->current_scope->related_entry = sym.get_internal_symbol();

            TL::ObjectList<TL::Symbol> primary_params = primary_sym.get_related_symbols();
            TL::ObjectList<TL::Type> spec_param_types = sym.get_type().parameters();
            ERROR_CONDITION(primary_params.size() != spec_param_types.size(), "Unreachable code", 0);

            // Create the related symbols of the new specialization, using the
            // parameter types of the specialization function type
            // FIXME: I'm not sure, but maybe the instantiation phase should
            // provided the related symbols of the specialization
            TL::ObjectList<TL::Symbol> spec_related_symbols;
            int num_params = primary_params.size();
            for (int i = 0; i < num_params; ++i)
            {
                TL::Symbol prim_param_sym = primary_params[i];
                TL::Type spec_param_type =  spec_param_types[i];

                TL::Symbol spec_param_sym = prototype_scope.new_symbol(prim_param_sym.get_name());

                spec_param_sym.get_internal_symbol()->kind = SK_VARIABLE;
                spec_param_sym.get_internal_symbol()->type_information = update_type(
                        spec_param_type.get_internal_type(),
                        sym.get_scope().get_decl_context(),
                        call.get_locus());

                symbol_entity_specs_set_is_user_declared(spec_param_sym.get_internal_symbol(), 1);

                spec_related_symbols.append(spec_param_sym);

                symbol_set_as_parameter_of_function(spec_param_sym.get_internal_symbol(),
                        sym.get_internal_symbol(), /* nesting */ 0, /* position */ i);

                instantiation_symbol_map_add(instantiation_symbol_map,
                        prim_param_sym.get_internal_symbol(), spec_param_sym.get_internal_symbol());
            }

            sym.set_related_symbols(spec_related_symbols);

            TL::OmpSs::FunctionTaskInfo template_task_info = _function_task_set->get_function_task(primary_sym);

            task_info = template_task_info.instantiate_function_task_info(sym, prototype_scope, instantiation_symbol_map);
            valid_task_info = true;

            // We update the function task set with the new specialized task info. Note that
            // future calls to the same specialization will use the same FunctionTaskInfo
            _function_task_set->add_function_task(sym, task_info);
        }

        // The current function call is not a task, ignore it!
        if (!valid_task_info)
            return;

        if (_base->emit_omp_report())
        {
            *(_base->get_omp_report_file())
                << "\n"
                << call.get_locus_str() << ": " << "TASK call\n"
                << call.get_locus_str() << ": " << "---------\n"
                << OpenMP::Report::indent
                << "Call to task '" << sym.get_qualified_name() << "' declared at '" << sym.get_locus_str() << "'\n"
                ;
        }
        Nodecl::NodeclBase exec_env = this->make_exec_environment(call, sym, task_info);

        Nodecl::NodeclBase call_site_exec_env = instantiate_exec_env(exec_env, call);

        Nodecl::OmpSs::TaskCall task_call = Nodecl::OmpSs::TaskCall::make(
                exec_env,
                // We need to copy the call because we need to preserve
                // the original place of the call
                call.shallow_copy(),
                // Site environment, do not use it
                call_site_exec_env,
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

    void FunctionCallVisitor::build_all_needed_task_expressions()
    {
        for (std::map<Nodecl::NodeclBase, TL::ObjectList<Nodecl::NodeclBase> >::iterator it = _enclosing_stmt_to_task_calls_map.begin();
                it != _enclosing_stmt_to_task_calls_map.end();
                ++it)
        {
            Nodecl::NodeclBase enclosing_stmt = it->first;
            TL::ObjectList<Nodecl::NodeclBase> task_calls = it->second;
            Nodecl::NodeclBase sequential_code = _enclosing_stmt_to_original_stmt_map.find(enclosing_stmt)->second;
            std::set<TL::Symbol> return_arguments = _enclosing_stmt_to_return_vars_map.find(enclosing_stmt)->second;

            ERROR_CONDITION(!enclosing_stmt.is<Nodecl::ExpressionStatement>(),
                    "Unexpected '%s' node",
                    ast_print_node_type(enclosing_stmt.get_kind()));

            Nodecl::NodeclBase join_task;
            if (enclosing_stmt.as<Nodecl::ExpressionStatement>().get_nest().is<Nodecl::OmpSs::TaskCall>())
            {
                Nodecl::OmpSs::TaskCall task_call =
                    enclosing_stmt.as<Nodecl::ExpressionStatement>().get_nest().as<Nodecl::OmpSs::TaskCall>();

                Nodecl::FunctionCall function_call = task_call.get_call().as<Nodecl::FunctionCall>();
                internal_error("%s: unsupported case: the function task '%s' cannot have return tasks as arguments yet\n",
                        enclosing_stmt.get_locus_str().c_str(),
                        function_call.get_called().as<Nodecl::Symbol>().get_symbol().get_name().c_str());
            }
            else
            {
                // Generate the inline task and It's execution environment
                join_task = generate_join_task(enclosing_stmt);
            }

            // This code will be executed if the current task is in a final context
            Nodecl::NodeclBase task_expr = Nodecl::ExpressionStatement::make(
                    Nodecl::OmpSs::TaskExpression::make(
                        join_task,
                        Nodecl::List::make(task_calls),
                        sequential_code,
                        enclosing_stmt.get_locus()));


            Nodecl::Utils::prepend_items_before(enclosing_stmt, task_expr);
            Nodecl::Utils::remove_from_enclosing_list(enclosing_stmt);
        }
    }

    Nodecl::NodeclBase FunctionCallVisitor::make_exec_environment(
            const Nodecl::FunctionCall &call,
            TL::Symbol function_sym,
            TL::OmpSs::FunctionTaskInfo& function_task_info)
    {
        const locus_t* locus = call.get_locus();

        TL::ObjectList<Nodecl::NodeclBase> result_list;

        TL::ObjectList<TL::OmpSs::FunctionTaskDependency> task_dependences = function_task_info.get_parameter_info();

        // This makes the report confusing, disable it
        bool old_omp_report = _base->emit_omp_report();

        _base->set_omp_report(false);
        _base->make_dependency_list<Nodecl::OpenMP::DepIn>(
                task_dependences,
                OpenMP::DEP_DIR_IN,
                locus,
                result_list);

        _base->make_dependency_list<Nodecl::OmpSs::DepWeakIn>(
                task_dependences,
                OpenMP::DEP_OMPSS_WEAK_IN,
                locus,
                result_list);

        _base->make_dependency_list<Nodecl::OmpSs::DepInPrivate>(
                task_dependences,
                OpenMP::DEP_OMPSS_DIR_IN_PRIVATE,
                locus,
                result_list);

        _base->make_dependency_list<Nodecl::OpenMP::DepOut>(
                task_dependences,
                OpenMP::DEP_DIR_OUT,
                locus,
                result_list);

        _base->make_dependency_list<Nodecl::OmpSs::DepWeakOut>(
                task_dependences,
                OpenMP::DEP_OMPSS_WEAK_OUT,
                locus,
                result_list);

        _base->make_dependency_list<Nodecl::OpenMP::DepInout>(
                task_dependences,
                OpenMP::DEP_DIR_INOUT,
                locus,
                result_list);

        _base->make_dependency_list<Nodecl::OmpSs::DepWeakInout>(
                task_dependences,
                OpenMP::DEP_OMPSS_WEAK_INOUT,
                locus,
                result_list);

        _base->make_dependency_list<Nodecl::OmpSs::Concurrent>(
                task_dependences,
                OpenMP::DEP_OMPSS_CONCURRENT,
                locus,
                result_list);

        _base->make_dependency_list<Nodecl::OmpSs::Commutative>(
                task_dependences,
                OpenMP::DEP_OMPSS_COMMUTATIVE,
                locus,
                result_list);

        // Make sure the remaining symbols are firstprivate
        std::vector<bool> has_dep(function_sym.get_type().parameters().size(), false);

        TL::ObjectList<Nodecl::NodeclBase> assumed_firstprivates, assumed_shareds;
        for (TL::ObjectList<TL::OmpSs::FunctionTaskDependency>::iterator it = task_dependences.begin();
                it != task_dependences.end();
                it++)
        {
            TL::DataReference data_ref = it->get_data_reference();
            TL::Symbol base_sym = data_ref.get_base_symbol();

            // OmpSs Assumption: dependences are passed always as SHARED
            assumed_shareds.append(base_sym.make_nodecl(base_sym.get_locus()));

            if (base_sym.is_parameter_of(function_sym))
            {
                has_dep[base_sym.get_parameter_position_in(function_sym)] = true;
            }
        }

        // Variables that have to be closed as shared
        TL::ObjectList<TL::Symbol> shared_closure = function_task_info.get_shared_closure();
        for (TL::ObjectList<TL::Symbol>::iterator it = shared_closure.begin();
                it != shared_closure.end();
                it++)
        {
            assumed_shareds.append(it->make_nodecl(it->get_locus()));
        }

        TL::ObjectList<TL::Symbol> parameters = function_sym.get_related_symbols();

        Nodecl::List arguments = call.get_arguments().as<Nodecl::List>();
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
                    Nodecl::Symbol symbol_ref = it->make_nodecl(/* set_ref_type */ true, locus);

                    assumed_firstprivates.append(symbol_ref);
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    Nodecl::Symbol symbol_ref = it->make_nodecl(/* set_ref_type */ true, locus);

                    if(!it->get_type().is_fortran_array())
                    {
                        Nodecl::NodeclBase arg;
                        if ((unsigned int)i < arguments.size())
                            arg = arguments[i];

                        // Skip missing arguments
                        if (arg.is<Nodecl::FortranNotPresent>())
                            continue;

                        ERROR_CONDITION(arg.is_null(), "Invalid node", 0);

                        warn_printf_at(
                                function_sym.get_locus(),
                                "assuming dummy argument '%s' of function task '%s' "
                                "is SHARED because it does not have VALUE attribute\n",
                                it->get_name().c_str(),
                                function_sym.get_name().c_str());
                        if (!arg.is_constant())
                        {
                            info_printf_at(function_sym.get_locus(),
                                    "during the execution of task '%s', the dummy argument '%s' may not have "
                                    "the value that the actual argument '%s' had at task creation\n",
                                    function_sym.get_name().c_str(),
                                    it->get_name().c_str(),
                                    arg.prettyprint().c_str());
                        }
                    }

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
        TL::Symbol called_symbol = function_sym;
        if (IS_FORTRAN_LANGUAGE)
        {
            called_symbol = call.as<Nodecl::FunctionCall>()
                                .get_called().as<Nodecl::Symbol>().get_symbol();
        }
        _base->make_execution_environment_target_information(
                function_task_info.get_target_info(),
                called_symbol,
                locus,
                result_list);


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

        if (!function_task_info.get_final_clause_conditional_expression().is_null())
        {
            result_list.append(
                    Nodecl::OpenMP::Final::make(function_task_info.get_final_clause_conditional_expression().shallow_copy())
                    );
        }

        if (!function_task_info.get_priority_clause_expression().is_null())
        {
            result_list.append(
                    Nodecl::OpenMP::Priority::make(function_task_info.get_priority_clause_expression().shallow_copy())
                    );
        }

        if (!function_task_info.get_cost_clause_expression().is_null())
        {
            result_list.append(
                    Nodecl::OmpSs::Cost::make(function_task_info.get_cost_clause_expression().shallow_copy())
                    );
        }

        if (!function_task_info.get_task_label().is_null())
        {
            result_list.append(
                    Nodecl::OmpSs::TaskLabel::make(
                        function_task_info.get_task_label().get_text()));
        }

        result_list.append(
                Nodecl::OpenMP::FunctionTaskParsingContext::make(
                    Nodecl::PragmaContext::make(function_task_info.get_parsing_scope()),
                    function_task_info.get_locus()
                    ));
        _base->set_omp_report(old_omp_report);

        return Nodecl::List::make(result_list);
    }

    template < typename T>
    void decompose_expression_statement(Nodecl::NodeclBase expr,
            Nodecl::NodeclBase& lhs_expr, Nodecl::NodeclBase& rhs_expr)
    {
        lhs_expr = expr.as<T>().get_lhs();
        rhs_expr = expr.as<T>().get_rhs();
    }

    void decompose_expression_statement(Nodecl::NodeclBase expression_stmt, Nodecl::NodeclBase & lhs_expr, Nodecl::NodeclBase& rhs_expr)
    {
        ERROR_CONDITION(!expression_stmt.is<Nodecl::ExpressionStatement>(),
                        "Unexpected node %s\n", ast_print_node_type(expression_stmt.get_kind()));

        rhs_expr = lhs_expr = nodecl_null();

        Nodecl::NodeclBase expr = expression_stmt.as<Nodecl::ExpressionStatement>().get_nest();
        if (expr.is<Nodecl::AddAssignment>())
            decompose_expression_statement<Nodecl::AddAssignment>(expr, lhs_expr, rhs_expr);
        else if (expr.is<Nodecl::ArithmeticShrAssignment>())
            decompose_expression_statement<Nodecl::ArithmeticShrAssignment>(expr, lhs_expr, rhs_expr);
        else if (expr.is<Nodecl::Assignment>())
            decompose_expression_statement<Nodecl::Assignment>(expr, lhs_expr, rhs_expr);
        else if (expr.is<Nodecl::BitwiseAndAssignment>())
            decompose_expression_statement<Nodecl::BitwiseAndAssignment>(expr, lhs_expr, rhs_expr);
        else if (expr.is<Nodecl::BitwiseOrAssignment>())
            decompose_expression_statement<Nodecl::BitwiseOrAssignment>(expr, lhs_expr, rhs_expr);
        else if (expr.is<Nodecl::BitwiseShlAssignment>())
            decompose_expression_statement<Nodecl::BitwiseShlAssignment>(expr, lhs_expr, rhs_expr);
        else if (expr.is<Nodecl::BitwiseShrAssignment>())
            decompose_expression_statement<Nodecl::BitwiseShrAssignment>(expr, lhs_expr, rhs_expr);
        else if (expr.is<Nodecl::BitwiseXorAssignment>())
            decompose_expression_statement<Nodecl::BitwiseXorAssignment>(expr, lhs_expr, rhs_expr);
        else if (expr.is<Nodecl::DivAssignment>())
            decompose_expression_statement<Nodecl::DivAssignment>(expr, lhs_expr, rhs_expr);
        else if (expr.is<Nodecl::MinusAssignment>())
            decompose_expression_statement<Nodecl::MinusAssignment>(expr, lhs_expr, rhs_expr);
        else if (expr.is<Nodecl::ModAssignment>())
            decompose_expression_statement<Nodecl::ModAssignment>(expr, lhs_expr, rhs_expr);
        else if (expr.is<Nodecl::MulAssignment>())
            decompose_expression_statement<Nodecl::MulAssignment>(expr, lhs_expr, rhs_expr);
        else rhs_expr = expr;
    }

    void FunctionCallVisitor::fill_map_parameters_to_arguments(
            TL::Symbol function,
            Nodecl::List arguments,
            sym_to_argument_expr_t& param_to_arg_expr)
    {
        int i = 0;
        Nodecl::List::iterator it = arguments.begin();

        // If the current function is a non-static function and It is member of a
        // class, the first argument of the arguments list represents the object of
        // this class. Skip it!
        if (IS_CXX_LANGUAGE
                && !function.is_static()
                && function.is_member())
        {
            it++;
        }

        for (; it != arguments.end(); it++, i++)
        {
            Nodecl::NodeclBase expression;
            TL::Symbol parameter_sym;
            Nodecl::NodeclBase arg;

            if (it->is<Nodecl::FortranActualArgument>())
            {
                // If this is a Fortran style argument use the symbol
                Nodecl::FortranActualArgument named_pair(it->as<Nodecl::FortranActualArgument>());

                parameter_sym = named_pair.get_symbol();
                arg = named_pair.get_argument();
            }
            else
            {
                // Get the i-th parameter of the function
                ERROR_CONDITION(((signed int)function.get_related_symbols().size() <= i), "Too many parameters", 0);
                parameter_sym = function.get_related_symbols()[i];
                arg = *it;
            }

            param_to_arg_expr[parameter_sym] = arg.no_conv();
        }
    }

    struct ReportTaskInfo : public Nodecl::ExhaustiveVisitor<void>
    {
        bool there_are_dependences, there_are_copies;
        ReportTaskInfo() : there_are_dependences(false), there_are_copies(false) { }


        void visit(const Nodecl::OpenMP::DepIn& dep_in)
        {
            there_are_dependences = true;
        }

        void visit(const Nodecl::OmpSs::DepWeakIn& dep_in)
        {
            there_are_dependences = true;
        }

        void visit(const Nodecl::OpenMP::DepOut& dep_out)
        {
            there_are_dependences = true;
        }

        void visit(const Nodecl::OmpSs::DepWeakOut& dep_out)
        {
            there_are_dependences = true;
        }

        void visit(const Nodecl::OpenMP::DepInout& dep_inout)
        {
            there_are_dependences = true;
        }

        void visit(const Nodecl::OmpSs::DepWeakInout& dep_inout)
        {
            there_are_dependences = true;
        }

        void visit(const Nodecl::OmpSs::DepInPrivate& dep_in)
        {
            there_are_dependences = true;
        }

        void visit(const Nodecl::OmpSs::Concurrent& dep_inout)
        {
            there_are_dependences = true;
        }

        void visit(const Nodecl::OmpSs::Commutative& dep_inout)
        {
            there_are_dependences = true;
        }

        void visit(const Nodecl::OmpSs::CopyIn& copy_in)
        {
            there_are_copies = true;
        }

        void visit(const Nodecl::OmpSs::CopyOut& copy_out)
        {
            there_are_copies = true;
        }

        void visit(const Nodecl::OmpSs::CopyInout& copy_inout)
        {
            there_are_copies = true;
        }
    };

    struct ReportExecEnvironmentDependences : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            std::ofstream* _omp_report_file;

        public:

        ReportExecEnvironmentDependences(const locus_t*, std::ofstream* omp_report_file)
            : _omp_report_file(omp_report_file)
        {
        }

        void report_dep(Nodecl::NodeclBase items, TL::OpenMP::DependencyDirection kind)
        {
            Nodecl::List list = items.as<Nodecl::List>();
            for (Nodecl::List::iterator it = list.begin();
                    it != list.end();
                    it++)
            {
                std::stringstream ss;
                ss
                    << OpenMP::Report::indent
                    << OpenMP::Report::indent
                    << it->prettyprint()
                    ;

                int length = ss.str().size();
                int diff = 20 - length;
                if (diff > 0)
                    std::fill_n( std::ostream_iterator<const char*>(ss), diff, " ");

                ss
                    << " " << OpenMP::Base::dependence_direction_to_str(kind) << "\n"
                    ;

                *_omp_report_file
                    << ss.str();
            }
        }

        void visit(const Nodecl::OpenMP::DepIn& dep_in)
        {
            report_dep(dep_in.get_in_deps(), OpenMP::DEP_DIR_IN);
        }

        void visit(const Nodecl::OmpSs::DepWeakIn& dep_in)
        {
            report_dep(dep_in.get_weakin_deps(), OpenMP::DEP_OMPSS_WEAK_IN);
        }

        void visit(const Nodecl::OpenMP::DepOut& dep_out)
        {
            report_dep(dep_out.get_out_deps(), OpenMP::DEP_DIR_OUT);
        }

        void visit(const Nodecl::OmpSs::DepWeakOut& dep_in)
        {
            report_dep(dep_in.get_weakout_deps(), OpenMP::DEP_OMPSS_WEAK_OUT);
        }

        void visit(const Nodecl::OpenMP::DepInout& dep_inout)
        {
            report_dep(dep_inout.get_inout_deps(), OpenMP::DEP_DIR_INOUT);
        }

        void visit(const Nodecl::OmpSs::DepWeakInout& dep_in)
        {
            report_dep(dep_in.get_weakinout_deps(), OpenMP::DEP_OMPSS_WEAK_INOUT);
        }

        void visit(const Nodecl::OmpSs::DepInPrivate& dep_in)
        {
            report_dep(dep_in.get_in_deps(), OpenMP::DEP_OMPSS_DIR_IN_PRIVATE);
        }

        void visit(const Nodecl::OmpSs::Concurrent& dep_inout)
        {
            report_dep(dep_inout.get_inout_deps(), OpenMP::DEP_OMPSS_CONCURRENT);
        }

        void visit(const Nodecl::OmpSs::Commutative& dep_inout)
        {
            report_dep(dep_inout.get_commutative_deps(), OpenMP::DEP_OMPSS_COMMUTATIVE);
        }
    };

    struct ReportExecEnvironmentCopies : public Nodecl::ExhaustiveVisitor<void>
    {
        const locus_t* _locus;
        std::ofstream* _omp_report_file;

        ReportExecEnvironmentCopies(const locus_t* locus, std::ofstream* omp_report_file)
            : _locus(locus), _omp_report_file(omp_report_file)
        {
        }

        void report_copy(Nodecl::NodeclBase items, TL::OmpSs::CopyDirection kind)
        {
            Nodecl::List list = items.as<Nodecl::List>();
            for (Nodecl::List::iterator it = list.begin();
                    it != list.end();
                    it++)
            {
                // Let's make sure this is properly aligned
                std::stringstream ss;
                ss
                    << OpenMP::Report::indent
                    << OpenMP::Report::indent
                    << it->prettyprint()
                    ;

                int length = ss.str().size();
                int diff = 20 - length;
                if (diff > 0)
                    std::fill_n( std::ostream_iterator<const char*>(ss), diff, " ");

                ss
                    << " " << OpenMP::Base::copy_direction_to_str(kind) << "\n"
                    ;

                *_omp_report_file
                    << ss.str();
            }
        }

        void visit(const Nodecl::OmpSs::CopyIn& copy_in)
        {
            report_copy(copy_in.get_input_copies(), TL::OmpSs::COPY_DIR_IN);
        }

        void visit(const Nodecl::OmpSs::CopyOut& copy_out)
        {
            report_copy(copy_out.get_output_copies(), TL::OmpSs::COPY_DIR_OUT);
        }

        void visit(const Nodecl::OmpSs::CopyInout& copy_inout)
        {
            report_copy(copy_inout.get_inout_copies(), TL::OmpSs::COPY_DIR_INOUT);
        }
    };

    struct ReportExecEnvironmentTarget : public Nodecl::ExhaustiveVisitor<void>
    {
        const locus_t* _locus;
        std::ofstream* _omp_report_file;

        ReportExecEnvironmentTarget(const locus_t* locus, std::ofstream* omp_report_file)
            : _locus(locus), _omp_report_file(omp_report_file)
        {
        }

        void visit(const Nodecl::OmpSs::Target& node)
        {

            Nodecl::List l = node.get_devices().as<Nodecl::List>();

            if (l.size() == 1)
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "The main implementation of this task targets the device '"
                    << l[0].prettyprint() << "'\n"
                    ;
            }
            else
            {
                *_omp_report_file << locus_to_str(_locus) << ": The main implementation of this task targets devices "
                    ;
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    if (it != l.begin())
                        *_omp_report_file << ", ";

                    *_omp_report_file << "'" << it->prettyprint() << "'";
                }

                *_omp_report_file << "\n";
            }

            // Make sure we walk the TARGET-associated items
            walk(node.get_items());
        }


        void visit(const Nodecl::OmpSs::NDRange& node)
        {
            *_omp_report_file
                << OpenMP::Report::indent
                << "The task call specifies the following NDRANGE(";

            Nodecl::List l = node.get_ndrange_expressions().as<Nodecl::List>();

            for (Nodecl::List::iterator it = l.begin();
                    it != l.end();
                    it++)
            {
                if (it != l.begin())
                    *_omp_report_file << ", ";

                *_omp_report_file << it->prettyprint();
            }

            *_omp_report_file << ")\n";
        }

        void visit(const Nodecl::OmpSs::ShMem& node)
        {
            *_omp_report_file << locus_to_str(_locus) << ": The task call specifies the following SHMEM(";

            Nodecl::List l = node.get_shmem_expressions().as<Nodecl::List>();

            for (Nodecl::List::iterator it = l.begin();
                    it != l.end();
                    it++)
            {
                if (it != l.begin())
                    *_omp_report_file << ", ";

                *_omp_report_file << it->prettyprint();
            }

            *_omp_report_file << ")\n";
        }

        void visit(const Nodecl::OmpSs::Implements& node)
        {
            TL::Symbol implementation_symbol = node.get_function_name().get_symbol();

            *_omp_report_file
                << OpenMP::Report::indent
                << "There is an alternate implementation for the device '"
                << node.get_device().prettyprint() << "' specified by the function task '"
                << implementation_symbol.get_qualified_name() << "' at '" << implementation_symbol.get_locus_str() << "'\n";
        }
    };

    Nodecl::NodeclBase FunctionCallVisitor::instantiate_exec_env(Nodecl::NodeclBase exec_env, Nodecl::FunctionCall call)
    {
        sym_to_argument_expr_t param_to_arg_expr;
        Nodecl::List arguments = call.get_arguments().as<Nodecl::List>();
        TL::Symbol called_sym = call.get_called().get_symbol();
        fill_map_parameters_to_arguments(called_sym, arguments, param_to_arg_expr);

        Nodecl::NodeclBase result = exec_env.shallow_copy();

        InstantiateExecEnvironment instantiate_visitor(param_to_arg_expr);
        instantiate_visitor.walk(result);

        // Simplify dependences
        SimplifyExecEnvironment simplify_env;
        simplify_env.walk(result);

        if (_base->emit_omp_report())
        {
            ReportTaskInfo report_info;
            report_info.walk(result);

            if (report_info.there_are_dependences)
            {
                *(_base->get_omp_report_file())
                    << OpenMP::Report::indent
                    << "Dependences\n"
                    ;
                ReportExecEnvironmentDependences report_deps(call.get_locus(), _base->get_omp_report_file());
                report_deps.walk(result);
            }

            if (report_info.there_are_copies)
            {
                *(_base->get_omp_report_file())
                    << OpenMP::Report::indent
                    << "Copies\n"
                    ;
                ReportExecEnvironmentCopies report_copies(call.get_locus(), _base->get_omp_report_file());
                report_copies.walk(result);
            }

            ReportExecEnvironmentTarget report_target(call.get_locus(), _base->get_omp_report_file());
            report_target.walk(result);
        }

        return result;
    }

    static bool expression_stmt_is_a_reduction(Nodecl::NodeclBase expr,
            std::shared_ptr<TL::OmpSs::FunctionTaskSet> function_task_set)
    {
        ERROR_CONDITION(!expr.is<Nodecl::ExpressionStatement>(),
                "Unexpected node %s\n", ast_print_node_type(expr.get_kind()));

        // We use the analysis phase here to know if an expression is a reduction.
        // Related ticket: https://pm.bsc.es/projects/mcxx/ticket/1873
#ifdef ANALYSIS_ENABLED
        TL::Analysis::AnalysisInterface a;
        bool is_reduc = a.is_ompss_reduction(expr.as<Nodecl::ExpressionStatement>().get_nest(), function_task_set);
        return is_reduc;
#else
        return 0;
#endif
    }

    Nodecl::OpenMP::Task FunctionCallVisitor::generate_join_task(const Nodecl::NodeclBase& enclosing_stmt)
    {
        Nodecl::List exec_environment;

        const locus_t* locus = enclosing_stmt.get_locus();

        TL::ObjectList<Nodecl::NodeclBase> target_items,
            /* copies information */ copy_in, copy_out, copy_inout,
            /* dependences information */ in_deps, concurrent_deps, out_deps,
            /* data-sharing information */ shared_symbols, shared_and_alloca_exprs,
            /* more data-sharings */ firstprivate_symbols, alloca_exprs;

        Nodecl::NodeclBase lhs_expr, rhs_expr;
        decompose_expression_statement(enclosing_stmt, lhs_expr, rhs_expr);

        // Create the output depedence if needed
        bool is_reduction = expression_stmt_is_a_reduction(enclosing_stmt, _function_task_set);
        if (!lhs_expr.is_null())
        {
            // OmpSs Assumption: dependences are passed always as SHARED
            TL::DataReference data_ref(lhs_expr);
            TL::Symbol sym = data_ref.get_base_symbol();
            shared_symbols.append(sym.make_nodecl(sym.get_locus()));

            if (is_reduction)
            {
                concurrent_deps.append(lhs_expr.shallow_copy());
                copy_inout.append(lhs_expr.shallow_copy());
            }
            else
            {
                out_deps.append(lhs_expr.shallow_copy());
                copy_out.append(lhs_expr.shallow_copy());
            }
        }

        // Obtain the nonlocal symbols from the right expression
        std::set<TL::Symbol> return_arguments = _enclosing_stmt_to_return_vars_map.find(enclosing_stmt)->second;
        TL::ObjectList<Nodecl::Symbol> nonlocal_symbols = Nodecl::Utils::get_nonlocal_symbols_first_occurrence(rhs_expr);
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
                firstprivate_symbols.append((*it2).shallow_copy());
            }
            else
            {
                // The return arguments present in the enclosing statement are added as alloca input dependences
                Nodecl::NodeclBase sym_nodecl = Nodecl::Symbol::make(sym, locus);
                sym_nodecl.set_type(lvalue_ref(sym.get_type().get_internal_type()));

                in_deps.append(
                        Nodecl::Dereference::make(
                            sym_nodecl,
                            sym.get_type().points_to().get_lvalue_reference_to(),
                            locus));

                copy_in.append(
                        Nodecl::Dereference::make(
                            sym_nodecl.shallow_copy(),
                            sym.get_type().points_to().get_lvalue_reference_to(),
                            locus));

                shared_and_alloca_exprs.append(
                        Nodecl::Dereference::make(
                            sym_nodecl.shallow_copy(),
                            sym.get_type().points_to().get_lvalue_reference_to(),
                            locus));
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

            Nodecl::NodeclBase sym_nodecl = Nodecl::Symbol::make(sym, locus);
            sym_nodecl.set_type(lvalue_ref(sym.get_type().get_internal_type()));

            alloca_exprs.append(
                    Nodecl::Dereference::make(
                        sym_nodecl,
                        sym.get_type().points_to().get_lvalue_reference_to(),
                        locus));
        }

        if (!firstprivate_symbols.empty())
        {
            exec_environment.append(
                    Nodecl::OpenMP::Firstprivate::make(
                        Nodecl::List::make(firstprivate_symbols),
                        locus));
        }

        if (!shared_symbols.empty())
        {
            exec_environment.append(
                    Nodecl::OpenMP::Shared::make(
                        Nodecl::List::make(shared_symbols),
                        locus));
        }

        if (!shared_and_alloca_exprs.empty())
        {
            exec_environment.append(
                    Nodecl::OmpSs::SharedAndAlloca::make(
                        Nodecl::List::make(shared_and_alloca_exprs),
                        locus));
        }

        if (!alloca_exprs.empty())
        {
            exec_environment.append(
                    Nodecl::OmpSs::Alloca::make(
                        Nodecl::List::make(alloca_exprs),
                        locus));
        }

        if (!in_deps.empty())
        {
            exec_environment.append(
                    Nodecl::OpenMP::DepIn::make(
                        Nodecl::List::make(in_deps),
                        locus));
        }

        if (!concurrent_deps.empty())
        {
            exec_environment.append(
                    Nodecl::OmpSs::Concurrent::make(
                        Nodecl::List::make(concurrent_deps),
                        locus));
        }

        if (!out_deps.empty())
        {
            exec_environment.append(
                    Nodecl::OpenMP::DepOut::make(
                        Nodecl::List::make(out_deps),
                        locus));
        }

        if (!copy_in.empty())
        {
            target_items.append(
                    Nodecl::OmpSs::CopyIn::make(
                        Nodecl::List::make(copy_in),
                        locus));
        }

        if (!copy_out.empty())
        {
            target_items.append(
                    Nodecl::OmpSs::CopyOut::make(
                        Nodecl::List::make(copy_out),
                        locus));
        }


        if (!copy_inout.empty())
        {
            target_items.append(
                    Nodecl::OmpSs::CopyInout::make(
                        Nodecl::List::make(copy_inout),
                        locus));
        }

        // The inline tasks are always SMP tasks
        exec_environment.append(Nodecl::OmpSs::Target::make(
                    Nodecl::List::make(Nodecl::Text::make("smp", locus)),
                    Nodecl::List::make(target_items),
                    locus));

        Nodecl::NodeclBase join_task_stmt;
        if (is_reduction)
        {
            Nodecl::List atomic_exec_env = Nodecl::List::make(
                    Nodecl::OpenMP::FlushAtEntry::make(locus),
                    Nodecl::OpenMP::FlushAtExit::make(locus));

            Nodecl::NodeclBase atomic_body =
                Nodecl::List::make(
                        Nodecl::Context::make(
                            Nodecl::List::make(enclosing_stmt.shallow_copy()),
                            new_block_context(enclosing_stmt.retrieve_context().get_decl_context())));

            Nodecl::OpenMP::Atomic atomic =
                Nodecl::OpenMP::Atomic::make(
                        atomic_exec_env,
                        atomic_body,
                        locus);

            join_task_stmt = atomic;
        }
        else
        {
            join_task_stmt = enclosing_stmt.shallow_copy();
        }

        Nodecl::OpenMP::Task join_task = Nodecl::OpenMP::Task::make(
                exec_environment,
                Nodecl::List::make(join_task_stmt),
                locus);

        return join_task;
    }

    TransformNonVoidFunctionCalls::TransformNonVoidFunctionCalls(
            std::shared_ptr<TL::OmpSs::FunctionTaskSet> function_task_set,
            bool task_expr_optim_disabled,
            bool ignore_template_functions)
        :
            _task_expr_optim_disabled(task_expr_optim_disabled),
            _ignore_template_functions(ignore_template_functions),
            _optimized_task_expr_counter(0),
            _new_return_vars_counter(0),
            _enclosing_stmt(nodecl_null()),
            _function_task_set(function_task_set),
            _transformed_task_map(),
            _ignore_these_function_calls(),
            _enclosing_stmts_with_more_than_one_task(),
            _funct_call_to_enclosing_stmt_map(),
            _enclosing_stmt_to_original_stmt(),
            _enclosing_stmt_to_return_vars_map()
    {
    }

    void TransformNonVoidFunctionCalls::visit(const Nodecl::FunctionCode& node)
    {
        if (IS_CXX_LANGUAGE
                && _ignore_template_functions
                && node.get_symbol().is_member()
                && node.get_symbol().get_class_type().is_dependent())
            return;

        this->Nodecl::ExhaustiveVisitor<void>::visit(node);
    }

    void TransformNonVoidFunctionCalls::visit(const Nodecl::TemplateFunctionCode& node)
    {
        if (_ignore_template_functions)
            return;

        this->Nodecl::ExhaustiveVisitor<void>::visit(node);
    }

    void TransformNonVoidFunctionCalls::visit(const Nodecl::ObjectInit& object_init)
    {
        TL::Symbol sym = object_init.get_symbol();
        if (sym.get_value().is_null())
            return;

        // Save the old enclosing expr statement
        Nodecl::NodeclBase old_enclosing_stmt = _enclosing_stmt;
        _enclosing_stmt = object_init;

        walk(sym.get_value());

        // Restore the old enclosing expr statement
        _enclosing_stmt = old_enclosing_stmt;
    }

    void TransformNonVoidFunctionCalls::visit(const Nodecl::ReturnStatement& return_stmt)
    {
        // Save the old enclosing expr statement
        Nodecl::NodeclBase old_enclosing_stmt = _enclosing_stmt;
        _enclosing_stmt = return_stmt;

        walk(return_stmt.get_value());

        // Restore the old enclosing expr statement
        _enclosing_stmt = old_enclosing_stmt;
    }


    void TransformNonVoidFunctionCalls::visit(const Nodecl::FunctionCall& func_call)
    {
        if (_ignore_these_function_calls.contains(func_call))
            return;

        // First of all, visit the arguments of the current function call
        Nodecl::List arguments = func_call.get_arguments().as<Nodecl::List>();
        walk(arguments);

        Nodecl::NodeclBase called = func_call.get_called();
        if (!called.is<Nodecl::Symbol>())
            return;

        TL::Symbol function_called = called.as<Nodecl::Symbol>().get_symbol();

        if (!_function_task_set->is_function_task(function_called))
            return;

        if (function_called.get_type().returns().is_void())
            return;

        // Some statements should be transformed into others more friendly
        Nodecl::NodeclBase enclosing_stmt = get_enclosing_stmt(func_call);
        if(enclosing_stmt.is<Nodecl::ObjectInit>())
        {
            transform_object_init(enclosing_stmt, func_call.get_locus());
        }
        else if(enclosing_stmt.is<Nodecl::ReturnStatement>())
        {
            transform_return_statement(enclosing_stmt, func_call.get_locus());
        }


        if (_enclosing_stmt_to_original_stmt.find(enclosing_stmt) == _enclosing_stmt_to_original_stmt.end())
            _enclosing_stmt_to_original_stmt.insert(std::make_pair(enclosing_stmt, enclosing_stmt.shallow_copy()));

        Scope scope = enclosing_stmt.retrieve_context();

        // Optimization: Do not create two tasks if there is only a task
        // involved in the task expression
        if ( !_task_expr_optim_disabled
                && expression_stmt_is_a_reduction(enclosing_stmt, _function_task_set)
                && only_one_task_is_involved_in_this_stmt(enclosing_stmt))
        {
            info_printf_at(enclosing_stmt.get_locus(),
                      "enabling experimental optimization: transforming task expression '%s' into a simple task\n",
                      enclosing_stmt.as<Nodecl::ExpressionStatement>().get_nest().prettyprint().c_str());
            info_printf_at(enclosing_stmt.get_locus(),
                      "To disable this optimization use the flag "
                      " --variable=disable_task_expression_optimization:1\n");

            transform_task_expression_into_simple_task(func_call, enclosing_stmt);
            return;
        }

        TL::Symbol transformed_task;
        TL::Type return_type = function_called.get_type().returns().get_pointer_to();
        std::map<TL::Symbol, TL::Symbol>::iterator it_transformed_task = _transformed_task_map.find(function_called);
        if (it_transformed_task == _transformed_task_map.end())
        {
            // Using the information of the nonvoid function task,create a new void function
            // task which acts like a wrapper (i. e. calls to the nonvoid function)
            std::string new_function_name = function_called.get_name() + "__";
            TL::ObjectList<std::string> parameter_names;
            TL::ObjectList<TL::Type> parameter_types;

            // An extra parameter should be added to this new function if the function called
            // is a nonstatic member. This parameter represents the 'this' object
            if (function_called.is_member()
                    && !function_called.is_static())
            {
                // Make sure we use the scope we used to parse the dependences
                TL::OmpSs::FunctionTaskInfo function_called_task_info =
                    _function_task_set->get_function_task(function_called);

                Scope sc = function_called_task_info.get_parsing_scope();
                TL::Symbol this_ = sc.get_symbol_this();
                ERROR_CONDITION(this_.is_invalid(), "Unreachable code", 0);
                parameter_names.append("this__");
                parameter_types.append(this_.get_type());
            }

            parameter_types.append(function_called.get_type().parameters());
            TL::ObjectList<TL::Symbol> function_called_related_symbols = function_called.get_related_symbols();
            for (TL::ObjectList<TL::Symbol>::iterator it = function_called_related_symbols.begin();
                    it != function_called_related_symbols.end();
                    it++)
            {
                parameter_names.append(it->get_name());
            }

            parameter_names.append("output");
            parameter_types.append(function_called.get_type().returns().get_pointer_to());

            TL::Symbol new_function = SymbolUtils::new_function_symbol(
                    scope.get_related_symbol(),
                    new_function_name,
                    TL::Type::get_void_type(),
                    parameter_names,
                    parameter_types);


            Nodecl::NodeclBase new_function_code,new_function_body;
            SymbolUtils::build_empty_body_for_function(new_function, new_function_code, new_function_body);

            symbol_entity_specs_set_function_code(new_function.get_internal_symbol(), new_function_code.get_internal_nodecl());

            // Update the map of transformed tasks
            _transformed_task_map.insert(std::make_pair(function_called, new_function));

            // Create the code of the new void function task
            Nodecl::NodeclBase new_function_stmts =
                create_body_for_new_function_task(function_called, new_function);

            Nodecl::Utils::append_to_top_level_nodecl(new_function_code);

            // In C++, we need to specify explicitly the forward declaration
            CXX_LANGUAGE()
            {
                Nodecl::Utils::prepend_items_before(
                        scope.get_related_symbol().get_function_code(),
                        Nodecl::CxxDecl::make(/* context */ nodecl_null(), new_function));
            }

            new_function_body.replace(new_function_stmts);

            // Finally, we should add the new void function task into the task set
            TL::OmpSs::FunctionTaskInfo function_called_task_info = _function_task_set->get_function_task(function_called);

            add_new_task_to_the_function_task_set(
                    function_called,
                    new_function,
                    function_called_task_info,
                    func_call,
                    /* has_return_argument */ true,
                    TL::OpenMP::DEP_DIR_OUT);

            transformed_task = new_function;
        }
        else
        {
            transformed_task = it_transformed_task->second;
        }

        // Create a new function call to the new void function and
        // replace the original function call by the variable used to
        // store the return value

        // Create the new called entity
        Nodecl::NodeclBase called_entity = Nodecl::Symbol::make(
                transformed_task,
                func_call.get_locus());

        called_entity.set_type(lvalue_ref(transformed_task.get_type().get_internal_type()));

        // Declare a new variable which represents the return of the original function as an argument
        std::stringstream ss;
        ss << "mcc_ret_" << _new_return_vars_counter;
        TL::Symbol return_arg_sym = scope.new_symbol(ss.str());
        _new_return_vars_counter++;

        return_arg_sym.get_internal_symbol()->kind = SK_VARIABLE;
        return_arg_sym.get_internal_symbol()->type_information = return_type.get_internal_type();
        symbol_entity_specs_set_is_user_declared(return_arg_sym.get_internal_symbol(), 1);

        // Create the new argument's list of the new function call to the void function task
        Nodecl::List new_arguments;
        Nodecl::List::iterator it = arguments.begin();

        // If this is a nonstatic member, the first argument should be the address of the 'this' object
        if (function_called.is_member()
                && !function_called.is_static())
        {
            new_arguments.append(
                    Nodecl::Reference::make(
                        *it,
                        it->get_type().no_ref().get_pointer_to(),
                        func_call.get_locus()));
            it++;
        }

        for (; it != arguments.end(); it++)
        {
            new_arguments.append(*it);
        }

        // Finally, extend the argument's list adding the output argument
        Nodecl::NodeclBase return_arg_nodecl = Nodecl::Symbol::make(
                return_arg_sym,
                func_call.get_locus());

        return_arg_nodecl.set_type(lvalue_ref(return_arg_sym.get_type().get_internal_type()));
        new_arguments.append(return_arg_nodecl);

        // Create the new function call and encapsulate it in a ExpressionStatement
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

        // Prepend the new function call before the enclosing statement of the original function call
        Nodecl::Utils::prepend_items_before(enclosing_stmt, expression_stmt);

        // Replace the original function call with the variable
        Nodecl::NodeclBase dereference_return =
            Nodecl::Conversion::make(
                    Nodecl::Dereference::make(
                        return_arg_nodecl,
                        return_arg_sym.get_type().points_to().get_lvalue_reference_to(),
                        func_call.get_locus()),
                    return_arg_sym.get_type().points_to());

        func_call.replace(dereference_return);

        // Update the map between enclosing statement and their return arguments
        _enclosing_stmt_to_return_vars_map[enclosing_stmt].insert(return_arg_sym);
    }

    std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& TransformNonVoidFunctionCalls::get_function_call_to_enclosing_stmt_map()
    {
        return _funct_call_to_enclosing_stmt_map;
    }

    std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& TransformNonVoidFunctionCalls::get_enclosing_stmt_to_original_stmt_map()
    {
        return _enclosing_stmt_to_original_stmt;
    }

    std::map<Nodecl::NodeclBase, std::set<TL::Symbol> >& TransformNonVoidFunctionCalls::get_enclosing_stmt_to_return_variables_map()
    {
        return _enclosing_stmt_to_return_vars_map;
    }

    void TransformNonVoidFunctionCalls::remove_nonvoid_function_tasks_from_function_task_set()
    {
        for (std::map<TL::Symbol, TL::Symbol>::const_iterator it = _transformed_task_map.begin();
                it != _transformed_task_map.end();
                it++)
        {
            TL::Symbol function_called = it->first;

            TL::OmpSs::FunctionTaskInfo function_task_info = _function_task_set->get_function_task(function_called);
            _function_task_set->remove_function_task(function_called);
        }
    }

    Nodecl::NodeclBase TransformNonVoidFunctionCalls::get_enclosing_stmt(Nodecl::NodeclBase function_call)
    {
        if (!_enclosing_stmt.is_null())
            return _enclosing_stmt;

        Nodecl::NodeclBase enclosing_stmt = function_call;
        while (!enclosing_stmt.is_null()
                && !enclosing_stmt.is<Nodecl::ExpressionStatement>())
        {
            enclosing_stmt = enclosing_stmt.get_parent();
        }

        ERROR_CONDITION(enclosing_stmt.is_null(), "The enclosing expression cannot be a NULL node", 0);

        return enclosing_stmt;
    }

    bool TransformNonVoidFunctionCalls::only_one_task_is_involved_in_this_stmt(Nodecl::NodeclBase stmt)
    {
        // If this enclosing stmt has been handled before and It has more than one task, ignore it
        if (_enclosing_stmts_with_more_than_one_task.contains(stmt))
            return false;

        class TaskFinder : public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                int _num_involved_tasks;
                std::shared_ptr<TL::OmpSs::FunctionTaskSet> _function_task_set;

            public:
                TaskFinder(std::shared_ptr<TL::OmpSs::FunctionTaskSet> function_task_set) :
                    _num_involved_tasks(0),
                    _function_task_set(function_task_set) { }

                void visit(const Nodecl::Symbol& node)
                {
                    TL::Symbol sym = node.get_symbol();
                    Nodecl::NodeclBase value = sym.get_value();
                    if (!value.is_null())
                    {
                        walk(sym.get_value());
                    }
                }

                void visit(const Nodecl::FunctionCall& func_call)
                {
                    if (_num_involved_tasks >= 2)
                        return;

                    Nodecl::NodeclBase called = func_call.get_called();
                    if (!called.is<Nodecl::Symbol>())
                        return;

                    TL::Symbol function_called = called.as<Nodecl::Symbol>().get_symbol();
                    if (_function_task_set->is_function_task(function_called))
                        _num_involved_tasks++;
                }

                bool only_one_task_is_involved() { return (_num_involved_tasks == 1); }
        };

        TaskFinder visitor(_function_task_set);
        visitor.walk(stmt);

        bool result = visitor.only_one_task_is_involved();
        if (!result)
        {
            _enclosing_stmts_with_more_than_one_task.append(stmt);
        }
        return result;
    }

    // Replace the object initialization with a variable definition and an assignment
    // Expected input:
    //      {
    //          int x = foo(...);
    //      }
    //     - where foo is a function task which returns an integer
    //
    // Expected output:
    //      {
    //          int x;
    //          x = foo(...);
    //      }
    void TransformNonVoidFunctionCalls::transform_object_init(Nodecl::NodeclBase enclosing_stmt, const locus_t* locus)
    {
        ERROR_CONDITION(!enclosing_stmt.is<Nodecl::ObjectInit>(),
                "Unexpected node '%s'\n",
                ast_print_node_type(enclosing_stmt.get_kind()));

        TL::Symbol sym = enclosing_stmt.as<Nodecl::ObjectInit>().get_symbol();
        sym.get_internal_symbol()->type_information = sym.get_type().get_unqualified_type().get_internal_type();

        Nodecl::NodeclBase sym_nodecl = Nodecl::Symbol::make(sym, locus);
        sym_nodecl.set_type(lvalue_ref(sym.get_type().get_internal_type()));

        Nodecl::NodeclBase new_expr_stmt = Nodecl::ExpressionStatement::make(
                Nodecl::Assignment::make(
                    sym_nodecl,
                    sym.get_value(),
                    sym_nodecl.get_type(),
                    locus),
                locus);

        // Replace the object initialization with this new assignment
        enclosing_stmt.replace(new_expr_stmt);

        // Remove the value of the 'sym' symbol
        sym.get_internal_symbol()->value = nodecl_null();

        CXX_LANGUAGE()
        {
            Nodecl::Utils::prepend_items_before(enclosing_stmt,
                    Nodecl::CxxDef::make( /* context */ nodecl_null(), sym, locus));
        }
    }

    // Replace the return statement with a variable definition, an assignment,
    // a taskwait and an empty return statement
    // Expected input:
    //      {
    //          return foo(...);
    //      }
    //     - where foo is a function task which returns an integer
    //
    // Expected output:
    //      {
    //          int mcc_ret_smt_0;
    //          mcc_ret_smt_0 = foo(...);
    //          #pragma omp taskwait
    //          return;
    //      }
    void TransformNonVoidFunctionCalls::transform_return_statement(Nodecl::NodeclBase enclosing_stmt, const locus_t* locus)
    {
        ERROR_CONDITION(!enclosing_stmt.is<Nodecl::ReturnStatement>(),
                "Unexpected node '%s'\n",
                ast_print_node_type(enclosing_stmt.get_kind()));

        Nodecl::NodeclBase value = enclosing_stmt.as<Nodecl::ReturnStatement>().get_value();

        std::stringstream ss;
        ss << "mcc_ret_stmt_" << _new_return_vars_counter;
        Scope scope = enclosing_stmt.retrieve_context();
        TL::Symbol new_symbol = scope.new_symbol(ss.str());
        _new_return_vars_counter++;

        new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
        new_symbol.get_internal_symbol()->type_information = value.get_type().get_internal_type();
        symbol_entity_specs_set_is_user_declared(new_symbol.get_internal_symbol(), 1);

        Nodecl::NodeclBase sym_nodecl = Nodecl::Symbol::make(new_symbol, locus);
        sym_nodecl.set_type(lvalue_ref(new_symbol.get_type().get_internal_type()));

        Nodecl::NodeclBase new_expr_stmt = Nodecl::ExpressionStatement::make(
                Nodecl::Assignment::make(
                    sym_nodecl,
                    value,
                    sym_nodecl.get_type(),
                    locus),
                locus);

        // Replace the return statement with this new assignment
        enclosing_stmt.replace(new_expr_stmt);

        Nodecl::Utils::append_items_after(enclosing_stmt, Nodecl::ReturnStatement::make(sym_nodecl.shallow_copy()));
        Nodecl::Utils::append_items_after(enclosing_stmt, Nodecl::OpenMP::TaskwaitShallow::make(/*exec environment*/ nodecl_null()));

        CXX_LANGUAGE()
        {
            Nodecl::Utils::prepend_items_before(enclosing_stmt,
                    Nodecl::CxxDef::make( /* context */ nodecl_null(), new_symbol, locus));
        }
    }

    void TransformNonVoidFunctionCalls::add_new_task_to_the_function_task_set(
            TL::Symbol ori_funct,
            TL::Symbol new_funct,
            const TL::OmpSs::FunctionTaskInfo& ori_funct_task_info,
            Nodecl::NodeclBase func_call,
            bool has_return_argument,
            TL::OpenMP::DependencyDirection dep_dir_ret_arg)
    {
        Nodecl::Utils::SimpleSymbolMap translation_map;
        TL::ObjectList<TL::Symbol> new_funct_related_symbols = new_funct.get_related_symbols();
        TL::ObjectList<TL::Symbol> ori_funct_related_symbols = ori_funct.get_related_symbols();
        unsigned int ori_funct_num_related_symbols = ori_funct.get_num_related_symbols();
        unsigned int j = 0;
        if (ori_funct.is_member()
                && !ori_funct.is_static())
        {
            j++;
        }

        for (unsigned int i = 0; i < ori_funct_num_related_symbols; ++i, ++j)
        {
            translation_map.add_map(ori_funct_related_symbols[j], new_funct_related_symbols[i]);
        }
        translation_map.add_map(ori_funct, new_funct);

        TL::OmpSs::FunctionTaskInfo new_funct_task_info(ori_funct_task_info, translation_map, new_funct);
        new_funct_task_info.set_locus(func_call.get_locus());

        if (has_return_argument)
        {
            TL::ObjectList<TL::Symbol> new_funcion_related_symbols = new_funct.get_related_symbols();
            TL::Symbol return_argument = new_funcion_related_symbols[new_funcion_related_symbols.size()-1];
            Nodecl::NodeclBase return_argument_nodecl = Nodecl::Symbol::make(
                    return_argument,
                    return_argument.get_locus());
            return_argument_nodecl.set_type(lvalue_ref(return_argument.get_type().get_internal_type()));

            TL::DataReference data_ref_dep(
                    Nodecl::Dereference::make(
                        return_argument_nodecl,
                        return_argument_nodecl.get_type().no_ref().points_to().get_lvalue_reference_to(),
                        return_argument.get_locus()));

            TL::OmpSs::FunctionTaskDependency result_dependence(data_ref_dep, dep_dir_ret_arg);
            new_funct_task_info.add_function_task_dependency(result_dependence);

            TL::OmpSs::TargetInfo& target_info = new_funct_task_info.get_target_info();
            TL::ObjectList<TL::OmpSs::CopyItem> new_copies;
            new_copies.append(TL::OmpSs::CopyItem(data_ref_dep, TL::OmpSs::COPY_DIR_OUT));
            target_info.append_to_copy_out(new_copies);
        }

        _function_task_set->add_function_task(new_funct, new_funct_task_info);
    }

    Nodecl::NodeclBase TransformNonVoidFunctionCalls::create_body_for_new_function_task(
            TL::Symbol original_function,
            TL::Symbol new_function)
    {
        // Create the new called entity
        Nodecl::NodeclBase called_entity = Nodecl::Symbol::make(
                original_function,
                original_function.get_locus());

        called_entity.set_type(lvalue_ref(original_function.get_type().get_internal_type()));

        // Create the list of arguments
        Nodecl::List argument_list;

        TL::ObjectList<TL::Symbol> new_function_related_symbols = new_function.get_related_symbols();
        unsigned int i = 0;
        if (original_function.is_member()
                && !original_function.is_static())
        {
            Nodecl::NodeclBase new_arg =
                Nodecl::Symbol::make(new_function_related_symbols[0],
                        original_function.get_locus());

            new_arg.set_type(lvalue_ref(new_function_related_symbols[0].get_type().get_internal_type()));
            argument_list.append(Nodecl::Dereference::make(
                        new_arg,
                        new_arg.get_type().no_ref().points_to().get_lvalue_reference_to(),
                        original_function.get_locus()));
            i++;
        }

        unsigned int new_function_num_related_symbols = new_function.get_num_related_symbols();
        for (; i < new_function_num_related_symbols - 1; ++i)
        {
            Nodecl::NodeclBase new_arg =
                Nodecl::Symbol::make(new_function_related_symbols[i],
                        original_function.get_locus());

            new_arg.set_type(lvalue_ref(new_function_related_symbols[i].get_type().get_internal_type()));
            argument_list.append(new_arg);
        }

        // Create the new function call
        Nodecl::NodeclBase new_function_call = Nodecl::FunctionCall::make(
                called_entity,
                argument_list,
                /* alternate name */ nodecl_null(),
                /* function_form */ nodecl_null(),
                original_function.get_type().returns(),
                original_function.get_locus());

        // The last parameter of the new function is used to store the return value of the new_function_call
        Nodecl::NodeclBase return_param = Nodecl::Symbol::make(
                new_function_related_symbols[new_function_num_related_symbols - 1],
                original_function.get_locus());

        return_param.set_type(lvalue_ref(new_function_related_symbols[new_function_num_related_symbols-1].get_type().get_internal_type()));

        Nodecl::NodeclBase deref_return_param = Nodecl::Dereference::make(
                return_param,
                return_param.get_type().no_ref().points_to().get_lvalue_reference_to(),
                original_function.get_locus());

        Nodecl::NodeclBase expression_stmt =
            Nodecl::ExpressionStatement::make(
                    Nodecl::Assignment::make(
                        deref_return_param,
                        new_function_call,
                        return_param.get_type(),
                        original_function.get_locus()));

        // The function call to the original task should be ignored!
        _ignore_these_function_calls.append(new_function_call.as<Nodecl::FunctionCall>());
        return expression_stmt;
    }



    // If the current task expression contains only a return task, we can create
    // a new function task which evaluates the whole expression, instead of create
    // two tasks (one for the return task and another one for the join).
    //
    // Expected input:
    //
    //      #pragma omp task in(*i)
    //      int foo(int* i) { return *i + 1; }
    //
    //      int main()
    //      {
    //          int x = 0, i = 1;
    //          x += foo(&i) + i;
    //      }
    //
    // Expected output:
    //      int foo(int *i) { return *i + 1; }
    //
    //      #pragma omp task in(*i) concurrent(*out)
    //      void foo__(int* i, int i_captured, int* out)
    //      {
    //          int tmp = foo(i) + i_captured;
    //          #pragma omp atomic
    //          *out += tmp;
    //      }
    //
    //      int main()
    //      {
    //          int x = 0, i = 1;
    //          foo__(&i, i, x);
    //      }
    //

    void generate_list_of_symbols_to_be_captured_rec(Nodecl::NodeclBase node,
            Nodecl::NodeclBase lhs_expr,
            Nodecl::NodeclBase func_call,
            TL::ObjectList<TL::Symbol>& captured_value_symbols)
    {
        if (node.is_null())
            return;

        if (node == func_call)
            return;

        if (!lhs_expr.is_null()
                && node.get_kind() == lhs_expr.get_kind()
                && Nodecl::Utils::structurally_equal_nodecls(node, lhs_expr))
            return;

        if (node.is<Nodecl::Symbol>())
        {
            TL::Symbol sym = node.as<Nodecl::Symbol>().get_symbol();
            if (sym.is_variable())
                captured_value_symbols.append(sym);
        }

        if (node.is<Nodecl::List>())
        {
            Nodecl::List l = node.as<Nodecl::List>();
            for (Nodecl::List::iterator it = l.begin(); it != l.end(); it++)
            {
                generate_list_of_symbols_to_be_captured_rec(
                        *it, lhs_expr, func_call, captured_value_symbols);
            }
        }
        else
        {
            Nodecl::NodeclBase::Children children = node.children();
            for (Nodecl::NodeclBase::Children::iterator it = children.begin();
                    it != children.end();
                    it++)
            {
                generate_list_of_symbols_to_be_captured_rec(
                        *it, lhs_expr, func_call, captured_value_symbols);
            }
        }
    }

    TL::ObjectList<TL::Symbol> TransformNonVoidFunctionCalls::generate_list_of_symbols_to_be_captured(
            Nodecl::NodeclBase rhs_expr,
            Nodecl::NodeclBase lhs_expr,
            Nodecl::NodeclBase func_call)
    {
        TL::ObjectList<TL::Symbol> captured_value_symbols;
        generate_list_of_symbols_to_be_captured_rec(
                rhs_expr, lhs_expr, func_call, captured_value_symbols);
        return captured_value_symbols;
    }

    void TransformNonVoidFunctionCalls::transform_task_expression_into_simple_task(
            Nodecl::FunctionCall func_call,
            Nodecl::NodeclBase enclosing_stmt)
    {
        Scope scope = enclosing_stmt.retrieve_context();
        Nodecl::List arguments = func_call.get_arguments().as<Nodecl::List>();
        TL::Symbol function_called = func_call.get_called().as<Nodecl::Symbol>().get_symbol();

        std::stringstream ss;
        ss << function_called.get_name() << "__" << _optimized_task_expr_counter;
        _optimized_task_expr_counter++;

        std::string new_function_name = ss.str();
        TL::ObjectList<std::string> parameter_names;
        TL::ObjectList<TL::Type> parameter_types;

        // An extra parameter should be added to this new function if the function called
        // is a nonstatic member. This parameter represents the 'this' object
        if (function_called.is_member()
                && !function_called.is_static())
        {
            // Make sure we use the scope we used to parse the dependences
            TL::OmpSs::FunctionTaskInfo function_called_task_info =
                _function_task_set->get_function_task(function_called);

            Scope sc = function_called_task_info.get_parsing_scope();
            TL::Symbol this_ = sc.get_symbol_this();
            ERROR_CONDITION(this_.is_invalid(), "Unreachable code", 0);
            parameter_names.append("this__");
            parameter_types.append(this_.get_type());
        }

        parameter_types.append(function_called.get_type().parameters());
        TL::ObjectList<TL::Symbol> function_called_related_symbols = function_called.get_related_symbols();
        for (TL::ObjectList<TL::Symbol>::iterator it = function_called_related_symbols.begin();
                it != function_called_related_symbols.end();
                it++)
        {
            parameter_names.append(it->get_name());
        }

        Nodecl::NodeclBase lhs_expr, rhs_expr;
        decompose_expression_statement(enclosing_stmt, lhs_expr, rhs_expr);
        bool has_return_argument = !lhs_expr.is_null();

        TL::ObjectList<TL::Symbol> captured_value_symbols =
            generate_list_of_symbols_to_be_captured(rhs_expr, lhs_expr, func_call);

        for (TL::ObjectList<TL::Symbol>::iterator it = captured_value_symbols.begin();
                it != captured_value_symbols.end(); ++it)
        {
            parameter_names.append(it->get_name() + "_capt");
            parameter_types.append(it->get_type());

        }

        if (has_return_argument)
        {
            parameter_names.append("output");
            parameter_types.append(function_called.get_type().returns().get_pointer_to());
        }

        TL::Symbol new_function = SymbolUtils::new_function_symbol(
                scope.get_related_symbol(),
                new_function_name,
                TL::Type::get_void_type(),
                parameter_names,
                parameter_types);

        Nodecl::NodeclBase new_function_code,new_function_body;
        SymbolUtils::build_empty_body_for_function(new_function, new_function_code, new_function_body);

        symbol_entity_specs_set_function_code(new_function.get_internal_symbol(), new_function_code.get_internal_nodecl());

        // Create the code of the new void function task
        Nodecl::NodeclBase new_stmts = create_body_for_new_optmized_function_task(
                function_called,
                new_function,
                enclosing_stmt.shallow_copy(),
                new_function_body,
                captured_value_symbols);

        Nodecl::Utils::append_to_top_level_nodecl(new_function_code);

        // In C++, we need to specify explicitly the forward declaration
        CXX_LANGUAGE()
        {
            Nodecl::Utils::prepend_items_before(
                    scope.get_related_symbol().get_function_code(),
                    Nodecl::CxxDecl::make(/* context */ nodecl_null(), new_function));
        }

        new_function_body.replace(new_stmts);

        // Finally, we should add the new void function task into the task set
        TL::OmpSs::FunctionTaskInfo function_called_task_info = _function_task_set->get_function_task(function_called);

        add_new_task_to_the_function_task_set(
                function_called,
                new_function,
                function_called_task_info,
                func_call,
                has_return_argument,
                TL::OpenMP::DEP_OMPSS_CONCURRENT);

        // Create the new called entity
        Nodecl::NodeclBase called_entity = Nodecl::Symbol::make(
                new_function,
                func_call.get_locus());

        called_entity.set_type(lvalue_ref(new_function.get_type().get_internal_type()));

        // Create the new argument's list of the new function call to the void function task
        Nodecl::List new_arguments;
        Nodecl::List::iterator it = arguments.begin();

        // If this is a nonstatic member, the first argument should be the address of the 'this' object
        if (function_called.is_member()
                && !function_called.is_static())
        {
            new_arguments.append(
                    Nodecl::Reference::make(
                        *it,
                        it->get_type().no_ref().get_pointer_to(),
                        func_call.get_locus()));
            it++;
        }

        for (; it != arguments.end(); it++)
        {
            new_arguments.append(*it);
        }

        for(TL::ObjectList<TL::Symbol>::iterator it2 = captured_value_symbols.begin();
                it2 != captured_value_symbols.end(); ++it2)
        {
            Nodecl::NodeclBase new_arg =
                Nodecl::Symbol::make(*it2,
                        func_call.get_locus());

            new_arg.set_type(lvalue_ref(it2->get_type().get_internal_type()));
            new_arguments.append(new_arg);
        }

        if (has_return_argument)
        {
            // Finally, extend the argument's list adding the output argument
            Nodecl::NodeclBase dereference_return =
                Nodecl::Reference::make(
                        lhs_expr,
                        lhs_expr.get_type().no_ref(),
                        func_call.get_locus());

            new_arguments.append(dereference_return);
        }

        // Create the new function call and encapsulate it in a ExpressionStatement
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

        enclosing_stmt.replace(expression_stmt);
    }

    void TransformNonVoidFunctionCalls::update_rhs_expression(
            Nodecl::NodeclBase node,
            Nodecl::NodeclBase lhs_expr,
            TL::Symbol ori_funct,
            const ObjectList<TL::Symbol>& new_funct_related_symbols,
            Nodecl::Utils::SimpleSymbolMap& map_for_captures,
            Nodecl::NodeclBase& task_call)
    {
        if (node.is_null())
            return;

        if (node.is<Nodecl::FunctionCall>())
        {
            Nodecl::NodeclBase called = node.as<Nodecl::FunctionCall>().get_called();
            if (called.is<Nodecl::Symbol>()
                    && ori_funct == called.as<Nodecl::Symbol>().get_symbol())
            {
                Nodecl::NodeclBase called_entity = Nodecl::Symbol::make(
                        ori_funct,
                        ori_funct.get_locus());

                called_entity.set_type(lvalue_ref(ori_funct.get_type().get_internal_type()));

                unsigned int i = 0;
                Nodecl::List argument_list;
                if (ori_funct.is_member()
                        && !ori_funct.is_static())
                {
                    Nodecl::NodeclBase new_arg =
                        Nodecl::Symbol::make(new_funct_related_symbols[0],
                                ori_funct.get_locus());

                    new_arg.set_type(lvalue_ref(new_funct_related_symbols[0].get_type().get_internal_type()));
                    argument_list.append(Nodecl::Dereference::make(
                                new_arg,
                                new_arg.get_type().no_ref().points_to().get_lvalue_reference_to(),
                                ori_funct.get_locus()));
                    i++;
                }

                unsigned int num_ori_funct_params = ori_funct.get_related_symbols().size();
                for (; i < num_ori_funct_params; ++i)
                {
                    Nodecl::NodeclBase new_arg =
                        Nodecl::Symbol::make(new_funct_related_symbols[i],
                                ori_funct.get_locus());

                    new_arg.set_type(lvalue_ref(new_funct_related_symbols[i].get_type().get_internal_type()));
                    argument_list.append(new_arg);
                }

                Nodecl::FunctionCall new_funct_call = Nodecl::FunctionCall::make(
                        called_entity,
                        argument_list,
                        /* alternate name */ nodecl_null(),
                        /* function_form */ nodecl_null(),
                        ori_funct.get_type().returns(),
                        ori_funct.get_locus());

                node.replace(new_funct_call);

                task_call = node;

                return;
            }
        }

        if (!lhs_expr.is_null()
                && node.get_kind() == lhs_expr.get_kind()
                && Nodecl::Utils::structurally_equal_nodecls(node, lhs_expr))
        {
            int new_funct_num_related_symbols = new_funct_related_symbols.size();

            Nodecl::NodeclBase return_param = Nodecl::Symbol::make(
                    new_funct_related_symbols[new_funct_num_related_symbols - 1],
                    ori_funct.get_locus());

            return_param.set_type(lvalue_ref(
                        new_funct_related_symbols[new_funct_num_related_symbols-1].get_type().get_internal_type()));

            Nodecl::NodeclBase deref_return_param = Nodecl::Dereference::make(
                    return_param,
                    return_param.get_type().no_ref().points_to().get_lvalue_reference_to(),
                    ori_funct.get_locus());

            node.replace(deref_return_param);
            return;
        }

        if (node.is<Nodecl::Symbol>())
        {
            Nodecl::NodeclBase node_updated
                = Nodecl::Utils::deep_copy(node, node, map_for_captures);
            node.replace(node_updated);
            return;
        }
        if (node.is<Nodecl::List>())
        {
            Nodecl::List l = node.as<Nodecl::List>();
            for (Nodecl::List::iterator it = l.begin(); it != l.end(); it++)
            {
                update_rhs_expression(*it, lhs_expr, ori_funct, new_funct_related_symbols, map_for_captures, task_call);
            }
        }
        else
        {
            Nodecl::NodeclBase::Children children = node.children();
            for (Nodecl::NodeclBase::Children::iterator it = children.begin();
                    it != children.end();
                    it++)
            {
                update_rhs_expression(*it, lhs_expr, ori_funct, new_funct_related_symbols, map_for_captures, task_call);
            }
        }
    }


    Nodecl::NodeclBase TransformNonVoidFunctionCalls::create_body_for_new_optmized_function_task(
            TL::Symbol ori_funct,
            TL::Symbol new_funct,
            Nodecl::NodeclBase enclosing_stmt,
            Nodecl::NodeclBase new_funct_body,
            TL::ObjectList<TL::Symbol>& captured_value_symbols)
    {
        Nodecl::List new_stmts;
        Nodecl::NodeclBase lhs_expr, rhs_expr;
        decompose_expression_statement(enclosing_stmt, lhs_expr, rhs_expr);

        // We need to update the rhs_expr because It's not expressed in terms
        // of the parameters of the new function
        Nodecl::Utils::SimpleSymbolMap map;
        int aux = ori_funct.get_related_symbols().size();
        TL::ObjectList<TL::Symbol> new_funct_related_symbols = new_funct.get_related_symbols();
        for (TL::ObjectList<TL::Symbol>::iterator it = captured_value_symbols.begin();
                it != captured_value_symbols.end(); ++it, ++aux)
        {
            map.add_map(*it, new_funct_related_symbols[aux]);
        }

        Nodecl::NodeclBase task_call = nodecl_null();
        update_rhs_expression(rhs_expr, lhs_expr, ori_funct, new_funct_related_symbols, map, task_call);

        ERROR_CONDITION(task_call.is_null(), "Unreachable code", 0);
        ERROR_CONDITION(!task_call.is<Nodecl::FunctionCall>(),
                "Unexpected '%s' node",
                    ast_print_node_type(task_call.get_kind()));

        if (!lhs_expr.is_null())
        {
            // We create a new auxiliar variable wich value is the result of the task call
            TL::Symbol aux_var = new_funct_body.retrieve_context().new_symbol("tmp");
            aux_var.get_internal_symbol()->kind = SK_VARIABLE;
            aux_var.get_internal_symbol()->type_information = task_call.get_type().no_ref().get_internal_type();
            symbol_entity_specs_set_is_user_declared(aux_var.get_internal_symbol(), 1);

            Nodecl::NodeclBase task_call_copied = task_call.shallow_copy();
            aux_var.get_internal_symbol()->value = task_call_copied.get_internal_nodecl();

            // The function call to the original task should be ignored!
             _ignore_these_function_calls.append(task_call_copied.as<Nodecl::FunctionCall>());

            new_stmts.append(Nodecl::ObjectInit::make(aux_var, new_funct_body.get_locus()));

            Nodecl::NodeclBase aux_var_nodecl = Nodecl::Symbol::make(
                    aux_var,
                    new_funct_body.get_locus());

            aux_var_nodecl.set_type(aux_var.get_type().get_internal_type());

            // Replace the task call with new auxiliar variable in the rhs expression
            task_call.replace(aux_var_nodecl);

            int new_funct_num_related_symbols = new_funct_related_symbols.size();
            Nodecl::NodeclBase return_param = Nodecl::Symbol::make(
                    new_funct_related_symbols[new_funct_num_related_symbols - 1],
                    ori_funct.get_locus());

            return_param.set_type(lvalue_ref(new_funct_related_symbols[new_funct_num_related_symbols-1].get_type().get_internal_type()));

            Nodecl::NodeclBase deref_return_param = Nodecl::Dereference::make(
                    return_param,
                    return_param.get_type().no_ref().points_to().get_lvalue_reference_to(),
                    ori_funct.get_locus());

            // Replace the lhs expression with the return parameter
            lhs_expr.replace(deref_return_param);


            Nodecl::List atomic_exec_env = Nodecl::List::make(
                    Nodecl::OpenMP::FlushAtEntry::make(new_funct_body.get_locus()),
                    Nodecl::OpenMP::FlushAtExit::make(new_funct_body.get_locus()));

            Nodecl::NodeclBase atomic_body =
                Nodecl::List::make(
                        Nodecl::Context::make(
                            Nodecl::List::make(enclosing_stmt.shallow_copy()),
                            new_block_context(enclosing_stmt.retrieve_context().get_decl_context())));

            Nodecl::OpenMP::Atomic atomic =
                Nodecl::OpenMP::Atomic::make(
                        atomic_exec_env,
                        atomic_body,
                        new_funct_body.get_locus());
            enclosing_stmt = atomic;
        }

         new_stmts.append(enclosing_stmt);
         return new_stmts;
    }
}}
