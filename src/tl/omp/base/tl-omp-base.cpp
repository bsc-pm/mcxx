/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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
            const std::string& filename, int line,
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
            result_list.append(T::make(Nodecl::List::make(data_ref_list), filename, line));
        }
    }

    template <typename T,
             typename List>
    static void make_copy_list(
            List& dependences,
            CopyDirection kind,
            const std::string& filename, int line,
            ObjectList<Nodecl::NodeclBase>& result_list)
    {
        TL::ObjectList<Nodecl::NodeclBase> data_ref_list;
        for (typename List::iterator it = dependences.begin();
                it != dependences.end();
                it++)
        {
            if (it->get_kind() != kind)
                continue;

            data_ref_list.append(it->get_copy_expression());
        }

        if (!data_ref_list.empty())
        {
            result_list.append(T::make(Nodecl::List::make(data_ref_list), filename, line));
        }
    }

    class FunctionCallVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            RefPtr<FunctionTaskSet> _function_task_set;

            typedef std::set<Symbol> module_function_tasks_set_t;
            module_function_tasks_set_t _module_function_tasks;
        public:
            FunctionCallVisitor(RefPtr<FunctionTaskSet> function_task_set)
                : _function_task_set(function_task_set)
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
                                call.get_filename(),
                                call.get_line());

                        call.replace(task_call);
                    }
                }
            }

        private:
            Nodecl::NodeclBase make_exec_environment(const Nodecl::FunctionCall &call,
                    TL::Symbol function_sym,
                    FunctionTaskInfo& function_task_info)
            {
                int line = call.get_line();
                std::string filename = call.get_filename();

                TL::ObjectList<Nodecl::NodeclBase> result_list;

                TL::ObjectList<FunctionTaskDependency> task_dependences = function_task_info.get_parameter_info();

                make_dependency_list<Nodecl::OpenMP::DepIn>(
                        task_dependences,
                        OpenMP::DEP_DIR_IN,
                        filename,
                        line,
                        result_list);

                make_dependency_list<Nodecl::OpenMP::DepOut>(
                        task_dependences,
                        OpenMP::DEP_DIR_OUT,
                        filename,
                        line,
                        result_list);

                make_dependency_list<Nodecl::OpenMP::DepInout>(
                        task_dependences,
                        OpenMP::DEP_DIR_INOUT,
                        filename,
                        line,
                        result_list);

                make_dependency_list<Nodecl::OpenMP::Concurrent>(
                        task_dependences,
                        OpenMP::DEP_CONCURRENT,
                        filename,
                        line,
                        result_list);

                make_dependency_list<Nodecl::OpenMP::Commutative>(
                        task_dependences,
                        OpenMP::DEP_COMMUTATIVE,
                        filename,
                        line,
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

                TL::ObjectList<Nodecl::NodeclBase> assumed_firstprivates;

                int i = 0;
                for (TL::ObjectList<TL::Symbol>::iterator it = parameters.begin();
                        it != parameters.end();
                        it++, i++)
                {
                    ERROR_CONDITION(i >= (signed int)has_dep.size(), "Mismatch between parameters and related symbols", 0);
                    if (!has_dep[i])
                    {
                        Nodecl::Symbol symbol_ref =
                            Nodecl::Symbol::make(*it, filename, line);
                        symbol_ref.set_type(lvalue_ref(it->get_type().get_internal_type()));

                        assumed_firstprivates.append(symbol_ref);
                    }
                }

                if (!assumed_firstprivates.empty())
                {
                    result_list.append(
                            Nodecl::OpenMP::Firstprivate::make(Nodecl::List::make(assumed_firstprivates),
                                filename, line)
                            );
                }

                // Build the tree which contains the target information
                TargetInfo target_info = function_task_info.get_target_info();

                TL::ObjectList<Nodecl::NodeclBase> devices;
                TL::ObjectList<Nodecl::NodeclBase> target_items;

                ObjectList<std::string> device_list = target_info.get_device_list();
                for (TL::ObjectList<std::string>::iterator it = device_list.begin(); it != device_list.end(); ++it)
                {
                    devices.append(Nodecl::Text::make(strtolower(it->c_str()), filename, line));
                }

                ObjectList<CopyItem> copy_in = target_info.get_copy_in();
                make_copy_list<Nodecl::OpenMP::CopyIn>(
                        copy_in,
                        OpenMP::COPY_DIR_IN,
                        filename, line,
                        target_items);

                ObjectList<CopyItem> copy_out = target_info.get_copy_out();
                make_copy_list<Nodecl::OpenMP::CopyOut>(
                        copy_out,
                        OpenMP::COPY_DIR_OUT,
                        filename, line,
                        target_items);

                ObjectList<CopyItem> copy_inout = target_info.get_copy_inout();
                make_copy_list<Nodecl::OpenMP::CopyInout>(
                        copy_inout,
                        OpenMP::COPY_DIR_INOUT,
                        filename, line,
                        target_items);

                ObjectList<Nodecl::NodeclBase> ndrange_exprs = target_info.get_ndrange();
                if (!ndrange_exprs.empty())
                {
                    target_items.append(
                            Nodecl::OpenMP::NDRange::make(
                                Nodecl::List::make(ndrange_exprs),
                                filename, line));
                }

                ObjectList<FunctionTaskInfo::implementation_pair_t> implementation_table =
                    function_task_info.get_devices_with_implementation();
                for (ObjectList<FunctionTaskInfo::implementation_pair_t>::iterator it = implementation_table.begin();
                        it != implementation_table.end(); ++it)
                {
                    target_items.append(
                            Nodecl::OpenMP::Implements::make(
                                Nodecl::Text::make(it->first),
                                Nodecl::Symbol::make(it->second, filename, line),
                                filename, line));
                }

                result_list.append(
                        Nodecl::OpenMP::Target::make(
                            Nodecl::List::make(devices),
                            Nodecl::List::make(target_items),
                            filename, line));

                if (function_task_info.get_untied())
                {
                    result_list.append(
                            Nodecl::OpenMP::Untied::make(filename, line));
                }

                if (!function_task_info.get_if_clause_conditional_expression().is_null())
                {
                    result_list.append(
                        Nodecl::OpenMP::If::make(function_task_info.get_if_clause_conditional_expression())
                        );
                }

                if (!function_task_info.get_priority_clause_expression().is_null())
                {
                    result_list.append(
                        Nodecl::OpenMP::Priority::make(function_task_info.get_priority_clause_expression())
                        );
                }

                return Nodecl::List::make(result_list);
            }
    };

    Base::Base()
        : PragmaCustomCompilerPhase("omp"), _core(), 
        _vectorizer(TL::Vectorization::Vectorizer::getVectorizer()),
        _simd_enabled(false), _svml_enabled(false), _ffast_math_enabled(false)
    {
        set_phase_name("OpenMP directive to parallel IR");
        set_phase_description("This phase lowers the semantics of OpenMP into the parallel IR of Mercurium");

        register_parameter("omp_dry_run",
                "Disables OpenMP transformation",
                _openmp_dry_run,
                "0");

        register_parameter("simd_enabled",
                "If set to '1' enables simd constructs, otherwise it is disabled",
                _simd_enabled_str,
                "0").connect(functor(&Base::set_simd, *this));


        register_parameter("svml_enabled",
                "If set to '1' enables svml math library, otherwise it is disabled",
                _svml_enabled_str,
                "0").connect(functor(&Base::set_svml, *this));

        register_parameter("ffast_math_enabled",
                "If set to '1' enables ffast_math operations, otherwise it is disabled",
                _ffast_math_enabled_str,
                "0").connect(functor(&Base::set_ffast_math, *this));

        // FIXME - Remove once ticket #1089 is fixed
        register_parameter("do_not_init_udr",
                "Disable initialization of UDRs. This may be needed in some setups due to a bug",
                _core._do_not_init_udr,
                "0");


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
        if (_simd_enabled && _ffast_math_enabled)
        {
            _vectorizer.enable_ffast_math();
        }

        if (_simd_enabled && _svml_enabled)
        {
            _vectorizer.enable_svml();
        }

        _core.run(dto);

        // Do nothing once we have analyzed everything
        if (_openmp_dry_run != "0")
            return;

        this->PragmaCustomCompilerPhase::run(dto);

        RefPtr<FunctionTaskSet> function_task_set = RefPtr<FunctionTaskSet>::cast_static(dto["openmp_task_info"]);

        Nodecl::NodeclBase translation_unit = dto["nodecl"];

        FunctionCallVisitor function_call_visitor(function_task_set);
        function_call_visitor.walk(translation_unit);
    }

#define INVALID_STATEMENT_HANDLER(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomStatement ctr) { \
            error_printf("%s: error: invalid '#pragma %s %s'\n",  \
                    ctr.get_locus().c_str(), \
                    ctr.get_text().c_str(), \
                    ctr.get_pragma_line().get_text().c_str()); \
        } \
        void Base::_name##_handler_post(TL::PragmaCustomStatement) { }

#define INVALID_DECLARATION_HANDLER(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomDeclaration ctr) { \
            error_printf("%s: error: invalid '#pragma %s %s'\n",  \
                    ctr.get_locus().c_str(), \
                    ctr.get_text().c_str(), \
                    ctr.get_pragma_line().get_text().c_str()); \
        } \
        void Base::_name##_handler_post(TL::PragmaCustomDeclaration) { }

        INVALID_DECLARATION_HANDLER(parallel)
        INVALID_DECLARATION_HANDLER(parallel_for)
        INVALID_DECLARATION_HANDLER(parallel_do)
        INVALID_DECLARATION_HANDLER(for)
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

    void Base::set_simd(const std::string simd_enabled_str)
    {
        if (simd_enabled_str == "1")
        {
            _simd_enabled = true;
        }
    }

    void Base::set_svml(const std::string svml_enabled_str)
    {
        if (svml_enabled_str == "1")
        {
            _svml_enabled = true;
        }
    }

    void Base::set_ffast_math(const std::string ffast_math_enabled_str)
    {
        if (ffast_math_enabled_str == "1")
        {
            _ffast_math_enabled = true;
        }
    }

    void Base::atomic_handler_pre(TL::PragmaCustomStatement) { }
    void Base::atomic_handler_post(TL::PragmaCustomStatement directive)
    {
        Nodecl::List execution_environment = Nodecl::List::make(
                Nodecl::OpenMP::FlushAtEntry::make(
                        directive.get_filename(),
                        directive.get_line()),
                Nodecl::OpenMP::FlushAtExit::make(
                        directive.get_filename(),
                        directive.get_line())
        );

        Nodecl::OpenMP::Atomic atomic =
            Nodecl::OpenMP::Atomic::make(
                    execution_environment,
                    directive.get_statements().shallow_copy(),
                    directive.get_filename(),
                    directive.get_line());

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
                    directive.get_filename(),
                    directive.get_line()
            );

        Nodecl::OpenMP::FlushAtExit exit_flush =
            Nodecl::OpenMP::FlushAtExit::make(
                    directive.get_filename(),
                    directive.get_line()
            );

        if (param.is_defined())
        {
            ObjectList<std::string> critical_name = param.get_tokenized_arguments();

            execution_environment = Nodecl::List::make(
                    Nodecl::OpenMP::CriticalName::make(critical_name[0],
                        directive.get_filename(),
                        directive.get_line()),
                    entry_flush, exit_flush);
        }
        else
        {
            execution_environment = Nodecl::List::make(entry_flush, exit_flush);
        }

        directive.replace(
                Nodecl::OpenMP::Critical::make(
                        execution_environment,
                        directive.get_statements().shallow_copy(),
                        directive.get_filename(),
                        directive.get_line())
                );
    }

    void Base::barrier_handler_pre(TL::PragmaCustomDirective) { }
    void Base::barrier_handler_post(TL::PragmaCustomDirective directive)
    {
        Nodecl::List execution_environment = Nodecl::List::make(
                Nodecl::OpenMP::FlushAtEntry::make(
                    directive.get_filename(),
                    directive.get_line()),
                Nodecl::OpenMP::FlushAtExit::make(
                    directive.get_filename(),
                    directive.get_line())
                );

        directive.replace(
                Nodecl::OpenMP::BarrierFull::make(
                    execution_environment,
                    directive.get_filename(),
                    directive.get_line())
                );
    }

    void Base::flush_handler_pre(TL::PragmaCustomDirective) { }
    void Base::flush_handler_post(TL::PragmaCustomDirective directive)
    {
        PragmaClauseArgList parameter = directive.get_pragma_line().get_parameter();

        TL::ObjectList<Nodecl::NodeclBase> expr_list;
        if (!parameter.is_null())
        {
            expr_list = parameter.get_arguments_as_expressions();
        }

        directive.replace(
                Nodecl::OpenMP::FlushMemory::make(
                    Nodecl::List::make(expr_list),
                    directive.get_filename(),
                    directive.get_line())
                );
    }

    void Base::master_handler_pre(TL::PragmaCustomStatement) { }
    void Base::master_handler_post(TL::PragmaCustomStatement directive)
    {
        directive.replace(
                Nodecl::OpenMP::Master::make(
                    directive.get_statements().shallow_copy(),
                    directive.get_filename(),
                    directive.get_line())
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

        if (!expr_list.empty())
        {
            Nodecl::OpenMP::DepInout dep_inout = Nodecl::OpenMP::DepInout::make(
                    Nodecl::List::make(expr_list),
                    directive.get_filename(),
                    directive.get_line());

            directive.replace(
                    Nodecl::OpenMP::WaitOnDependences::make(
                        Nodecl::List::make(dep_inout),
                        directive.get_filename(), directive.get_line())
                    );
        }
        else
        {
            directive.replace(
                    Nodecl::OpenMP::TaskwaitShallow::make(
                        directive.get_filename(),
                        directive.get_line())
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
                        directive.get_filename(),
                        directive.get_line()));
        }

        PragmaCustomClause priority = pragma_line.get_clause("priority");
        if (priority.is_defined())
        {
            TL::ObjectList<Nodecl::NodeclBase> expr_list = priority.get_arguments_as_expressions(directive);

            if (expr_list.size() != 1)
            {
                warn_printf("%s: warning: ignoring invalid 'priority' clause in 'task' construct\n",
                        directive.get_locus().c_str());
            }
            else
            {
                execution_environment.append(
                        Nodecl::OpenMP::Priority::make(
                            expr_list[0],
                            directive.get_filename(),
                            directive.get_line()));
            }
        }

        // Attach the implicit flushes at the entry and exit of the task (for analysis purposes)
        execution_environment.append(
                Nodecl::OpenMP::FlushAtEntry::make(
                    directive.get_filename(),
                    directive.get_line())
        );
        execution_environment.append(
                Nodecl::OpenMP::FlushAtExit::make(
                    directive.get_filename(),
                    directive.get_line())
        );

        // Label task (this is used only for instrumentation)
        PragmaCustomClause label_clause = pragma_line.get_clause("label");
        if (label_clause.is_defined())
        {
            TL::ObjectList<Nodecl::NodeclBase> expr_list = priority.get_arguments_as_expressions(directive);

            if (expr_list.size() != 1)
            {
                warn_printf("%s: warning: ignoring invalid 'label' clause in 'task' construct\n",
                        directive.get_locus().c_str());
            }
            else
            {
                execution_environment.append(
                        Nodecl::OpenMP::TaskLabel::make(
                            expr_list[0],
                            directive.get_filename(),
                            directive.get_line()));
            }
        }

        Nodecl::NodeclBase async_code =
                    Nodecl::OpenMP::Task::make(execution_environment,
                        directive.get_statements().shallow_copy(),
                        directive.get_filename(),
                        directive.get_line());

        async_code = honour_if_clause(pragma_line, directive, async_code);

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
                    directive.get_filename(),
                    directive.get_line())
        );
        execution_environment.append(
                Nodecl::OpenMP::FlushAtExit::make(
                    directive.get_filename(),
                    directive.get_line())
        );

        Nodecl::NodeclBase parallel_code = Nodecl::OpenMP::Parallel::make(
                    execution_environment,
                    num_threads,
                    directive.get_statements().shallow_copy(),
                    directive.get_filename(),
                    directive.get_line());

        parallel_code = honour_if_clause(pragma_line, directive, parallel_code);

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
                        directive.get_filename(),
                        directive.get_line())
            );

            execution_environment.append(
                    Nodecl::OpenMP::BarrierAtEnd::make(
                        directive.get_filename(),
                        directive.get_line()));
        }

        Nodecl::List code;
        code.append(
                Nodecl::OpenMP::Single::make(
                    execution_environment,
                    directive.get_statements().shallow_copy(),
                    directive.get_filename(),
                    directive.get_line()));

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
                        directive.get_filename(),
                        directive.get_line())
            );
            execution_environment.append(
                    Nodecl::OpenMP::BarrierAtEnd::make(
                        directive.get_filename(),
                        directive.get_line()));
        }

        if (is_combined_worksharing)
        {
            execution_environment.append(
                    Nodecl::OpenMP::CombinedWorksharing::make(
                        directive.get_filename(),
                        directive.get_line()));
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
                        p.get_filename(),
                        p.get_line()));
        }

        Nodecl::OpenMP::Sections sections =
            Nodecl::OpenMP::Sections::make(
                    execution_environment,
                    section_list,
                    directive.get_filename(),
                    directive.get_line());

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
                if (schedule == "static")
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

            if (schedule == "static"
                    || schedule == "dynamic"
                    || schedule == "guided"
                    || schedule == "runtime"
                    || schedule == "auto")
            {
                execution_environment.append(
                        Nodecl::OpenMP::Schedule::make(
                            chunk,
                            schedule,
                            directive.get_filename(),
                            directive.get_line()));
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
                        directive.get_filename(),
                        directive.get_line()));
        }

        if (barrier_at_end)
        {
            execution_environment.append(
                    Nodecl::OpenMP::FlushAtExit::make(
                        directive.get_filename(),
                        directive.get_line())
            );

            execution_environment.append(
                    Nodecl::OpenMP::BarrierAtEnd::make(
                        directive.get_filename(),
                        directive.get_line()));
        }

        if (is_combined_worksharing)
        {
            execution_environment.append(
                    Nodecl::OpenMP::CombinedWorksharing::make(
                        directive.get_filename(),
                        directive.get_line()));
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
                            for_statement.get_filename(),
                            for_statement.get_line())),
                    for_statement.get_statement(),
                    directive.get_filename(),
                    directive.get_line());

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
                    directive.get_filename(),
                    directive.get_line())
        );
        execution_environment.append(
                Nodecl::OpenMP::FlushAtExit::make(
                    directive.get_filename(),
                    directive.get_line())
        );

        Nodecl::NodeclBase code = loop_handler_post(directive, statement, /* barrier_at_end */ false, /* is_combined_worksharing */ true);

        Nodecl::NodeclBase parallel_code
            = Nodecl::OpenMP::Parallel::make(
                execution_environment,
                num_threads,
                code,
                directive.get_filename(),
                directive.get_line());

        parallel_code = honour_if_clause(pragma_line, directive, parallel_code);

        directive.replace(parallel_code);
    }

    // Function tasks
    void Base::task_handler_pre(TL::PragmaCustomDeclaration declaration) { }
    void Base::task_handler_post(TL::PragmaCustomDeclaration decl)
    {
        Nodecl::Utils::remove_from_enclosing_list(decl);
    }

    void Base::target_handler_pre(TL::PragmaCustomStatement stmt)   { }
    void Base::target_handler_pre(TL::PragmaCustomDeclaration decl) { }

    void Base::target_handler_post(TL::PragmaCustomStatement stmt)
    {
        stmt.replace(stmt.get_statements());
    }

    void Base::target_handler_post(TL::PragmaCustomDeclaration decl)
    {
        if (decl.get_nested_pragma().is_null())
        {
            Nodecl::NodeclBase result;
            ObjectList<Nodecl::NodeclBase> devices;
            ObjectList<Nodecl::NodeclBase> symbols;

            int line = decl.get_line();
            std::string file = decl.get_filename();

            PragmaCustomLine pragma_line = decl.get_pragma_line();
            PragmaCustomClause device_clause = pragma_line.get_clause("device");
            if (device_clause.is_defined())
            {
                ObjectList<std::string> device_names = device_clause.get_tokenized_arguments();
                for (ObjectList<std::string>::iterator it = device_names.begin();
                        it != device_names.end();
                        ++it)
                {
                    devices.append(Nodecl::Text::make(*it, file, line));
                }
            }

            ERROR_CONDITION(!decl.has_symbol(),
                    "%s: expecting a function declaration or definition", decl.get_locus().c_str());

            Symbol sym = decl.get_symbol();

            ERROR_CONDITION(!sym.is_function(),
                    "%s: the '%s' symbol is not a function", decl.get_locus().c_str(), sym.get_name().c_str());

            symbols.append(Nodecl::Symbol::make(sym, file, line));

            Nodecl::NodeclBase function_code = sym.get_function_code();
            if (!function_code.is_null())
            {
                result = Nodecl::OpenMP::TargetDefinition::make(
                        Nodecl::List::make(devices),
                        Nodecl::List::make(symbols),
                        file, line);
            }
            else
            {
                result = Nodecl::OpenMP::TargetDeclaration::make(
                        Nodecl::List::make(devices),
                        Nodecl::List::make(symbols),
                        file, line);
            }

            decl.replace(result);
        }
        else
        {
            Nodecl::Utils::remove_from_enclosing_list(decl);
        }
    }

    // SIMD For Statement
    void Base::simd_handler_pre(TL::PragmaCustomStatement) { }
    void Base::simd_handler_post(TL::PragmaCustomStatement stmt) 
    {
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
                node.append_sibling(epilog);
            }
        }

        // Remove #pragma
        stmt.replace(statements);
    }
    
    // SIMD Functions
    void Base::simd_handler_pre(TL::PragmaCustomDeclaration decl) { }
    void Base::simd_handler_post(TL::PragmaCustomDeclaration decl) 
    {
        if (_simd_enabled)
        {
            ERROR_CONDITION(!decl.has_symbol(), "Expecting a function definition here (1)", 0);

            TL::Symbol sym = decl.get_symbol();
            ERROR_CONDITION(!sym.is_function(), "Expecting a function definition here (2)", 0);

            Nodecl::NodeclBase node = sym.get_function_code();
            ERROR_CONDITION(!node.is<Nodecl::FunctionCode>(), "Expecting a function definition here (3)", 0);
            Nodecl::FunctionCode function_code = node.as<Nodecl::FunctionCode>();

            Nodecl::FunctionCode vectorized_func_code = 
                Nodecl::Utils::deep_copy(function_code, function_code).as<Nodecl::FunctionCode>();

            // Vectorize function
            _vectorizer.vectorize(vectorized_func_code, 
                    "smp", 16, NULL); 

            // Set new name
            std::string vectorized_func_name = 
                "__" + sym.get_name() + "_sse_16" ; // + device + vectorlength

            vectorized_func_code.get_symbol().set_name(vectorized_func_name);

            // Add SIMD version to vector function versioning
            _vectorizer.add_vector_function_version(sym.get_name(), vectorized_func_code, 
                    "smp", 16, NULL, TL::Vectorization::SIMD_FUNC_PRIORITY);

            // Append vectorized function code to scalar function
            function_code.append_sibling(vectorized_func_code);
        }
        // Remove #pragma
        Nodecl::Utils::remove_from_enclosing_list(decl);
    }

    void Base::simd_for_handler_pre(TL::PragmaCustomStatement) { }
    void Base::simd_for_handler_post(TL::PragmaCustomStatement stmt) 
    {
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

            // omp for
            PragmaCustomLine pragma_line = stmt.get_pragma_line();
            bool barrier_at_end = !pragma_line.get_clause("nowait").is_defined();

            Nodecl::NodeclBase code = loop_handler_post(
                    stmt, node, barrier_at_end, /* is_combined_worksharing */ false);

            //Nodecl::Utils::remove_from_enclosing_list(node);
            //ast_list_node2.push_back(code);

            // Removing #pragma
            stmt.replace(node);
            //stmt.replace(statements);
        }
        else
        {
            // Remove #pragma
            stmt.replace(statements);
        }
    }

    void Base::simd_for_handler_pre(TL::PragmaCustomDeclaration decl) { }
    void Base::simd_for_handler_post(TL::PragmaCustomDeclaration decl) { }

    void Base::sections_handler_pre(TL::PragmaCustomStatement) { }
    void Base::sections_handler_post(TL::PragmaCustomStatement directive)
    {
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        bool barrier_at_end = !pragma_line.get_clause("nowait").is_defined();

        Nodecl::NodeclBase sections_code = sections_handler_common(directive,
                directive.get_statements(),
                barrier_at_end,
                /* is_combined_worksharing */ false);
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
                        directive.get_locus().c_str());
            }
            else
            {
                openmp_construct = Nodecl::IfElseStatement::make(
                        expr_list[0],
                        Nodecl::List::make(openmp_construct),
                        directive.get_statements().shallow_copy(),
                        directive.get_filename(),
                        directive.get_line());
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
                    directive.get_filename(),
                    directive.get_line())
        );
        execution_environment.append(
                Nodecl::OpenMP::FlushAtExit::make(
                    directive.get_filename(),
                    directive.get_line())
        );

        Nodecl::NodeclBase parallel_code
            = Nodecl::OpenMP::Parallel::make(
                execution_environment,
                num_threads,
                sections_code,
                directive.get_filename(),
                directive.get_line());

        parallel_code = honour_if_clause(pragma_line, directive, parallel_code);

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
                    directive.get_filename(),
                    directive.get_line())
        );
        execution_environment.append(
                Nodecl::OpenMP::FlushAtExit::make(
                    directive.get_filename(),
                    directive.get_line())
        );

        Nodecl::NodeclBase code = loop_handler_post(directive, statement, /* barrier_at_end */ false, /* is_combined_worksharing */ true);

        Nodecl::NodeclBase parallel_code
            = Nodecl::OpenMP::Parallel::make(
                    execution_environment,
                    num_threads,
                    code,
                    directive.get_filename(),
                    directive.get_line());

        parallel_code = honour_if_clause(pragma_line, directive, parallel_code);

        directive.replace(parallel_code);
    }

    void Base::threadprivate_handler_pre(TL::PragmaCustomDirective) { }
    void Base::threadprivate_handler_post(TL::PragmaCustomDirective directive)
    {
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

        Nodecl::Utils::remove_from_enclosing_list(directive);
    }

    // Remove
    void Base::declare_reduction_handler_pre(TL::PragmaCustomDirective)
    {
    }


    void Base::declare_reduction_handler_post(TL::PragmaCustomDirective)
    {
    }

    struct SymbolBuilder : TL::Functor<Nodecl::NodeclBase, Symbol>
    {
        private:
            std::string _filename;
            int _line;

        public:
            SymbolBuilder(const std::string& filename, int line)
                : _filename(filename), _line(line)
            {
            }

            virtual Nodecl::NodeclBase do_(ArgType arg) const
            {
                return Nodecl::Symbol::make(arg, _filename, _line);
            }
    };

    struct ReductionSymbolBuilder : TL::Functor<Nodecl::NodeclBase, ReductionSymbol>
    {
        private:
            std::string _filename;
            int _line;

        public:
            ReductionSymbolBuilder(const std::string& filename, int line)
                : _filename(filename), _line(line)
            {
            }

            virtual Nodecl::NodeclBase do_(ArgType arg) const
            {
                return Nodecl::OpenMP::ReductionItem::make(
                        /* reductor */ Nodecl::Symbol::make(arg.get_udr().get_symbol_holder(), _filename, _line),
                        /* reduced symbol */ Nodecl::Symbol::make(arg.get_symbol(), _filename, _line),
                        _filename, _line);
            }
    };

    template <typename T>
    static void make_data_sharing_list(
            OpenMP::DataSharingEnvironment &data_sharing_env,
            OpenMP::DataSharingAttribute data_attr,
            const std::string& filename, int line,
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
            TL::ObjectList<Nodecl::NodeclBase> nodecl_symbols = symbols.map(SymbolBuilder(filename, line));

            result_list.append(T::make(Nodecl::List::make(nodecl_symbols), filename, line));
        }
    }

    Nodecl::List Base::make_execution_environment_for_combined_worksharings(OpenMP::DataSharingEnvironment &data_sharing_env, PragmaCustomLine pragma_line)
    {
        int line = pragma_line.get_line();
        std::string filename = pragma_line.get_filename();

        TL::ObjectList<Nodecl::NodeclBase> result_list;

        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_SHARED,
                filename, line,
                result_list);
        // Everything should go transparent here
        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_PRIVATE,
                filename, line,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_FIRSTPRIVATE,
                filename, line,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_LASTPRIVATE,
                filename, line,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_FIRSTLASTPRIVATE,
                filename, line,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_AUTO,
                filename, line,
                result_list);

        TL::ObjectList<ReductionSymbol> reductions;
        data_sharing_env.get_all_reduction_symbols(reductions);
        TL::ObjectList<Symbol> reduction_symbols = reductions.map(functor(&ReductionSymbol::get_symbol));
        if (!reduction_symbols.empty())
        {
            TL::ObjectList<Nodecl::NodeclBase> nodecl_symbols =
                reduction_symbols.map(SymbolBuilder(filename, line));

            result_list.append(Nodecl::OpenMP::Shared::make(Nodecl::List::make(nodecl_symbols),
                        filename, line));
        }


        // Build the tree which contains the target information
        TargetInfo target_info = data_sharing_env.get_target_info();

        TL::ObjectList<Nodecl::NodeclBase> devices;
        TL::ObjectList<Nodecl::NodeclBase> target_items;

        ObjectList<Nodecl::NodeclBase> ndrange_exprs = target_info.get_ndrange();
        if (!ndrange_exprs.empty())
        {
            target_items.append(
                    Nodecl::OpenMP::NDRange::make(
                        Nodecl::List::make(ndrange_exprs),
                        filename, line));
        }

        ObjectList<std::string> device_list = target_info.get_device_list();
        for (TL::ObjectList<std::string>::iterator it = device_list.begin(); it != device_list.end(); ++it)
        {
            devices.append(Nodecl::Text::make(*it, filename, line));
        }

        result_list.append(
                Nodecl::OpenMP::Target::make(
                    Nodecl::List::make(devices),
                    Nodecl::List::make(target_items),
                    filename, line));


        // FIXME - Dependences and copies for combined worksharings???
        //
        // TL::ObjectList<OpenMP::DependencyItem> dependences;
        // data_sharing_env.get_all_dependences(dependences);

        // make_dependency_list<Nodecl::OpenMP::DepIn>(
        //         dependences,
        //         OpenMP::DEP_DIR_IN,
        //         pragma_line.get_filename(), pragma_line.get_line(),
        //         result_list);

        // make_dependency_list<Nodecl::OpenMP::DepOut>(
        //         dependences,
        //         OpenMP::DEP_DIR_OUT,
        //         pragma_line.get_filename(), pragma_line.get_line(),
        //         result_list);

        // make_dependency_list<Nodecl::OpenMP::DepInout>(
        //         dependences, OpenMP::DEP_DIR_INOUT,
        //         pragma_line.get_filename(), pragma_line.get_line(),
        //         result_list);

        // TL::ObjectList<OpenMP::CopyItem> copies;
        // data_sharing_env.get_all_copies(copies);

        // make_copy_list<Nodecl::OpenMP::CopyIn>(
        //         copies,
        //         OpenMP::COPY_DIR_IN,
        //         pragma_line.get_filename(), pragma_line.get_line(),
        //         result_list);

        // make_copy_list<Nodecl::OpenMP::CopyOut>(
        //         copies,
        //         OpenMP::COPY_DIR_IN,
        //         pragma_line.get_filename(), pragma_line.get_line(),
        //         result_list);

        // make_copy_list<Nodecl::OpenMP::CopyInout>(
        //         copies,
        //         OpenMP::COPY_DIR_INOUT,
        //         pragma_line.get_filename(), pragma_line.get_line(),
        //         result_list);

        return Nodecl::List::make(result_list);
    }

    Nodecl::List Base::make_execution_environment(OpenMP::DataSharingEnvironment &data_sharing_env, PragmaCustomLine pragma_line)
    {
        int line = pragma_line.get_line();
        std::string filename = pragma_line.get_filename();

        TL::ObjectList<Nodecl::NodeclBase> result_list;

        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_SHARED,
                filename, line,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Private>(
                data_sharing_env, OpenMP::DS_PRIVATE,
                filename, line,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Firstprivate>(
                data_sharing_env, OpenMP::DS_FIRSTPRIVATE,
                filename, line,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Lastprivate>(
                data_sharing_env, OpenMP::DS_LASTPRIVATE,
                filename, line,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::FirstLastprivate>(
                data_sharing_env, OpenMP::DS_FIRSTLASTPRIVATE,
                filename, line,
                result_list);
        make_data_sharing_list<Nodecl::OpenMP::Auto>(
                data_sharing_env, OpenMP::DS_AUTO,
                filename, line,
                result_list);

        TL::ObjectList<ReductionSymbol> reductions;
        data_sharing_env.get_all_reduction_symbols(reductions);
        if (!reductions.empty())
        {
            TL::ObjectList<Nodecl::NodeclBase> reduction_nodes = reductions.map(ReductionSymbolBuilder(filename, line));

            result_list.append(
                    Nodecl::OpenMP::Reduction::make(Nodecl::List::make(reduction_nodes),
                        filename, line)
                    );
        }

        TL::ObjectList<OpenMP::DependencyItem> dependences;
        data_sharing_env.get_all_dependences(dependences);

        make_dependency_list<Nodecl::OpenMP::DepIn>(
                dependences,
                OpenMP::DEP_DIR_IN,
                filename, line,
                result_list);

        make_dependency_list<Nodecl::OpenMP::DepOut>(
                dependences,
                OpenMP::DEP_DIR_OUT,
                filename, line,
                result_list);

        make_dependency_list<Nodecl::OpenMP::DepInout>(
                dependences, OpenMP::DEP_DIR_INOUT,
                filename, line,
                result_list);

        make_dependency_list<Nodecl::OpenMP::Concurrent>(
                dependences, OpenMP::DEP_CONCURRENT,
                filename, line,
                result_list);

        make_dependency_list<Nodecl::OpenMP::Commutative>(
                dependences, OpenMP::DEP_COMMUTATIVE,
                filename, line,
                result_list);

        // Build the tree which contains the target information
        TargetInfo target_info = data_sharing_env.get_target_info();

        TL::ObjectList<Nodecl::NodeclBase> devices;
        TL::ObjectList<Nodecl::NodeclBase> target_items;

        ObjectList<std::string> device_list = target_info.get_device_list();
        for (TL::ObjectList<std::string>::iterator it = device_list.begin(); it != device_list.end(); ++it)
        {
            devices.append(Nodecl::Text::make(*it, filename, line));
        }

        ObjectList<CopyItem> copy_in = target_info.get_copy_in();
        make_copy_list<Nodecl::OpenMP::CopyIn>(
                copy_in,
                OpenMP::COPY_DIR_IN,
                filename, line,
                target_items);

        ObjectList<CopyItem> copy_out = target_info.get_copy_out();
        make_copy_list<Nodecl::OpenMP::CopyOut>(
                copy_out,
                OpenMP::COPY_DIR_OUT,
                filename, line,
                target_items);

        ObjectList<CopyItem> copy_inout = target_info.get_copy_inout();
        make_copy_list<Nodecl::OpenMP::CopyInout>(
                copy_inout,
                OpenMP::COPY_DIR_INOUT,
                filename, line,
                target_items);

        result_list.append(
                Nodecl::OpenMP::Target::make(
                    Nodecl::List::make(devices),
                    Nodecl::List::make(target_items),
                    filename, line));

        return Nodecl::List::make(result_list);
    }

    } }

EXPORT_PHASE(TL::OpenMP::Base)
