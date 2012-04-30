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
#include "tl-nodecl-alg.hpp"
#include "cxx-diagnostic.h"
#include "cxx-cexpr.h"

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

            data_ref_list.append(it->get_dependency_expression());
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

                        Nodecl::NodeclBase exec_env = this->make_exec_environment(call, task_info);

                        Nodecl::Parallel::AsyncCall async_call = Nodecl::Parallel::AsyncCall::make(
                                exec_env,
                                // Do we need to copy this?
                                call.copy(),
                                call.get_filename(),
                                call.get_line());

                        call.integrate(async_call);
                    }
                }
            }

        private:
            Nodecl::NodeclBase make_exec_environment(const Nodecl::FunctionCall &call, FunctionTaskInfo& function_task_info)
            {
                TL::ObjectList<Nodecl::NodeclBase> result_list;

                TL::ObjectList<FunctionTaskDependency> task_dependences = function_task_info.get_parameter_info();

                make_dependency_list<Nodecl::Parallel::DepIn>(
                        task_dependences, 
                        OpenMP::DEP_DIR_IN,
                        call.get_filename(), 
                        call.get_line(),
                        result_list);

                make_dependency_list<Nodecl::Parallel::DepOut>(
                        task_dependences, 
                        OpenMP::DEP_DIR_OUT,
                        call.get_filename(), 
                        call.get_line(),
                        result_list);

                make_dependency_list<Nodecl::Parallel::DepInout>(
                        task_dependences, 
                        OpenMP::DEP_DIR_INOUT,
                        call.get_filename(), 
                        call.get_line(),
                        result_list);

                return Nodecl::List::make(result_list);
            }
    };

    Base::Base()
        : PragmaCustomCompilerPhase("omp"), _core()
    {
        set_phase_name("OpenMP directive to parallel IR");
        set_phase_description("This phase lowers the semantics of OpenMP into the parallel IR of Mercurium");

#define OMP_DIRECTIVE(_directive, _name) \
                { \
                    dispatcher().directive.pre[_directive].connect(functor(&Base::_name##_handler_pre, *this)); \
                    dispatcher().directive.post[_directive].connect(functor(&Base::_name##_handler_post, *this)); \
                }
#define OMP_CONSTRUCT_COMMON(_directive, _name, _noend) \
                { \
                    dispatcher().declaration.pre[_directive].connect(functor((void (Base::*)(TL::PragmaCustomDeclaration))&Base::_name##_handler_pre, *this)); \
                    dispatcher().declaration.post[_directive].connect(functor((void (Base::*)(TL::PragmaCustomDeclaration))&Base::_name##_handler_post, *this)); \
                    dispatcher().statement.pre[_directive].connect(functor((void (Base::*)(TL::PragmaCustomStatement))&Base::_name##_handler_pre, *this)); \
                    dispatcher().statement.post[_directive].connect(functor((void (Base::*)(TL::PragmaCustomStatement))&Base::_name##_handler_post, *this)); \
                }
#define OMP_CONSTRUCT(_directive, _name) OMP_CONSTRUCT_COMMON(_directive, _name, false)
#define OMP_CONSTRUCT_NOEND(_directive, _name) OMP_CONSTRUCT_COMMON(_directive, _name, true)
#include "tl-omp-constructs.def"
#undef OMP_DIRECTIVE
#undef OMP_CONSTRUCT_COMMON
#undef OMP_CONSTRUCT
#undef OMP_CONSTRUCT_NOEND
    }

    void Base::pre_run(TL::DTO& dto)
    {
        _core.pre_run(dto);
        this->PragmaCustomCompilerPhase::pre_run(dto);
    }

    void Base::run(TL::DTO& dto)
    {
        _core.run(dto);
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
        INVALID_DECLARATION_HANDLER(for)
        INVALID_DECLARATION_HANDLER(do)
        INVALID_DECLARATION_HANDLER(parallel_sections)
        INVALID_DECLARATION_HANDLER(sections)
        INVALID_DECLARATION_HANDLER(single)
        INVALID_DECLARATION_HANDLER(critical)
        INVALID_DECLARATION_HANDLER(atomic)

#define EMPTY_HANDLERS_CONSTRUCT(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomStatement) { } \
        void Base::_name##_handler_post(TL::PragmaCustomStatement) { } \
        void Base::_name##_handler_pre(TL::PragmaCustomDeclaration) { } \
        void Base::_name##_handler_post(TL::PragmaCustomDeclaration) { } \

#define EMPTY_HANDLERS_DIRECTIVE(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomDirective) { } \
        void Base::_name##_handler_post(TL::PragmaCustomDirective) { }

        EMPTY_HANDLERS_CONSTRUCT(master)
        EMPTY_HANDLERS_CONSTRUCT(ordered)
        EMPTY_HANDLERS_CONSTRUCT(parallel_do)

        EMPTY_HANDLERS_DIRECTIVE(section)

        EMPTY_HANDLERS_DIRECTIVE(taskyield)

    void Base::atomic_handler_pre(TL::PragmaCustomStatement) { }
    void Base::atomic_handler_post(TL::PragmaCustomStatement directive)
    {
        Nodecl::Parallel::Atomic atomic =
            Nodecl::Parallel::Atomic::make(
                    Nodecl::NodeclBase::null(),
                    directive.get_statements().copy(),
                    directive.get_filename(),
                    directive.get_line());

        directive.integrate(atomic);
    }

    void Base::critical_handler_pre(TL::PragmaCustomStatement) { }
    void Base::critical_handler_post(TL::PragmaCustomStatement directive)
    {
        TL::PragmaCustomLine pragma_line = directive.get_pragma_line();

        TL::PragmaCustomParameter param = pragma_line.get_parameter();

        Nodecl::NodeclBase exec_env = Nodecl::NodeclBase::null();

        if (param.is_defined())
        {
            ObjectList<std::string> critical_name = param.get_tokenized_arguments();

            exec_env = Nodecl::List::make(
                    Nodecl::Parallel::CriticalName::make(critical_name[0],
                        directive.get_filename(),
                        directive.get_line()));
        }

        directive.integrate(
                Nodecl::Parallel::Exclusive::make(
                    exec_env,
                    directive.get_statements().copy(),
                    directive.get_filename(),
                    directive.get_line()));
    }

    void Base::barrier_handler_pre(TL::PragmaCustomDirective) { }
    void Base::barrier_handler_post(TL::PragmaCustomDirective directive)
    {
        directive.integrate(
                Nodecl::Parallel::BarrierFull::make(directive.get_filename(), directive.get_line())
                );
    }

    void Base::flush_handler_pre(TL::PragmaCustomDirective) { }
    void Base::flush_handler_post(TL::PragmaCustomDirective directive)
    {
        PragmaClauseArgList parameter = directive.get_pragma_line().get_parameter();
        directive.integrate(
                Nodecl::Parallel::FlushMemory::make(
                    parameter.copy(),
                    directive.get_filename(), directive.get_line())
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
            Nodecl::Parallel::DepInout dep_inout = Nodecl::Parallel::DepInout::make(
                    Nodecl::List::make(expr_list),
                    directive.get_filename(),
                    directive.get_line());

            directive.integrate(
                    Nodecl::Parallel::WaitAsyncsDependences::make(
                        Nodecl::List::make(dep_inout),
                        directive.get_filename(), directive.get_line())
                    );
        }
        else
        {
            directive.integrate(
                    Nodecl::Parallel::WaitAsyncsShallow::make(
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
            execution_environment.push_back(
                    Nodecl::Parallel::Untied::make(
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
                execution_environment.push_back(
                        Nodecl::Parallel::Priority::make(
                            expr_list[0],
                            directive.get_filename(),
                            directive.get_line()));
            }
        }

        PragmaCustomClause if_clause = pragma_line.get_clause("if");

        Nodecl::NodeclBase async_code =
                    Nodecl::Parallel::Async::make(execution_environment,
                        directive.get_statements().copy(),
                        directive.get_filename(),
                        directive.get_line());

        // Honour if-clause
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
                async_code = Nodecl::IfElseStatement::make(
                        expr_list[0],
                        Nodecl::List::make(async_code),
                        directive.get_statements().copy(),
                        directive.get_filename(),
                        directive.get_line());
            }
        }
        directive.integrate(async_code);
    }

    void Base::parallel_handler_pre(TL::PragmaCustomStatement) { }
    void Base::parallel_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment(ds, pragma_line);

        Nodecl::NodeclBase number_of_replicas;
        PragmaCustomClause clause = pragma_line.get_clause("num_threads");
        if (clause.is_defined())
        {
            ObjectList<Nodecl::NodeclBase> args = clause.get_arguments_as_expressions();

            // Let core check this for us
            ERROR_CONDITION (args.size() != 1, "num_threads wrong clause", 0);

            number_of_replicas = args[0];
        }

        directive.integrate(
                Nodecl::Parallel::Replicate::make(
                    Nodecl::NodeclBase::null(),
                    number_of_replicas,
                    Nodecl::Parallel::Async::make(
                        execution_environment,
                        directive.get_statements().copy(),
                        directive.get_filename(),
                        directive.get_line()),
                    directive.get_filename(),
                    directive.get_line()));
    }

    void Base::single_handler_pre(TL::PragmaCustomStatement) { }
    void Base::single_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment(ds, pragma_line);

        Nodecl::List code;
        code.push_back(
                Nodecl::Parallel::Single::make(
                    execution_environment,
                    directive.get_statements().copy(),
                    directive.get_filename(),
                    directive.get_line()));

        if (!pragma_line.get_clause("nowait").is_defined())
        {
            code.push_back(
                    Nodecl::Parallel::BarrierFull::make(
                        directive.get_filename(),
                        directive.get_line()));
        }

        directive.integrate(code);
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

        loop_handler_post(directive, statement);
    }

    void Base::loop_handler_pre(TL::PragmaCustomStatement) { }
    void Base::loop_handler_post(TL::PragmaCustomStatement directive, Nodecl::NodeclBase statement)
    {
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();
        Nodecl::List execution_environment = this->make_execution_environment(ds, pragma_line);
        Nodecl::List distribute_environment;

        if (pragma_line.get_clause("schedule").is_defined())
        {
            PragmaCustomClause clause = pragma_line.get_clause("schedule");

            ObjectList<std::string> arguments = clause.get_tokenized_arguments();

            Nodecl::NodeclBase chunk;

            if (arguments.size() == 1)
            {
                chunk = const_value_to_nodecl(const_value_get_signed_int(1));
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

            std::string schedule = arguments[0];
            schedule = strtolower(schedule.c_str());

            if (schedule == "static"
                    || schedule == "dynamic"
                    || schedule == "guided"
                    || schedule == "runtime"
                    || schedule == "auto")
            {
            distribute_environment.push_back(
                    Nodecl::Parallel::Schedule::make(
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
            distribute_environment.push_back(
                    Nodecl::Parallel::Schedule::make(
                        ::const_value_to_nodecl(const_value_get_signed_int(1)),
                        "static",
                        directive.get_filename(),
                        directive.get_line()));
        }

        if (!pragma_line.get_clause("nowait").is_defined())
        {
            distribute_environment.push_back(
                    Nodecl::Parallel::BarrierAtEnd::make(
                        directive.get_filename(),
                        directive.get_line()));
        }

        ERROR_CONDITION (!statement.is<Nodecl::ForStatement>(), "Invalid tree", 0);
        TL::ForStatement for_statement(statement.as<Nodecl::ForStatement>());

        Nodecl::List code;
        Nodecl::Parallel::Distribute distribute =
            Nodecl::Parallel::Distribute::make(
                    distribute_environment,
                    // This is a list because of multidimensional distribution
                    Nodecl::List::make(
                        Nodecl::Parallel::DistributeRange::make(
                            for_statement.get_lower_bound(),
                            for_statement.get_upper_bound(),
                            for_statement.get_step(),
                            for_statement.get_induction_variable(),
                            for_statement.get_filename(),
                            for_statement.get_line())),
                    Nodecl::Parallel::Async::make(
                        execution_environment,
                        for_statement.get_statement(),
                        for_statement.get_filename(),
                        for_statement.get_line()),
                    directive.get_filename(),
                    directive.get_line());

        code.push_back(distribute);

        directive.integrate(code);
    }

    void Base::do_handler_pre(TL::PragmaCustomStatement directive)
    {
        loop_handler_pre(directive);
    }

    void Base::do_handler_post(TL::PragmaCustomStatement directive)
    {
        Nodecl::NodeclBase statement = directive.get_statements();
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();

        loop_handler_post(directive, statement);
    }

    // Function tasks
    void Base::task_handler_pre(TL::PragmaCustomDeclaration declaration) { }
    void Base::task_handler_post(TL::PragmaCustomDeclaration decl)
    {
        Nodecl::Utils::remove_from_enclosing_list(decl);
    }

    void Base::target_handler_pre(TL::PragmaCustomStatement) { }
    void Base::target_handler_pre(TL::PragmaCustomDeclaration decl) { }

    void Base::target_handler_post(TL::PragmaCustomStatement stmt)
    {
        stmt.integrate(stmt.get_statements());
    }

    void Base::target_handler_post(TL::PragmaCustomDeclaration decl)
    {
        Nodecl::Utils::remove_from_enclosing_list(decl);
    }

    void Base::sections_handler_pre(TL::PragmaCustomStatement) { }
    void Base::sections_handler_post(TL::PragmaCustomStatement directive)
    {
#if 0
        OpenMP::DataSharingEnvironment &ds = _core.get_openmp_info()->get_data_sharing(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment(ds, pragma_line);

        Nodecl::List tasks = directive.get_statements().as<Nodecl::List>();
        ObjectList<Nodecl::NodeclBase> new_tasks;

        for (Nodecl::List::iterator it = tasks.begin();
                it != tasks.end();
                it++)
        {
            // We should check that they are pragma section
            ERROR_CONDITION((!it->is<Nodecl::PragmaCustomStatement>()), "Invalid node found", 0);

            Nodecl::PragmaCustomStatement p = it->as<Nodecl::PragmaCustomStatement>();

            new_tasks.append( Nodecl::Parallel::Async::make(
                        execution_environment.copy(),
                        p.get_statements().copy(),
                        it->get_filename(),
                        it->get_line()) );
        }

        Nodecl::List set_of_tasks = Nodecl::List::make(new_tasks);

        directive.integrate(
                Nodecl::Parallel::Composite::make(
                    Nodecl::NodeclBase::null(),
                    set_of_tasks,
                    directive.get_filename(),
                    directive.get_line()));
#endif
    }

    void Base::parallel_for_handler_pre(TL::PragmaCustomStatement)
    {
    }

    void Base::parallel_for_handler_post(TL::PragmaCustomStatement)
    {
    }

    // Keep these
    void Base::threadprivate_handler_pre(TL::PragmaCustomDirective) { }
    void Base::threadprivate_handler_post(TL::PragmaCustomDirective) { }

    // Remove
    void Base::declare_reduction_handler_pre(TL::PragmaCustomDirective)
    {
    }

    void Base::parallel_sections_handler_pre(TL::PragmaCustomStatement)
    {
    }

    void Base::declare_reduction_handler_post(TL::PragmaCustomDirective)
    {
    }

    void Base::parallel_sections_handler_post(TL::PragmaCustomStatement)
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
                return Nodecl::Parallel::ReductionItem::make(
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

        if (!symbols.empty())
        {
            TL::ObjectList<Nodecl::NodeclBase> nodecl_symbols = symbols.map(SymbolBuilder(filename, line));

            result_list.append(T::make(Nodecl::List::make(nodecl_symbols), filename, line));
        }
    }

    Nodecl::List Base::make_execution_environment(OpenMP::DataSharingEnvironment &data_sharing_env, PragmaCustomLine pragma_line)
    {
        TL::ObjectList<Nodecl::NodeclBase> result_list;

        make_data_sharing_list<Nodecl::Parallel::Shared>(
                data_sharing_env, OpenMP::DS_SHARED,
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);
        make_data_sharing_list<Nodecl::Parallel::Private>(
                data_sharing_env, OpenMP::DS_PRIVATE,
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);
        make_data_sharing_list<Nodecl::Parallel::Capture>(
                data_sharing_env, OpenMP::DS_FIRSTPRIVATE,
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);

        TL::ObjectList<ReductionSymbol> reductions;
        data_sharing_env.get_all_reduction_symbols(reductions);
        if (!reductions.empty())
        {
            TL::ObjectList<Nodecl::NodeclBase> reduction_nodes = reductions.map(ReductionSymbolBuilder(pragma_line.get_filename(), pragma_line.get_line()));

            result_list.append(
                    Nodecl::Parallel::Reduction::make(Nodecl::List::make(reduction_nodes),
                        pragma_line.get_filename(), pragma_line.get_line())
                    );
        }

        TL::ObjectList<OpenMP::DependencyItem> dependences;
        data_sharing_env.get_all_dependences(dependences);

        make_dependency_list<Nodecl::Parallel::DepIn>(
                dependences,
                OpenMP::DEP_DIR_IN,
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);

        make_dependency_list<Nodecl::Parallel::DepOut>(
                dependences,
                OpenMP::DEP_DIR_OUT,
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);

        make_dependency_list<Nodecl::Parallel::DepInout>(
                dependences, OpenMP::DEP_DIR_INOUT,
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);

        TL::ObjectList<OpenMP::CopyItem> copies;
        data_sharing_env.get_all_copies(copies);

        make_copy_list<Nodecl::Parallel::CopyIn>(
                copies,
                OpenMP::COPY_DIR_IN,
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);

        make_copy_list<Nodecl::Parallel::CopyOut>(
                copies,
                OpenMP::COPY_DIR_IN,
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);

        make_copy_list<Nodecl::Parallel::CopyInout>(
                copies,
                OpenMP::COPY_DIR_INOUT,
                pragma_line.get_filename(), pragma_line.get_line(),
                result_list);

        return Nodecl::List::make(result_list);
    }
    } }

EXPORT_PHASE(TL::OpenMP::Base)
