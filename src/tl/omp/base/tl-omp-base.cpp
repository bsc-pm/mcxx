/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "tl-omp-base.hpp"
#include "tl-ompss-base-task.hpp"
#include "tl-omp-base-utils.hpp"

#include "config.h"
#include "tl-nodecl-utils.hpp"
#include "tl-predicateutils.hpp"
#include "tl-counters.hpp"
#include "tl-compilerpipeline.hpp"

#include "cxx-diagnostic.h"
#include "cxx-cexpr.h"
#include "fortran03-scope.h"

#include <algorithm>
#include <iterator>

namespace TL { namespace OpenMP {

    namespace Report
    {
        const char indent[] = "  ";
    }

    Base::Base()
        : PragmaCustomCompilerPhase(),
        _core(),
        _simd_enabled(false),
        _ompss_mode(false),
        _omp_report(false),
        _copy_deps_by_default(true),
        _untied_tasks_by_default(true)
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
                "0").connect(std::bind(&Base::set_discard_unused_data_sharings, this, std::placeholders::_1));

        register_parameter("simd_enabled",
                "If set to '1' enables simd constructs, otherwise it is disabled",
                _simd_enabled_str,
                "0").connect(std::bind(&Base::set_simd, this, std::placeholders::_1));

        register_parameter("allow_shared_without_copies",
                "If set to '1' allows shared without any copy directionality, otherwise they are set to copy_inout",
                _allow_shared_without_copies_str,
                "0").connect(std::bind(&Base::set_allow_shared_without_copies, this, std::placeholders::_1));

        register_parameter("allow_array_reductions",
                "If set to '1' enables extended support for array reductions in C/C++",
                _allow_array_reductions_str,
                "1").connect(std::bind(&Base::set_allow_array_reductions, this, std::placeholders::_1));

        register_parameter("ompss_mode",
                "Enables OmpSs semantics instead of OpenMP semantics",
                _ompss_mode_str,
                "0").connect(std::bind(&Base::set_ompss_mode, this, std::placeholders::_1));

        register_parameter("omp_report",
                "Emits an OpenMP report describing the OpenMP semantics of the code",
                _omp_report_str,
                "0").connect(std::bind(&Base::set_omp_report_parameter, this, std::placeholders::_1));

        register_parameter("copy_deps_by_default",
                "Enables copy_deps by default",
                _copy_deps_str,
                "1").connect(std::bind(&Base::set_copy_deps_by_default, this, std::placeholders::_1));

        register_parameter("untied_tasks_by_default",
                "If set to '1' tasks are untied by default, otherwise they are tied. This flag is only valid in OmpSs",
                _untied_tasks_by_default_str,
                "1").connect(std::bind(&Base::set_untied_tasks_by_default, this, std::placeholders::_1));

        register_parameter("disable_task_expression_optimization",
                "Disables some optimizations applied to task expressions",
                _disable_task_expr_optim_str,
                "0");

        register_parameter("enable_input_by_value_dependences",
                "Enables input by value experimental dependences",
                _enable_input_by_value_dependences,
                "0").connect(std::bind(&Base::set_enable_input_by_value_dependences, this, std::placeholders::_1));

        register_parameter("enable_nonvoid_function_tasks",
                "Enables experimental nonvoid function tasks (Only for C/C++)",
                _enable_nonvoid_function_tasks,
                "0").connect(std::bind(&Base::set_enable_nonvoid_function_tasks, this, std::placeholders::_1));

        register_omp();
        register_ompss();
    }

    void Base::register_omp()
    {
#define OMP_DIRECTIVE(_directive, _name, _pred) \
                if (_pred) { \
                    std::string directive_name = remove_separators_of_directive(_directive); \
                    dispatcher("omp").directive.pre[directive_name].connect(std::bind(&Base::_name##_handler_pre, this, std::placeholders::_1)); \
                    dispatcher("omp").directive.post[directive_name].connect(std::bind(&Base::_name##_handler_post, this, std::placeholders::_1)); \
                }
#define OMP_CONSTRUCT_COMMON(_directive, _name, _noend, _pred) \
                if (_pred) { \
                    std::string directive_name = remove_separators_of_directive(_directive); \
                    dispatcher("omp").declaration.pre[directive_name].connect(std::bind((void (Base::*)(TL::PragmaCustomDeclaration))&Base::_name##_handler_pre, this, std::placeholders::_1)); \
                    dispatcher("omp").declaration.post[directive_name].connect(std::bind((void (Base::*)(TL::PragmaCustomDeclaration))&Base::_name##_handler_post, this, std::placeholders::_1)); \
                    dispatcher("omp").statement.pre[directive_name].connect(std::bind((void (Base::*)(TL::PragmaCustomStatement))&Base::_name##_handler_pre, this, std::placeholders::_1)); \
                    dispatcher("omp").statement.post[directive_name].connect(std::bind((void (Base::*)(TL::PragmaCustomStatement))&Base::_name##_handler_post, this, std::placeholders::_1)); \
                }
#define OMP_CONSTRUCT(_directive, _name, _pred) OMP_CONSTRUCT_COMMON(_directive, _name, false, _pred)
#define OMP_CONSTRUCT_NOEND(_directive, _name, _pred) OMP_CONSTRUCT_COMMON(_directive, _name, true, _pred)
#include "tl-omp-constructs.def"
#undef OMP_DIRECTIVE
#undef OMP_CONSTRUCT_COMMON
#undef OMP_CONSTRUCT
#undef OMP_CONSTRUCT_NOEND
    }

    void Base::register_ompss()
    {
        // OSS constructs
        dispatcher("oss").directive.pre["taskwait"].connect(std::bind(&Base::taskwait_handler_pre, this, std::placeholders::_1));
        dispatcher("oss").directive.post["taskwait"].connect(std::bind(&Base::taskwait_handler_post, this, std::placeholders::_1));

        dispatcher("oss").declaration.pre["task"].connect(
                std::bind((void (Base::*)(TL::PragmaCustomDeclaration))&Base::task_handler_pre, this, std::placeholders::_1));
        dispatcher("oss").declaration.post["task"].connect(
                std::bind((void (Base::*)(TL::PragmaCustomDeclaration))&Base::task_handler_post, this, std::placeholders::_1));

        dispatcher("oss").statement.pre["task"].connect(
                std::bind((void (Base::*)(TL::PragmaCustomStatement))&Base::task_handler_pre, this, std::placeholders::_1));
        dispatcher("oss").statement.post["task"].connect(
                std::bind((void (Base::*)(TL::PragmaCustomStatement))&Base::task_handler_post, this, std::placeholders::_1));

        dispatcher("oss").statement.pre["critical"].connect(
                std::bind((void (Base::*)(TL::PragmaCustomStatement))&Base::critical_handler_pre, this, std::placeholders::_1));
        dispatcher("oss").statement.post["critical"].connect(
                std::bind((void (Base::*)(TL::PragmaCustomStatement))&Base::critical_handler_post, this, std::placeholders::_1));
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
        if (CURRENT_CONFIGURATION->explicit_instantiation)
        {
            this->set_ignore_template_functions(true);
        }

        _core.run(dto);

        if (diagnostics_get_error_count() != 0)
            return;

        // Do nothing once we have analyzed everything
        if (_openmp_dry_run != "0")
            return;

        if (emit_omp_report())
        {
            TL::CompiledFile current = TL::CompilationProcess::get_current_file();
            std::string report_filename = current.get_filename() + "." +
                    std::string(in_ompss_mode() ? "ompss.report" : "openmp.report");

            info_printf_at(
                    ::make_locus(current.get_filename().c_str(), 0, 0),
                    "creating %s report in '%s'\n",
                    in_ompss_mode() ? "OmpSs" : "OpenMP",
                    report_filename.c_str());

            _omp_report_file = new std::ofstream(report_filename.c_str());
            *_omp_report_file
                << (in_ompss_mode() ? "OmpSs " : "OpenMP ") << "Report for file '" << current.get_filename() << "'\n"
                << "=================================================================\n";
        }

        this->PragmaCustomCompilerPhase::run(dto);

        std::shared_ptr<TL::OmpSs::FunctionTaskSet> function_task_set =
            std::static_pointer_cast<TL::OmpSs::FunctionTaskSet>(dto["openmp_task_info"]);

        Nodecl::NodeclBase translation_unit = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);

        bool task_expr_optim_disabled = (_disable_task_expr_optim_str == "1");
        OmpSs::TransformNonVoidFunctionCalls transform_nonvoid_task_calls(function_task_set, task_expr_optim_disabled,
                /* ignore_template_functions */ CURRENT_CONFIGURATION->explicit_instantiation);
        transform_nonvoid_task_calls.walk(translation_unit);
        transform_nonvoid_task_calls.remove_nonvoid_function_tasks_from_function_task_set();

        const std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& funct_call_to_enclosing_stmt_map =
            transform_nonvoid_task_calls.get_function_call_to_enclosing_stmt_map();

        const std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& enclosing_stmt_to_original_stmt_map =
            transform_nonvoid_task_calls.get_enclosing_stmt_to_original_stmt_map();

        const std::map<Nodecl::NodeclBase, std::set<TL::Symbol> >& enclosing_stmt_to_return_vars_map =
            transform_nonvoid_task_calls.get_enclosing_stmt_to_return_variables_map();

        OmpSs::FunctionCallVisitor function_call_visitor(
                function_task_set,
                funct_call_to_enclosing_stmt_map,
                enclosing_stmt_to_original_stmt_map,
                enclosing_stmt_to_return_vars_map,
                this,
                /* ignore_template_functions */ CURRENT_CONFIGURATION->explicit_instantiation);

        function_call_visitor.walk(translation_unit);
        function_call_visitor.build_all_needed_task_expressions();

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n=================================================================\n"
                << "End of report\n"
                << std::endl;
            _omp_report_file->close();
            _omp_report_file = NULL;
        }
    }

    void Base::phase_cleanup(DTO& data_flow)
    {
        _core.phase_cleanup(data_flow);
    }

    void Base::phase_cleanup_end_of_pipeline(DTO& data_flow)
    {
        _core.phase_cleanup_end_of_pipeline(data_flow);
    }

#define INVALID_STATEMENT_HANDLER(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomStatement ctr) { \
            error_printf_at(ctr.get_locus(), "invalid '#pragma %s %s'\n",  \
                    ctr.get_text().c_str(), \
                    ctr.get_pragma_line().get_text().c_str()); \
        } \
        void Base::_name##_handler_post(TL::PragmaCustomStatement) { }

#define INVALID_DECLARATION_HANDLER(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomDeclaration ctr) { \
            error_printf_at(ctr.get_locus(), "invalid '#pragma %s %s'\n",  \
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
        INVALID_DECLARATION_HANDLER(workshare)
        INVALID_DECLARATION_HANDLER(critical)
        INVALID_DECLARATION_HANDLER(atomic)
        INVALID_DECLARATION_HANDLER(master)
        INVALID_DECLARATION_HANDLER(teams)
        INVALID_DECLARATION_HANDLER(distribute)
        INVALID_DECLARATION_HANDLER(distribute_parallel_for)
        INVALID_DECLARATION_HANDLER(distribute_parallel_do)
        INVALID_DECLARATION_HANDLER(target_teams)
        INVALID_DECLARATION_HANDLER(target_data)
        INVALID_DECLARATION_HANDLER(teams_distribute)
        INVALID_DECLARATION_HANDLER(target_teams_distribute)
        INVALID_DECLARATION_HANDLER(teams_distribute_parallel_for)
        INVALID_DECLARATION_HANDLER(teams_distribute_parallel_do)
        INVALID_DECLARATION_HANDLER(target_teams_distribute_parallel_for)
        INVALID_DECLARATION_HANDLER(target_teams_distribute_parallel_do)
        INVALID_DECLARATION_HANDLER(taskloop)

        INVALID_STATEMENT_HANDLER(declare_simd)

#define EMPTY_HANDLERS_CONSTRUCT(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomStatement) { } \
        void Base::_name##_handler_post(TL::PragmaCustomStatement) { } \
        void Base::_name##_handler_pre(TL::PragmaCustomDeclaration) { } \
        void Base::_name##_handler_post(TL::PragmaCustomDeclaration) { }

#define EMPTY_HANDLERS_STATEMENT(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomStatement) { } \
        void Base::_name##_handler_post(TL::PragmaCustomStatement) { }

#define EMPTY_HANDLERS_DIRECTIVE(_name) \
        void Base::_name##_handler_pre(TL::PragmaCustomDirective) { } \
        void Base::_name##_handler_post(TL::PragmaCustomDirective) { }

        EMPTY_HANDLERS_CONSTRUCT(ordered)

        EMPTY_HANDLERS_DIRECTIVE(section)

        EMPTY_HANDLERS_STATEMENT(distribute_parallel_for)
        EMPTY_HANDLERS_STATEMENT(distribute_parallel_do)
        EMPTY_HANDLERS_STATEMENT(target_teams)
        EMPTY_HANDLERS_STATEMENT(teams_distribute)
        EMPTY_HANDLERS_STATEMENT(target_teams_distribute)
        EMPTY_HANDLERS_STATEMENT(teams_distribute_parallel_for)
        EMPTY_HANDLERS_STATEMENT(teams_distribute_parallel_do)
        EMPTY_HANDLERS_STATEMENT(target_teams_distribute_parallel_for)
        EMPTY_HANDLERS_STATEMENT(target_teams_distribute_parallel_do)

    void Base::set_simd(const std::string &simd_enabled_str)
    {
        parse_boolean_option("simd_enabled",
                simd_enabled_str,
                _simd_enabled,
                "Assuming false");
    }

    void Base::set_ompss_mode(const std::string& str)
    {
        parse_boolean_option("ompss_mode", str, _ompss_mode, "Assuming false.");
        _core.set_ompss_mode(_ompss_mode);
    }

    void Base::set_omp_report_parameter(const std::string& str)
    {
        parse_boolean_option("omp_report", str, _omp_report, "Assuming false.");
    }

    bool Base::in_ompss_mode() const
    {
        return _ompss_mode;
    }

    bool Base::emit_omp_report() const
    {
        return _omp_report;
    }

    void Base::set_copy_deps_by_default(const std::string& str)
    {
        parse_boolean_option("copy_deps", str, _copy_deps_by_default, "Assuming true.");
        _core.set_copy_deps_by_default(_copy_deps_by_default);
    }

    bool Base::copy_deps_by_default() const
    {
        return _copy_deps_by_default;
    }

    void Base::set_untied_tasks_by_default(const std::string& str)
    {
         parse_boolean_option("untied_tasks", str, _untied_tasks_by_default, "Assuming true.");
        _core.set_untied_tasks_by_default(_untied_tasks_by_default);
    }

    void Base::set_enable_input_by_value_dependences(const std::string& str)
    {
        bool b;
         parse_boolean_option("enable_input_by_value_dependences", str, b, "Assuming false.");
        _core.set_enable_input_by_value_dependences(b);
    }

    void Base::set_enable_nonvoid_function_tasks(const std::string& str)
    {
        bool b;
         parse_boolean_option("enable_nonvoid_function_tasks", str, b, "Assuming false.");
        _core.set_enable_nonvoid_function_tasks(b);
    }

    bool Base::untied_tasks_by_default() const
    {
        return _untied_tasks_by_default;
    }

    void Base::set_allow_shared_without_copies(const std::string &allow_shared_without_copies_str)
    {
        bool b = false;
        parse_boolean_option("allow_shared_without_copies",
                allow_shared_without_copies_str, b, "Assuming false");
        _core.set_allow_shared_without_copies(b);
    }

    void Base::set_allow_array_reductions(const std::string &allow_array_reductions)
    {
        bool b = true;
        parse_boolean_option("allow_array_reductions",
                allow_array_reductions, b, "Assuming true");
        _core.set_allow_array_reductions(b);
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

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "ATOMIC construct\n"
                << directive.get_locus_str() << ": " << "----------------\n"
                << OpenMP::Report::indent << directive.get_statements().prettyprint() << "\n"
                ;
        }

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

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "CRITICAL construct\n"
                << directive.get_locus_str() << ": " << "------------------\n"
                ;
        }

        if (param.is_defined())
        {
            ObjectList<std::string> critical_name = param.get_tokenized_arguments();

            execution_environment = Nodecl::List::make(
                    Nodecl::OpenMP::CriticalName::make(critical_name[0],
                        directive.get_locus()),
                    entry_flush, exit_flush);

            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "Named critical construct: '" << critical_name[0] << "'\n";
            }
        }
        else
        {
            execution_environment = Nodecl::List::make(entry_flush, exit_flush);

            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "Unnamed critical construct\n";
            }
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

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "BARRIER construct\n"
                << directive.get_locus_str() << ": " << "-----------------\n"
                << OpenMP::Report::indent << "(There is no more information for BARRIER)\n"
                ;
        }

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

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "FLUSH construct\n"
                << directive.get_locus_str() << ": " << "---------------\n"
                << OpenMP::Report::indent << "(There is no more information for FLUSH)\n"
                ;
        }

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

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "MASTER construct\n"
                << directive.get_locus_str() << ": " << "----------------\n"
                << OpenMP::Report::indent << "(There is no more information for MASTER)\n"
                ;
        }

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

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "TASKWAIT construct\n"
                << directive.get_locus_str() << ": " << "------------------\n"
                ;

            if (pragma_line.get_clause("on").is_defined())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This taskwait contains an 'on' clause\n"
                    ;
            }
        }

        OpenMP::DataEnvironment &data_environment =
            _core.get_openmp_info()->get_data_environment(directive);
        Nodecl::List environment = this->make_execution_environment(
                data_environment,
                pragma_line,
                /* ignore_target_info */ true,
                /* is_inline_task */ false);

        PragmaCustomClause noflush_clause = pragma_line.get_clause("noflush");
        if (noflush_clause.is_defined())
        {
            environment.append(
                    Nodecl::OpenMP::NoFlush::make(directive.get_locus()));
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This taskwait does not flush device overlaps due to 'noflush' clause\n"
                    ;
            }
        }
        else
        {
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This taskwait flushes device overlaps (if any device is used)\n"
                    ;
            }
        }

        pragma_line.diagnostic_unused_clauses();

        TL::ObjectList<OpenMP::DependencyItem> dependences;
        data_environment.get_all_dependences(dependences);
        if (!dependences.empty())
        {
            if (!this->in_ompss_mode())
            {
                error_printf_at(directive.get_locus(),
                        "a 'taskwait' construct with a 'on' clause is valid only in OmpSs\n");
            }

            directive.replace(
                    Nodecl::OmpSs::WaitOnDependences::make(
                        environment,
                        directive.get_locus())
                    );
        }
        else
        {
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This taskwait waits for all tasks created in the current context\n"
                    ;
            }
            directive.replace(
                    Nodecl::OpenMP::TaskwaitShallow::make(
                        environment,
                        directive.get_locus())
                    );
        }
    }


    void Base::taskyield_handler_pre(TL::PragmaCustomDirective) { }
    void Base::taskyield_handler_post(TL::PragmaCustomDirective directive)
    {
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "TASKYIELD construct\n"
                << directive.get_locus_str() << ": " << "------------------\n"
                ;
        }

        directive.replace(
                Nodecl::OpenMP::Taskyield::make(
                    directive.get_locus())
                );
    }

    // Inline tasks
    void Base::task_handler_pre(TL::PragmaCustomStatement) { }
    void Base::task_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataEnvironment &ds = _core.get_openmp_info()->get_data_environment(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "TASK construct\n"
                << directive.get_locus_str() << ": " << "--------------\n"
                ;
        }

        Nodecl::List execution_environment = this->make_execution_environment(ds,
                pragma_line, /* ignore_target_info */ false, /* is_inline_task */ true);

        PragmaCustomClause tied = pragma_line.get_clause("tied");
        PragmaCustomClause untied = pragma_line.get_clause("untied");
        if (untied.is_defined()
                // The tasks are untied by default and the current task has not defined the 'tied' clause
                || (_untied_tasks_by_default && !tied.is_defined()))
        {
            execution_environment.append(
                    Nodecl::OpenMP::Untied::make(
                        directive.get_locus()));

            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This is an untied task. The thread that executes the task may change "
                    "during the execution of the task (i.e. because of preemptions)\n"
                    ;
            }
        }
        else
        {
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This is a tied task. The thread that executes the task will not change "
                    "during the execution of the task\n"
                    ;
            }
        }


        PragmaCustomClause cost = pragma_line.get_clause("cost");
        {
            Nodecl::NodeclBase cost_expression;
            if (cost.is_defined())
            {
                TL::ObjectList<Nodecl::NodeclBase> expr_list = cost.get_arguments_as_expressions(directive);

                if (expr_list.empty())
                {
                    warn_printf_at(directive.get_locus(),
                                   "empty 'cost' clause. Ignoring\n");
                }
                else
                {
                    cost_expression = expr_list[0];
                    if (expr_list.size() > 1)
                    {
                        warn_printf_at(
                            expr_list[1].get_locus(),
                            "too many expressions for 'cost' clause\n");
                    }
                }
            }

            if (!cost_expression.is_null())
            {
                execution_environment.append(
                        Nodecl::OmpSs::Cost::make(
                            cost_expression,
                            directive.get_locus()));

                if (emit_omp_report())
                {
                    *_omp_report_file
                        << OpenMP::Report::indent
                        << "This task has cost '" << cost_expression.prettyprint() << "'\n";
                    ;
                }
            }
            else
            {
                if (emit_omp_report())
                {
                    *_omp_report_file
                        << OpenMP::Report::indent
                        << "No cost was defined for this task\n";
                    ;
                }
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

        handle_label_clause(directive, execution_environment);

        handle_task_if_clause(directive, /* parsing_context */ directive, execution_environment);
        handle_task_final_clause(directive, /* parsing_context */ directive, execution_environment);
        handle_task_priority_clause(directive, /* parsing_context */ directive, execution_environment);

        pragma_line.diagnostic_unused_clauses();

        Nodecl::NodeclBase body_of_task =
            directive.get_statements().shallow_copy();

        Nodecl::NodeclBase async_code =
            Nodecl::OpenMP::Task::make(execution_environment,
                    body_of_task,
                    directive.get_locus());

        directive.replace(async_code);
    }

    void Base::parallel_handler_pre(TL::PragmaCustomStatement)
    {
        if (this->in_ompss_mode())
        {
            return;
        }
    }
    void Base::parallel_handler_post(TL::PragmaCustomStatement directive)
    {
        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "PARALLEL construct\n"
                << directive.get_locus_str() << ": " << "------------------\n"
                ;
        }
        if (this->in_ompss_mode())
        {
            warn_printf_at(directive.get_locus(),
                    "explicit parallel regions do not have any effect in OmpSs\n");
            // Ignore parallel
            directive.replace(directive.get_statements());

            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This construct is ignored in OmpSs mode\n"
                    ;
            }
            return;
        }

        OpenMP::DataEnvironment &ds = _core.get_openmp_info()->get_data_environment(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment(ds,
                pragma_line, /* ignore_target_info */ false, /* is_inline_task */ false);

        handle_label_clause(directive, execution_environment);

        Nodecl::NodeclBase num_threads;
        PragmaCustomClause clause = pragma_line.get_clause("num_threads");
        {
            ObjectList<Nodecl::NodeclBase> args = clause.get_arguments_as_expressions();
            if (clause.is_defined()
                    && args.size() == 1)
            {
                num_threads = args[0];
                if (emit_omp_report())
                {
                    *_omp_report_file
                        << OpenMP::Report::indent
                        << "Number of threads requested '" << num_threads.prettyprint() << "'\n";
                }
            }
            else
            {
                if (clause.is_defined())
                {
                    error_printf_at(directive.get_locus(), "ignoring invalid 'num_threads' clause\n");
                }
            }
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

        // Set implicit barrier at the exit of the combined worksharing
        execution_environment.append(
            Nodecl::OpenMP::BarrierAtEnd::make(
                directive.get_locus()));

        PragmaCustomClause if_clause = pragma_line.get_clause("if");
        {
            ObjectList<Nodecl::NodeclBase> expr_list = if_clause.get_arguments_as_expressions(directive);
            if (if_clause.is_defined()
                    && expr_list.size() == 1)
            {
                execution_environment.append(Nodecl::OpenMP::If::make(expr_list[0].shallow_copy()));

                if (emit_omp_report())
                {
                    *_omp_report_file
                        << OpenMP::Report::indent
                        << "A team of threads will be created only if '"
                        << expr_list[0].prettyprint()
                        << "' holds\n";
                }
            }
            else
            {
                if (if_clause.is_defined())
                {
                    error_printf_at(directive.get_locus(), "ignoring invalid 'if' clause\n");
                }
                if (emit_omp_report())
                {
                    // *_omp_report_file
                    //     << OpenMP::Report::indent
                    //     << "A team of threads will always be created\n"
                    //     ;
                }
            }
        }

        // if (emit_omp_report())
        // {
        //     *_omp_report_file
        //         << OpenMP::Report::indent
        //         << "Recall that a PARALLEL construct always implies a BARRIER at the end\n"
        //         ;
        // }

        Nodecl::NodeclBase parallel_code = Nodecl::OpenMP::Parallel::make(
                    execution_environment,
                    num_threads,
                    directive.get_statements().shallow_copy(),
                    directive.get_locus());

        pragma_line.diagnostic_unused_clauses();
        directive.replace(parallel_code);
    }

    void Base::single_handler_pre(TL::PragmaCustomStatement) { }
    void Base::single_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataEnvironment &ds = _core.get_openmp_info()->get_data_environment(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "SINGLE construct\n"
                << directive.get_locus_str() << ": " << "----------------\n"
                ;
        }

        Nodecl::List execution_environment = this->make_execution_environment(
                ds, pragma_line, /* ignore_target_info */ true, /* is_inline_task */ false);

        if (!pragma_line.get_clause("nowait").is_defined())
        {
            execution_environment.append(
                    Nodecl::OpenMP::FlushAtExit::make(
                        directive.get_locus())
            );

            execution_environment.append(
                    Nodecl::OpenMP::BarrierAtEnd::make(
                        directive.get_locus()));

            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This SINGLE construct does NOT have a BARRIER at the"
                    " end because of the 'nowait' clause\n";
                    ;
            }
        }
        else
        {
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This SINGLE construct implies a BARRIER at the end\n";
                    ;
            }
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

    void Base::workshare_handler_pre(TL::PragmaCustomStatement) { }
    void Base::workshare_handler_post(TL::PragmaCustomStatement directive)
    {
        OpenMP::DataEnvironment &ds = _core.get_openmp_info()->get_data_environment(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "WORKSHARE construct\n"
                << directive.get_locus_str() << ": " << "-------------------\n"
                ;
        }

        Nodecl::List execution_environment = this->make_execution_environment(
                ds, pragma_line, /* ignore_target_info */ true, /* is_inline_task */ false);

        if (!pragma_line.get_clause("nowait").is_defined())
        {
            execution_environment.append(
                    Nodecl::OpenMP::FlushAtExit::make(
                        directive.get_locus())
            );

            execution_environment.append(
                    Nodecl::OpenMP::BarrierAtEnd::make(
                        directive.get_locus()));

            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This WORKSHARE construct does not have a "
                    "BARRIER at the end due to the 'nowait' clause\n"
                    ;
            }
        }
        else
        {
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This WORKSHARE construct implies a BARRIER at the end\n"
                    ;
            }
        }

        Nodecl::List code;
        code.append(
                Nodecl::OpenMP::Workshare::make(
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

        PragmaCustomLine pragma_line = directive.get_pragma_line();
        bool barrier_at_end = !pragma_line.get_clause("nowait").is_defined();

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "FOR construct\n"
                << directive.get_locus_str() << ": " << "-------------\n"
            ;
        }

        Nodecl::NodeclBase code = loop_handler_post(directive, statement, barrier_at_end, /* is_combined_with_parallel */ false);
        pragma_line.diagnostic_unused_clauses();
        directive.replace(code);
    }

    Nodecl::NodeclBase Base::sections_handler_common(
            TL::PragmaCustomStatement directive,
            Nodecl::NodeclBase statements,
            bool barrier_at_end,
            bool is_combined_with_parallel)
    {
        OpenMP::DataEnvironment &ds = _core.get_openmp_info()->get_data_environment(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment(ds,
                pragma_line, /* ignore_target_info */ false, /* is_inline_task */ false );

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

            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This SECTIONS construct implies a BARRIER at the end\n"
                    ;
            }
        }
        else
        {
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This SECTIONS construct does not have a barrier at end\n"
                    ;
            }
        }

        if (is_combined_with_parallel)
        {
            execution_environment.append(
                    Nodecl::OpenMP::CombinedWithParallel::make(
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
            Nodecl::NodeclBase context,
            bool barrier_at_end,
            bool is_combined_with_parallel)
    {
        OpenMP::DataEnvironment &ds = _core.get_openmp_info()->get_data_environment(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment(
                ds, pragma_line, /* ignore_target_info */ false, /* is_inline_task */ false);

        handle_label_clause(directive, execution_environment);

        if (this->in_ompss_mode())
            handle_task_final_clause(directive, /* parsing_context */ directive, execution_environment);

        if (pragma_line.get_clause("schedule").is_defined())
        {
            PragmaCustomClause clause = pragma_line.get_clause("schedule");

            ObjectList<std::string> arguments = clause.get_tokenized_arguments();

            Nodecl::NodeclBase chunk;

            std::string schedule = arguments[0];
            schedule = strtolower(schedule.c_str());

            std::string checked_schedule_name = schedule;

            // Allow OpenMP schedules be prefixed with 'ompss_', 'omp_' and 'openmp_'

            std::string valid_prefixes[] = { "ompss_", "omp_", "openmp_", ""};
            int i = 0;
            bool found = false;
            while (valid_prefixes[i] != "" && !found)
            {
                found = checked_schedule_name.substr(0,valid_prefixes[i].size()) == valid_prefixes[i];
                if (found)
                    checked_schedule_name = checked_schedule_name.substr(valid_prefixes[i].size());

                ++i;
            }

            bool default_chunk = false;
            if (arguments.size() == 1)
            {
                if (checked_schedule_name == "static")
                {
                    default_chunk = true;
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

            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "Loop has been explictly scheduled as '"
                    << schedule << "'";

               if (!default_chunk)
               {
                   *_omp_report_file << " with a chunk of '" << chunk.prettyprint() << "'"
                       ;
               }

               *_omp_report_file << "\n";
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

            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "Loop has been implicitly scheduled as 'STATIC'\n"
                    ;
            }
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

            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This loop implies a BARRIER at end\n"
                    ;
            }
        }
        else
        {
            if (emit_omp_report())
            {
                if (!is_combined_with_parallel)
                {
                    *_omp_report_file
                        << OpenMP::Report::indent
                        << "This loop does not have any BARRIER at end\n"
                        ;
                }
                else
                {
                    *_omp_report_file
                        << OpenMP::Report::indent
                        << "This loop implies a BARRIER at end of the enclosing PARALLEL\n"
                        ;
                }
            }
        }

        if (is_combined_with_parallel)
        {
            execution_environment.append(
                    Nodecl::OpenMP::CombinedWithParallel::make(
                        directive.get_locus()));
        }

        // ERROR_CONDITION (!context.is<Nodecl::ForStatement>(), "Invalid tree of kind '%s'", ast_print_node_type(statement.get_kind()));
        // TL::ForStatement for_statement(context.as<Nodecl::ForStatement>());

        Nodecl::OpenMP::For distribute =
            Nodecl::OpenMP::For::make(
                    execution_environment,
                    context,
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
        ERROR_CONDITION(!statement.is<Nodecl::Context>(), "Invalid tree", 0);

        PragmaCustomLine pragma_line = directive.get_pragma_line();
        bool barrier_at_end = !pragma_line.get_clause("nowait").is_defined();

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "DO construct\n"
                << directive.get_locus_str() << ": " << "------------\n"
            ;
        }
        Nodecl::NodeclBase code = loop_handler_post(directive, statement, barrier_at_end, /* is_combined_with_parallel */ false);
        pragma_line.diagnostic_unused_clauses();
        directive.replace(code);
    }

    void Base::taskloop_handler_pre(TL::PragmaCustomStatement directive) { }
    void Base::taskloop_handler_post(TL::PragmaCustomStatement directive)
    {
        Nodecl::NodeclBase statement = directive.get_statements();
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();
        ERROR_CONDITION(!statement.is<Nodecl::Context>(), "Invalid tree", 0);

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "TASKLOOP construct\n"
                << directive.get_locus_str() << ": " << "------------------\n"
                ;
        }

        OpenMP::DataEnvironment &ds = _core.get_openmp_info()->get_data_environment(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        warn_printf_at(pragma_line.get_locus(), "'taskloop' construct is EXPERIMENTAL\n");

        PragmaCustomClause grainsize_clause = pragma_line.get_clause("grainsize");
        PragmaCustomClause num_tasks_clause = pragma_line.get_clause("num_tasks");
        PragmaCustomClause nogroup = pragma_line.get_clause("nogroup");

        Nodecl::NodeclBase grainsize_expr, num_tasks_expr;
        if (grainsize_clause.is_defined() == num_tasks_clause.is_defined())
        {
            if (grainsize_clause.is_defined())
            {
                error_printf_at(pragma_line.get_locus(), "cannot define 'grainsize' and 'num_tasks' clauses at the same time\n");
            }
            else
            {
                error_printf_at(pragma_line.get_locus(), "missing a 'grainsize' or a 'num_tasks' clauses\n");
            }
        }
        else
        {
            if (grainsize_clause.is_defined())
            {
                TL::ObjectList<Nodecl::NodeclBase> args = grainsize_clause.get_arguments_as_expressions();
                int num_args = args.size();
                if (num_args >= 1)
                {
                    grainsize_expr = args[0];
                    if (num_args != 1)
                    {
                        error_printf_at(pragma_line.get_locus(), "too many expressions in 'grainsize' clause\n");
                    }
                }
                else
                {
                    error_printf_at(pragma_line.get_locus(), "missing expression in 'grainsize' clause\n");
                }
            }
            else // num_tasks.is_defined()
            {
                TL::ObjectList<Nodecl::NodeclBase> args = num_tasks_clause.get_arguments_as_expressions();
                int num_args = args.size();
                if (num_args >= 1)
                {
                    num_tasks_expr = args[0];
                    if (num_args != 1)
                    {
                        error_printf_at(pragma_line.get_locus(), "too many expressions in 'num_tasks' clause\n");
                    }
                }
                else
                {
                    error_printf_at(pragma_line.get_locus(), "missing expression in 'grainsize' clause\n");
                }
            }
        }

        // grainsize_expr or num_tasks_expr has to be valid, otherwise we skip the taskloop
        if ((grainsize_expr.is_null() || grainsize_expr.is<Nodecl::ErrExpr>()) &&
            (num_tasks_expr.is_null() || num_tasks_expr.is<Nodecl::ErrExpr>()))
            return;

        bool taskwait_at_the_end = true;
        if (nogroup.is_defined())
            taskwait_at_the_end = false;

        // Since we are going to transform the taskloop construct to a task
        // construct defined inside the loop, we can set the 'is_inline_task'
        // to true. This flag is only used to generate task reductions instead
        // of worksharing reductions
        Nodecl::List execution_environment = this->make_execution_environment(
                ds, pragma_line, /* ignore_target_info */ false, /* is_inline_task */ true);

        handle_label_clause(directive, execution_environment);

        handle_task_if_clause(directive, /* parsing_context */ statement, execution_environment);
        handle_task_final_clause(directive, /* parsing_context */ statement, execution_environment);
        handle_task_priority_clause(directive, /* parsing_context */ statement, execution_environment);

        pragma_line.diagnostic_unused_clauses();

        taskloop_block_loop(directive, statement, execution_environment, grainsize_expr, num_tasks_expr);

        Nodecl::List list;
        list.append(statement);
        if (taskwait_at_the_end)
        {
            list.append(
                    Nodecl::OpenMP::TaskwaitShallow::make(
                        /*environment*/ nodecl_null(),
                        directive.get_locus()));
        }

        directive.replace(list);
    }

    // Since parallel {for,do,sections} are split into two nodes: parallel and
    // then {for,do,section}, we need to make sure the children of the new
    // parallel contains a proper context as its child
    void Base::nest_context_in_pragma(TL::PragmaCustomStatement directive)
    {
        Nodecl::NodeclBase stms = directive.get_statements();

        const decl_context_t* new_context =
            new_block_context(directive.retrieve_context().get_decl_context());
        Nodecl::NodeclBase ctx = Nodecl::List::make(
                Nodecl::Context::make(
                    stms,
                    new_context,
                    stms.get_locus()));

        directive.set_statements(ctx);
    }

    void Base::parallel_do_handler_pre(TL::PragmaCustomStatement directive)
    {
        if (this->in_ompss_mode())
        {
            do_handler_pre(directive);
            return;
        }

        nest_context_in_pragma(directive);
    }

    void Base::parallel_do_handler_post(TL::PragmaCustomStatement directive)
    {
        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "PARALLEL DO construct\n"
                << directive.get_locus_str() << ": " << "---------------------\n"
            ;
        }

        if (this->in_ompss_mode())
        {
            // In OmpSs this is like a simple DO
            warn_printf_at(directive.get_locus(),
                    "explicit parallel regions do not have any effect in OmpSs\n");
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "Note that in OmpSs the PARALLEL part of a PARALLEL DO is ignored\n"
                    ;
            }
            do_handler_post(directive);
            return;
        }

        OpenMP::DataEnvironment &ds = _core.get_openmp_info()->get_data_environment(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();
        Nodecl::List execution_environment = this->make_execution_environment_for_combined_worksharings(ds, pragma_line);

        Nodecl::NodeclBase statement = directive.get_statements();
        // This first context was added by nest_context_in_pragma
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();
        ERROR_CONDITION(!statement.is<Nodecl::Context>(), "Invalid tree", 0);
        // This is the usual context of the statements of a pragma
        statement = statement.as<Nodecl::Context>().get_in_context();
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();
        ERROR_CONDITION(!statement.is<Nodecl::Context>(), "Invalid tree", 0);

        Nodecl::NodeclBase num_threads;
        PragmaCustomClause clause = pragma_line.get_clause("num_threads");
        {
            ObjectList<Nodecl::NodeclBase> args = clause.get_arguments_as_expressions();
            if (clause.is_defined()
                    && args.size() == 1)
            {
                num_threads = args[0];
            }
            else if (clause.is_defined())
            {
                error_printf_at(directive.get_locus(), "ignoring invalid 'num_threads' wrong clause\n");
            }
        }

        if (!num_threads.is_null())
        {
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "Number of threads requested '" << num_threads.prettyprint() << "'\n";
            }
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

        // Set implicit barrier at the exit of the combined worksharing
        execution_environment.append(
                Nodecl::OpenMP::BarrierAtEnd::make(
                    directive.get_locus()));

        PragmaCustomClause if_clause = pragma_line.get_clause("if");
        if (if_clause.is_defined())
        {
            ObjectList<Nodecl::NodeclBase> expr_list = if_clause.get_arguments_as_expressions(directive);
            if (expr_list.size() != 1)
            {
                error_printf_at(directive.get_locus(),
                        "clause 'if' requires just one argument\n");
            }
            execution_environment.append(Nodecl::OpenMP::If::make(expr_list[0].shallow_copy()));
        }

        // for-statement
        Nodecl::NodeclBase for_statement_code = loop_handler_post(directive,
                statement,
                /* barrier_at_end */ false,
                /* is_combined_with_parallel */ true);

        statement = directive.get_statements();
        // This first context was added by nest_context_in_pragma
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();
        ERROR_CONDITION(!statement.is<Nodecl::Context>(), "Invalid tree", 0);
        // Nest the for in the place where we expect the for-statement code
        statement.as<Nodecl::Context>().set_in_context(for_statement_code);

        Nodecl::NodeclBase parallel_code
            = Nodecl::OpenMP::Parallel::make(
                execution_environment,
                num_threads,
                directive.get_statements().shallow_copy(),
                directive.get_locus());

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

    void Base::target_handler_pre(TL::PragmaCustomStatement stmt)
    {
        if (!this->in_ompss_mode())
        {
            omp_target_handler_pre(stmt);
        }
        else
        {
            ompss_target_handler_pre(stmt);
        }
    }
    void Base::target_handler_pre(TL::PragmaCustomDeclaration decl)
    {
        if (!this->in_ompss_mode())
        {
            omp_target_handler_pre(decl);
        }
        else
        {
            ompss_target_handler_pre(decl);
        }
    }

    void Base::target_handler_post(TL::PragmaCustomStatement stmt)
    {
        if (!this->in_ompss_mode())
        {
            omp_target_handler_post(stmt);
        }
        else
        {
            ompss_target_handler_post(stmt);
        }
    }

    void Base::target_handler_post(TL::PragmaCustomDeclaration decl)
    {
        if (!this->in_ompss_mode())
        {
            omp_target_handler_post(decl);
        }
        else
        {
            ompss_target_handler_post(decl);
        }
    }

    // clause(list[:int])
    template <typename openmp_node>
    void Base::process_symbol_list_colon_int_clause(
            const TL::PragmaCustomLine& pragma_line,
            const std::string& pragma_name,
            const Nodecl::NodeclBase& ref_scope,
            Nodecl::List& environment,
            const int default_int)
    {
        PragmaCustomClause clause_clause = pragma_line.get_clause(pragma_name);

        if (clause_clause.is_defined())
        {
            TL::ObjectList<std::string> arg_clauses_list = clause_clause.get_raw_arguments();

            TL::ExpressionTokenizerTrim colon_tokenizer(':');
            TL::ExpressionTokenizerTrim comma_tokenizer(',');

            for(TL::ObjectList<std::string>::iterator it = arg_clauses_list.begin();
                    it != arg_clauses_list.end();
                    it++)
            {
                TL::ObjectList<std::string> colon_splited_list = colon_tokenizer.tokenize(*it);

                int colon_splited_list_size = colon_splited_list.size();

                ERROR_CONDITION((colon_splited_list_size <= 0) ||
                        (colon_splited_list_size > 2),
                        "'%s' clause has a wrong format", 
                        pragma_name.c_str());

                // Int value will be default_int
                Nodecl::IntegerLiteral int_value = 
                    const_value_to_nodecl(const_value_get_signed_int(default_int));

                if (colon_splited_list_size == 2)
                {
                    TL::Source colon_src;
                    colon_src << colon_splited_list.back();

                    Nodecl::NodeclBase nodecl_int_value = colon_src.parse_expression(
                            ref_scope.retrieve_context());

                    ERROR_CONDITION(!nodecl_int_value.is<Nodecl::IntegerLiteral>(),
                            "wrong int_value in '%s' clause", pragma_name.c_str());

                    int_value = nodecl_int_value.as<Nodecl::IntegerLiteral>();
                }

                TL::ObjectList<std::string> comma_splited_list = comma_tokenizer.tokenize(
                        colon_splited_list.front());

                Nodecl::List clause_variables = 
                    Nodecl::List::make(Nodecl::Utils::get_strings_as_expressions(
                                comma_splited_list, ref_scope));

                environment.append(openmp_node::make(
                            clause_variables, int_value,
                            pragma_line.get_locus()));
            }
        }
    }

    // clause(list)
    template <typename openmp_node>
    void Base::process_symbol_list_clause(
            const TL::PragmaCustomLine& pragma_line,
            const std::string& pragma_name,
            const Nodecl::NodeclBase& ref_scope,
            Nodecl::List& environment)
    {
        PragmaCustomClause clause = pragma_line.get_clause(pragma_name);

        if (clause.is_defined())
        {
            environment.append(openmp_node::make(
                        Nodecl::List::make(
                            clause.get_arguments_as_expressions(ref_scope)),
                        pragma_line.get_locus()));
        }
    }

    void Base::process_common_simd_clauses(
            const TL::PragmaCustomLine& pragma_line,
            const Nodecl::NodeclBase& ref_scope,
            Nodecl::List& environment)
    {
        // Aligned
        process_symbol_list_colon_int_clause<Nodecl::OpenMP::Aligned>
            (pragma_line, "aligned", ref_scope, environment, 0);

        // Linear
        process_symbol_list_colon_int_clause<Nodecl::OpenMP::Linear>
            (pragma_line, "linear", ref_scope, environment, 1);

        // Uniform
        process_symbol_list_clause<Nodecl::OpenMP::Uniform>
            (pragma_line, "uniform", ref_scope, environment);

        // Suitable
        process_symbol_list_clause<Nodecl::OpenMP::Suitable>
            (pragma_line, "suitable", ref_scope, environment);

        // Unroll
        PragmaCustomClause unroll_clause = pragma_line.get_clause("unroll");

        if (unroll_clause.is_defined())
        {
            environment.append(
                    Nodecl::OpenMP::Unroll::make(
                        Nodecl::IntegerLiteral::make(TL::Type::get_int_type(),
                            unroll_clause.get_arguments_as_expressions().front().get_constant()),
                        pragma_line.get_locus()));
        }

        // Unroll and Jam
        PragmaCustomClause unroll_and_jam_clause = pragma_line.get_clause("unroll_and_jam");

        if (unroll_and_jam_clause.is_defined())
        {
            environment.append(
                    Nodecl::OpenMP::UnrollAndJam::make(
                        Nodecl::IntegerLiteral::make(TL::Type::get_int_type(),
                            unroll_and_jam_clause.get_arguments_as_expressions().front().get_constant()),
                        pragma_line.get_locus()));
        }

        // VectorLength
        PragmaCustomClause vectorlength_clause = pragma_line.get_clause("vectorlength");

        if (vectorlength_clause.is_defined())
        {
            environment.append(Nodecl::OpenMP::VectorLength::make(
                Nodecl::IntegerLiteral::make(
                    TL::Type::get_int_type(),
                    vectorlength_clause.get_arguments_as_expressions()
                        .front()
                        .get_constant(),
                    pragma_line.get_locus()),
                pragma_line.get_locus()));
        }

        // VectorLengthFor
        PragmaCustomClause vectorlengthfor_clause = pragma_line.get_clause("vectorlengthfor");

        if (vectorlengthfor_clause.is_defined())
        {
            TL::Source target_type_src;

            target_type_src << vectorlengthfor_clause.get_raw_arguments().front();

            TL::Type target_type = target_type_src.parse_c_type_id(ref_scope.retrieve_context());

            environment.append(
                    Nodecl::OpenMP::VectorLengthFor::make(
                        target_type,
                        pragma_line.get_locus()));
        }

        // Non-temporal (Stream stores)
        PragmaCustomClause nontemporal_clause = pragma_line.get_clause("nontemporal");

        if (nontemporal_clause.is_defined())
        {
            TL::ObjectList<std::string> arg_clauses_list = nontemporal_clause.get_raw_arguments();

            TL::ExpressionTokenizerTrim colon_tokenizer(':');
            TL::ExpressionTokenizerTrim comma_tokenizer(',');

            for(TL::ObjectList<std::string>::iterator it = arg_clauses_list.begin();
                    it != arg_clauses_list.end();
                    it++)
            {
                TL::ObjectList<std::string> colon_splited_list = colon_tokenizer.tokenize(*it);

                int colon_splited_list_size = colon_splited_list.size();

                ERROR_CONDITION((colon_splited_list_size <= 0) ||
                        (colon_splited_list_size > 2),
                        "'nontemporal' clause has a wrong format", 0);

                //Nodecl::IntegerLiteral alignment = const_value_to_nodecl(const_value_get_zero(4, 1));

                TL::ObjectList<std::string> comma_splited_list;
                TL::ObjectList<Nodecl::NodeclBase> nontemporal_flags_obj_list;

                if (colon_splited_list_size == 2)
                {
                    comma_splited_list = comma_tokenizer.tokenize(colon_splited_list.back());

                    for(ObjectList<std::string>::iterator comma_it = comma_splited_list.begin();
                            comma_it != comma_splited_list.end();
                            comma_it++)
                    {
                        if ((*comma_it) == "relaxed")
                        {
                            nontemporal_flags_obj_list.insert(Nodecl::RelaxedFlag::make());
                            printf("Relaxed!\n");
                        }
                        else if((*comma_it) == "evict")
                        {
                            nontemporal_flags_obj_list.insert(Nodecl::EvictFlag::make());
                            printf("Evict!\n");
                        }
                        else
                        {
                            printf("%s\n", comma_it->c_str());
                            fatal_error("Neither 'relaxed' nor 'evict'");
                        }
                    }
                }

                comma_splited_list = comma_tokenizer.tokenize(
                        colon_splited_list.front());

                Nodecl::List nontemporal_variables =
                    Nodecl::List::make(Nodecl::Utils::get_strings_as_expressions(
                                comma_splited_list, pragma_line));

                Nodecl::List nontemporal_flags =
                    Nodecl::List::make(nontemporal_flags_obj_list);

                environment.append(
                        Nodecl::OpenMP::Nontemporal::make(
                            nontemporal_variables,
                            nontemporal_flags,
                            pragma_line.get_locus()));
            }
        }

        // Overlap
        PragmaCustomClause overlap_clause = pragma_line.get_clause("overlap");

        if (overlap_clause.is_defined())
        {
            TL::ObjectList<std::string> arg_clauses_list = overlap_clause.get_raw_arguments();

            TL::ExpressionTokenizerTrim colon_tokenizer(':');
            TL::ExpressionTokenizerTrim comma_tokenizer(',');

            for(TL::ObjectList<std::string>::iterator it = arg_clauses_list.begin();
                    it != arg_clauses_list.end();
                    it++)
            {
                TL::ObjectList<std::string> colon_splited_list = colon_tokenizer.tokenize(*it);

                int colon_splited_list_size = colon_splited_list.size();

                ERROR_CONDITION((colon_splited_list_size <= 0) ||
                        (colon_splited_list_size > 2),
                        "'overlap' clause has a wrong format", 0);

                //Nodecl::IntegerLiteral alignment = const_value_to_nodecl(const_value_get_zero(4, 1));

                TL::ObjectList<std::string> comma_splited_list;
                TL::ObjectList<Nodecl::NodeclBase> overlap_flags_obj_list;

                Nodecl::NodeclBase min_group_loads;
                Nodecl::NodeclBase max_group_registers;
                Nodecl::NodeclBase max_groups;

                if (colon_splited_list_size == 2)
                {
                    comma_splited_list = comma_tokenizer.tokenize(colon_splited_list.back());

                    ERROR_CONDITION(comma_splited_list.size() > 3,
                        "'overlap' clause has a wrong format", 0);

                    TL::ObjectList<std::string>::iterator comma_splited_it =
                       comma_splited_list.begin();

                    // Min group loads
                    if (comma_splited_it != comma_splited_list.end())
                    {
                        TL::Source it_src;
                        it_src << *comma_splited_it;

                        min_group_loads = it_src.parse_expression(
                                ref_scope.retrieve_context());

                        ERROR_CONDITION(!min_group_loads.is<Nodecl::IntegerLiteral>(),
                                "'min_group_loads' in 'overlap' clause has a wrong type", 0);

                        comma_splited_it++;
                    }
                    else
                    {
                        fatal_error("Missing 'min_group_loads' parameter in 'overlap' clause");
                    } 

                    // Max group registers
                    if (comma_splited_it != comma_splited_list.end())
                    {
                        TL::Source it_src;
                        it_src << *comma_splited_it;

                        max_group_registers = it_src.parse_expression(
                                ref_scope.retrieve_context());

                        ERROR_CONDITION(!min_group_loads.is<Nodecl::IntegerLiteral>(),
                                "'max_group_registers' in 'overlap' clause has a wrong type", 0);

                        comma_splited_it++;
                    }
                    else
                    {
                        fatal_error("Missing 'max_group_registers' parameter in 'overlap' clause");
                    } 

                    // Max groups
                    if (comma_splited_it != comma_splited_list.end())
                    {
                        TL::Source it_src;
                        it_src << *comma_splited_it;

                        max_groups = it_src.parse_expression(
                                ref_scope.retrieve_context());

                        ERROR_CONDITION(!min_group_loads.is<Nodecl::IntegerLiteral>(),
                                "'max_groups' in 'overlap' clause has a wrong type", 0);

                        comma_splited_it++;
                    }
                    else
                    {
                        fatal_error("Missing 'max_groups' parameter in 'overlap' clause");
                    } 
                }

                comma_splited_list = comma_tokenizer.tokenize(
                        colon_splited_list.front());

                Nodecl::List overlap_variables =
                    Nodecl::List::make(Nodecl::Utils::get_strings_as_expressions(
                                comma_splited_list, pragma_line));

                environment.append(
                        Nodecl::OpenMP::Overlap::make(
                            overlap_variables,
                            min_group_loads,
                            max_group_registers,
                            max_groups,
                            pragma_line.get_locus()));
            }
        }

        // Prefetch
        PragmaCustomClause prefetch_clause = pragma_line.get_clause("prefetch");

        if (prefetch_clause.is_defined())
        {
            TL::ObjectList<std::string> arg_clauses_list = prefetch_clause.get_raw_arguments();

            TL::ExpressionTokenizerTrim colon_tokenizer(':');
            TL::ExpressionTokenizerTrim comma_tokenizer(',');

            for(TL::ObjectList<std::string>::iterator it = arg_clauses_list.begin();
                    it != arg_clauses_list.end();
                    it++)
            {
                TL::ObjectList<std::string> colon_splited_list = colon_tokenizer.tokenize(*it);

                int colon_splited_list_size = colon_splited_list.size();

                ERROR_CONDITION(colon_splited_list_size <= 0 || colon_splited_list_size > 2,
                        "'prefetch' clause has a wrong format", 0);

                // On top prefetch strategy by default
                Nodecl::NodeclBase prefetch_strategy_node = Nodecl::OnTopFlag::make();

                if (colon_splited_list_size == 2)
                {
                    std::string prefetch_strategy_str = colon_splited_list.back();

                    if (prefetch_strategy_str == "in_place")
                        prefetch_strategy_node = Nodecl::InPlaceFlag::make();
                    else
                    {
                        ERROR_CONDITION(prefetch_strategy_str != "on_top",
                                "wrong prefetch strategy '%s'", prefetch_strategy_str.c_str());
                    }
                }

                TL::ObjectList<std::string> comma_splited_list = comma_tokenizer.tokenize(
                        colon_splited_list.front());

                ERROR_CONDITION(comma_splited_list.size() != 2,
                        "Expected (l2_distance,l1_distance) paramenters in prefetch clause", 0);
                
                TL::ObjectList<Nodecl::NodeclBase> expr_list =
                    Nodecl::Utils::get_strings_as_expressions(comma_splited_list, pragma_line);

                environment.append(Nodecl::OpenMP::Prefetch::make(
                            Nodecl::List::make(expr_list),
                            prefetch_strategy_node,
                            pragma_line.get_locus()));
            }
        }
    }

    // SIMD Statement
    void Base::simd_handler_pre(TL::PragmaCustomStatement) { }
    void Base::simd_handler_post(TL::PragmaCustomStatement stmt)
    {
#ifndef VECTORIZATION_DISABLED
        if (_simd_enabled)
        {
            // SIMD Clauses
            PragmaCustomLine pragma_line = stmt.get_pragma_line();
            OpenMP::DataEnvironment &ds = _core.get_openmp_info()->get_data_environment(stmt);

            Nodecl::List environment = this->make_execution_environment(ds,
                    pragma_line, /* ignore_target_info */ false, /* is_inline_task */ false);

            process_common_simd_clauses(pragma_line, stmt, environment);

            Nodecl::NodeclBase loop_statement = get_statement_from_pragma(stmt);

            ERROR_CONDITION(!(loop_statement.is<Nodecl::ForStatement>() ||
                    loop_statement.is<Nodecl::WhileStatement>()),
                    "Unexpected node %s. Expecting a for-statement or while-statement"\
                    " after '#pragma omp simd'", 
                    ast_print_node_type(loop_statement.get_kind()));

            Nodecl::OpenMP::Simd omp_simd_node =
               Nodecl::OpenMP::Simd::make(
                       loop_statement.shallow_copy(),
                       environment,
                       loop_statement.get_locus());

            pragma_line.diagnostic_unused_clauses();
            stmt.replace(Nodecl::List::make(omp_simd_node));
        }
#else
    warn_printf_at(stmt.get_locus(), "ignoring '#pragma omp simd'\n");
#endif
    }

    void Base::simd_fortran_handler_pre(TL::PragmaCustomStatement stmt) { }
    void Base::simd_fortran_handler_post(TL::PragmaCustomStatement stmt) {
        warn_printf_at(stmt.get_locus(), "ignoring '!$OMP SIMD'\n");
    }

    void Base::simd_fortran_handler_pre(TL::PragmaCustomDeclaration stmt) { }
    void Base::simd_fortran_handler_post(TL::PragmaCustomDeclaration stmt) { }

    // SIMD Functions
#ifndef VECTORIZATION_DISABLED
    void Base::register_simd_function(
            OpenMP::DataEnvironment& ds,
            TL::Symbol sym,
            Nodecl::NodeclBase context_of_parameters,
            TL::PragmaCustomLine pragma_line,
            const locus_t* locus)
    {
        ERROR_CONDITION(!sym.is_valid(), "Expecting a symbol", 0);
        ERROR_CONDITION(!sym.is_function(), "Expecting a function", 0);

        if (sym.get_type().is_template_specialized_type()
                && sym == sym.get_type().get_related_template_type().get_primary_template().get_symbol())
        {
            // This is a primary template
            if (!CURRENT_CONFIGURATION->explicit_instantiation)
            {
                error_printf_at(locus, "cannot use '#pragma omp simd' on template functions when they are not instantiated\n");
            }
            else
            {
                TL::Type template_type = sym.get_type().get_related_template_type();
                TL::ObjectList<TL::Type> specializations = template_type.get_specializations();
                for (TL::ObjectList<TL::Type>::iterator it = specializations.begin();
                        it != specializations.end();
                        it++)
                {
                    // Skip the primary
                    if (it->get_symbol() == template_type.get_primary_template().get_symbol())
                        continue;

                    TL::Symbol current_specialization = it->get_symbol();

                    ERROR_CONDITION(current_specialization.get_function_code().is_null(),
                            "Expecting the code of this function", 0);
                    Nodecl::FunctionCode function_code_spec =
                        current_specialization.get_function_code().as<Nodecl::FunctionCode>();

                    info_printf_at(locus, "extending '#pragma omp declare simd' to function instantiation '%s'\n",
                        current_specialization.get_qualified_name().c_str());

                    Nodecl::NodeclBase context_of_parameters_spec = function_code_spec.get_statements();

                    register_simd_function(ds,
                            current_specialization,
                            context_of_parameters_spec,
                            pragma_line,
                            locus);
                }
            }
            // We are done
            return;
        }

        Nodecl::NodeclBase function_code = sym.get_function_code();
        ERROR_CONDITION(!function_code.is<Nodecl::FunctionCode>(), "Expecting a symbol with code", 0);

        Nodecl::List environment = this->make_execution_environment(ds,
                pragma_line, /* ignore_target_info */ false, /* is_inline_task */ false);

        process_common_simd_clauses(pragma_line, 
                context_of_parameters, environment);

        // Mask
        PragmaCustomClause mask_clause = pragma_line.get_clause("mask");
        PragmaCustomClause inbranch_clause = pragma_line.get_clause("inbranch");

        if (mask_clause.is_defined()
                || inbranch_clause.is_defined())
        {
            environment.append(
                    Nodecl::OpenMP::Mask::make(locus));
        }

        // No Mask
        PragmaCustomClause no_mask_clause = pragma_line.get_clause("nomask");
        PragmaCustomClause not_inbranch_clause = pragma_line.get_clause("notinbranch");

        if (no_mask_clause.is_defined()
                || not_inbranch_clause.is_defined())
        {
            environment.append(
                    Nodecl::OpenMP::NoMask::make(locus));
        }

        // Now we replace the whole function code with a SimdFunction
        // (vectorizer will later create a new FunctionCode for it)
        Nodecl::OpenMP::SimdFunction simd_func =
            Nodecl::OpenMP::SimdFunction::make(
                    function_code.shallow_copy(),
                    environment,
                    function_code.get_locus());

        function_code.replace(simd_func);
    }
#endif

    void Base::simd_handler_pre(TL::PragmaCustomDeclaration decl) { }
    void Base::simd_handler_post(TL::PragmaCustomDeclaration decl)
    {
#ifndef VECTORIZATION_DISABLED
        if (_simd_enabled)
        {
            // SIMD Clauses
            TL::PragmaCustomLine pragma_line = decl.get_pragma_line();
            OpenMP::DataEnvironment &ds = _core.get_openmp_info()->get_data_environment(decl);

            Base::register_simd_function(
                    ds,
                    decl.get_symbol(),
                    decl.get_context_of_parameters(),
                    pragma_line,
                    decl.get_locus());

            pragma_line.diagnostic_unused_clauses();
            // Remove #pragma
            Nodecl::Utils::remove_from_enclosing_list(decl);
        }
#else
    warn_printf_at(decl.get_locus(), "ignoring #pragma omp declare simd\n");
#endif
    }

    void Base::declare_simd_handler_pre(TL::PragmaCustomDeclaration decl)
    {
        simd_handler_pre(decl);
    }

    void Base::declare_simd_handler_post(TL::PragmaCustomDeclaration decl)
    {
        simd_handler_post(decl);
    }

    // SIMD For Statement
    void Base::simd_for_handler_pre(TL::PragmaCustomStatement) { }
    void Base::simd_for_handler_post(TL::PragmaCustomStatement stmt)
    {
#ifndef VECTORIZATION_DISABLED
        if (_simd_enabled)
        {
            // SIMD Clauses
            PragmaCustomLine pragma_line = stmt.get_pragma_line();
            Nodecl::List environment;

            process_common_simd_clauses(pragma_line, stmt, environment);

            // Skipping AST_LIST_NODE
            Nodecl::NodeclBase statements = stmt.get_statements();
            ERROR_CONDITION(!statements.is<Nodecl::List>(),
                    "'pragma omp simd' Expecting a AST_LIST_NODE (1)", 0);
            Nodecl::List ast_list_node = statements.as<Nodecl::List>();
            ERROR_CONDITION(ast_list_node.size() != 1,
                    "AST_LIST_NODE after '#pragma omp simd' must be equal to 1 (1)", 0);

            // Skipping NODECL_CONTEXT
            Nodecl::NodeclBase context = ast_list_node.front();
            //ERROR_CONDITION(!context.is<Nodecl::Context>(),
            //        "'pragma omp simd' Expecting a NODECL_CONTEXT", 0);

            // Skipping AST_LIST_NODE
            //Nodecl::NodeclBase in_context = context.as<Nodecl::Context>().get_in_context();
            // ERROR_CONDITION(!in_context.is<Nodecl::List>(),
            //         "'pragma omp simd' Expecting a AST_LIST_NODE (2)", 0);
            // Nodecl::List ast_list_node2 = in_context.as<Nodecl::List>();
            // ERROR_CONDITION(ast_list_node2.size() != 1,
            //         "AST_LIST_NODE after '#pragma omp simd' must be equal to 1 (2)", 0);

            // Nodecl::NodeclBase for_statement = ast_list_node2.front();
            // ERROR_CONDITION(!for_statement.is<Nodecl::ForStatement>(),
            //         "Unexpected node %s. Expecting a ForStatement after '#pragma omp simd'",
            //         ast_print_node_type(for_statement.get_kind()));

            // for_handler_post
            bool barrier_at_end = !pragma_line.get_clause("nowait").is_defined();

            Nodecl::OpenMP::For omp_for = loop_handler_post(
                    stmt, context, barrier_at_end, false).as<Nodecl::List>().front()
                .as<Nodecl::OpenMP::For>();

            Nodecl::OpenMP::SimdFor omp_simd_for_node =
               Nodecl::OpenMP::SimdFor::make(
                       omp_for,
                       environment,
                       context.get_locus());

            // Removing #pragma
            pragma_line.diagnostic_unused_clauses();
            //stmt.replace(code);
            stmt.replace(omp_simd_for_node);
        }
#else
    warn_printf_at(stmt.get_locus(), "ignoring #pragma omp simd for\n");
#endif
    }

    void Base::parallel_simd_for_handler_pre(TL::PragmaCustomStatement) { }
    void Base::parallel_simd_for_handler_post(TL::PragmaCustomStatement stmt)
    {
#ifndef VECTORIZATION_DISABLED
        TL::PragmaCustomLine pragma_line = stmt.get_pragma_line();
        pragma_line.diagnostic_unused_clauses();
        // FIXME - What is supposed to happen here?
        // It is still not supported
#else
    warn_printf_at(stmt.get_locus(), "ignoring #pragma omp parallel simd for\n");
#endif
    }

    void Base::sections_handler_pre(TL::PragmaCustomStatement) { }
    void Base::sections_handler_post(TL::PragmaCustomStatement directive)
    {
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "SECTIONS construct\n"
                << directive.get_locus_str() << ": " << "------------------\n"
                << directive.get_locus_str() << ": " << directive.get_statements().prettyprint() << "\n"
                ;
        }

        bool barrier_at_end = !pragma_line.get_clause("nowait").is_defined();

        Nodecl::NodeclBase sections_code = sections_handler_common(directive,
                directive.get_statements(),
                barrier_at_end,
                /* is_combined_with_parallel */ false);
        pragma_line.diagnostic_unused_clauses();
        directive.replace(sections_code);
    }

    void Base::parallel_sections_handler_pre(TL::PragmaCustomStatement directive)
    {
        if (this->in_ompss_mode())
        {
            sections_handler_pre(directive);
            return;
        }

        nest_context_in_pragma(directive);
    }

    void Base::parallel_sections_handler_post(TL::PragmaCustomStatement directive)
    {
        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "PARALLEL SECTIONS construct\n"
                << directive.get_locus_str() << ": " << "---------------------------\n"
                << directive.get_locus_str() << ": " << directive.get_statements().prettyprint() << "\n"
                ;
        }
        if (this->in_ompss_mode())
        {
            warn_printf_at(directive.get_locus(), "explicit parallel regions do not have any effect in OmpSs\n");
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "Note that the PARALLEL part of PARALLEL SECTIONS is ignored in OmpSs\n"
                    ;
            }
            sections_handler_post(directive);
            return;
        }

        OpenMP::DataEnvironment &ds = _core.get_openmp_info()->get_data_environment(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();

        Nodecl::List execution_environment = this->make_execution_environment_for_combined_worksharings(ds, pragma_line);

        Nodecl::NodeclBase statement = directive.get_statements();
        // This first context was added by nest_context_in_pragma
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();
        ERROR_CONDITION(!statement.is<Nodecl::Context>(), "Invalid tree", 0);
        // This is the usual context of the statements of a pragma
        statement = statement.as<Nodecl::Context>().get_in_context();

        Nodecl::NodeclBase num_threads;
        PragmaCustomClause clause = pragma_line.get_clause("num_threads");
        {
            ObjectList<Nodecl::NodeclBase> args = clause.get_arguments_as_expressions();
            if (clause.is_defined()
                    && args.size() == 1)
            {
                num_threads = args[0];
            }
            else if (clause.is_defined())
            {
                error_printf_at(directive.get_locus(), "ignoring invalid 'num_threads' wrong clause\n");
            }
        }

        if (!num_threads.is_null())
        {
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "Number of threads requested '" << num_threads.prettyprint() << "'\n";
            }
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

        // Set implicit barrier at the exit of the combined worksharing
        execution_environment.append(
                Nodecl::OpenMP::BarrierAtEnd::make(
                    directive.get_locus()));

        PragmaCustomClause if_clause = pragma_line.get_clause("if");
        {
            ObjectList<Nodecl::NodeclBase> expr_list = if_clause.get_arguments_as_expressions(directive);
            if (if_clause.is_defined()
                    && expr_list.size() == 1)
            {
                execution_environment.append(Nodecl::OpenMP::If::make(expr_list[0].shallow_copy()));

                if (emit_omp_report())
                {
                    *_omp_report_file
                        << OpenMP::Report::indent
                        << "A team of threads will be created only if '"
                        << expr_list[0].prettyprint()
                        << "' holds\n";
                }
            }
            else
            {
                if (if_clause.is_defined())
                {
                    error_printf_at(directive.get_locus(),
                            "clause 'if' requires just one argument\n");
                }
                // if (emit_omp_report())
                // {
                //     *_omp_report_file
                //         << directive.get_locus_str() << ": " << "A team of threads will always be created\n"
                //         ;
                // }
            }
        }

        Nodecl::NodeclBase sections_code = sections_handler_common(directive,
                statement,
                /* barrier_at_end */ false,
                /* is_combined_with_parallel */ true);

        statement = directive.get_statements();
        // This first context was added by nest_context_in_pragma
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();
        ERROR_CONDITION(!statement.is<Nodecl::Context>(), "Invalid tree", 0);
        // Nest the for in the place where we expect the for-statement code
        statement.as<Nodecl::Context>().set_in_context(sections_code);

        Nodecl::NodeclBase parallel_code
            = Nodecl::OpenMP::Parallel::make(
                execution_environment,
                num_threads,
                directive.get_statements().shallow_copy(),
                directive.get_locus());

        pragma_line.diagnostic_unused_clauses();
        directive.replace(parallel_code);
    }

    void Base::parallel_for_handler_pre(TL::PragmaCustomStatement directive)
    {
        if (this->in_ompss_mode())
        {
            for_handler_pre(directive);
            return;
        }

        nest_context_in_pragma(directive);
    }
    void Base::parallel_for_handler_post(TL::PragmaCustomStatement directive)
    {
        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "PARALLEL FOR construct\n"
                << directive.get_locus_str() << ": " << "----------------------\n"
                << directive.get_locus_str() << ": " << directive.get_statements().prettyprint() << "\n"
                ;
        }
        if (this->in_ompss_mode())
        {
            // In OmpSs this is like a simple for
            warn_printf_at(directive.get_locus(), "explicit parallel regions do not have any effect in OmpSs\n");
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "Note that in OmpSs the PARALLEL part of PARALLEL FOR is ignored\n"
                    ;
            }
            for_handler_post(directive);
            return;
        }

        OpenMP::DataEnvironment &ds = _core.get_openmp_info()->get_data_environment(directive);
        PragmaCustomLine pragma_line = directive.get_pragma_line();
        Nodecl::List execution_environment = this->make_execution_environment_for_combined_worksharings(ds, pragma_line);

        Nodecl::NodeclBase statement = directive.get_statements();
        // This first context was added by nest_context_in_pragma
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();
        ERROR_CONDITION(!statement.is<Nodecl::Context>(), "Invalid tree", 0);
        // This is the usual context of the statements of a pragma
        statement = statement.as<Nodecl::Context>().get_in_context();
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();
        ERROR_CONDITION(!statement.is<Nodecl::Context>(), "Invalid tree", 0);

        Nodecl::NodeclBase num_threads;
        PragmaCustomClause clause = pragma_line.get_clause("num_threads");
        {
            ObjectList<Nodecl::NodeclBase> args = clause.get_arguments_as_expressions();
            if (clause.is_defined()
                    && args.size() == 1)
            {
                num_threads = args[0];
                if (emit_omp_report())
                {
                    *_omp_report_file
                        << OpenMP::Report::indent
                        << "Number of threads requested '" << num_threads.prettyprint() << "'\n";
                }
            }
            else
            {
                if (clause.is_defined())
                {
                    error_printf_at(directive.get_locus(), "ignoring invalid 'num_threads' clause\n");
                }
            }
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

        // Set implicit barrier at the exit of the combined worksharing
        execution_environment.append(
                Nodecl::OpenMP::BarrierAtEnd::make(
                    directive.get_locus()));

        PragmaCustomClause if_clause = pragma_line.get_clause("if");
        {
            ObjectList<Nodecl::NodeclBase> expr_list = if_clause.get_arguments_as_expressions(directive);
            if (if_clause.is_defined()
                    && expr_list.size() == 1)
            {
                execution_environment.append(Nodecl::OpenMP::If::make(expr_list[0].shallow_copy()));

                if (emit_omp_report())
                {
                    *_omp_report_file
                        << OpenMP::Report::indent
                        << "A team of threads will be created only if '"
                        << expr_list[0].prettyprint()
                        << "' holds\n";
                }
            }
            else
            {
                if (if_clause.is_defined())
                {
                    error_printf_at(directive.get_locus(), "ignoring invalid 'if' clause\n");
                }
                // if (emit_omp_report())
                // {
                //     *_omp_report_file
                //         << OpenMP::Report::indent
                //         << "A team of threads will always be created\n"
                //         ;
                // }
            }
        }

        // for-statement
        Nodecl::NodeclBase for_statement_code = loop_handler_post(directive,
                statement,
                /* barrier_at_end */ false,
                /* is_combined_with_parallel */ true);

        statement = directive.get_statements();
        // This first context was added by nest_context_in_pragma
        ERROR_CONDITION(!statement.is<Nodecl::List>(), "Invalid tree", 0);
        statement = statement.as<Nodecl::List>().front();
        ERROR_CONDITION(!statement.is<Nodecl::Context>(), "Invalid tree", 0);
        // Nest the for in the place where we expect the for-statement code
        statement.as<Nodecl::Context>().set_in_context(for_statement_code);

        Nodecl::NodeclBase parallel_code
            = Nodecl::OpenMP::Parallel::make(
                    execution_environment,
                    num_threads,
                    directive.get_statements().shallow_copy(),
                    directive.get_locus());

        pragma_line.diagnostic_unused_clauses();
        directive.replace(parallel_code);
    }

    void Base::threadprivate_handler_pre(TL::PragmaCustomDirective) { }
    void Base::threadprivate_handler_post(TL::PragmaCustomDirective directive)
    {
        TL::PragmaCustomLine pragma_line = directive.get_pragma_line();
        OpenMP::DataEnvironment &ds = _core.get_openmp_info()->get_data_environment(directive);

        if (emit_omp_report())
        {
            *_omp_report_file
                << "\n"
                << directive.get_locus_str() << ": " << "THREADPRIVATE construct\n"
                << directive.get_locus_str() << ": " << "-----------------------\n"
                ;
        }

        TL::ObjectList<Symbol> threadprivate_symbols;
        ds.get_all_symbols(OpenMP::DS_THREADPRIVATE, threadprivate_symbols);

        if (!threadprivate_symbols.empty())
        {
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "List of variables set as threadprivate\n"
                    ;
            }

            for (TL::ObjectList<Symbol>::iterator it = threadprivate_symbols.begin();
                    it != threadprivate_symbols.end();
                    it++)
            {
                TL::Symbol &sym(*it);
                if (emit_omp_report())
                {
                    *_omp_report_file
                        << OpenMP::Report::indent
                        << sym.get_qualified_name() << std::endl;
                        ;
                }

                // Mark as __thread
                scope_entry_t* entry = sym.get_internal_symbol();
                symbol_entity_specs_set_is_thread(entry, 1);

                if (IS_FORTRAN_LANGUAGE)
                {
                    error_printf_at(directive.get_locus(), "!$OMP THREADPRIVATE is not supported in Fortran\n");
                }
            }
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

    void Base::unregister_handler_pre(TL::PragmaCustomDirective) { }
    void Base::unregister_handler_post(TL::PragmaCustomDirective directive)
    {
        TL::PragmaCustomLine pragma_line = directive.get_pragma_line();
        PragmaCustomParameter parameter = pragma_line.get_parameter();

        if (!parameter.is_defined())
        {
            error_printf_at(directive.get_locus(),
                    "missing parameter clause in '#pragma omp unregister'\n");
            return;
        }

        ObjectList<Nodecl::NodeclBase> expr_list = parameter.get_arguments_as_expressions();
        if (expr_list.empty())
        {
            warn_printf_at(directive.get_locus(), "ignoring empty '#pragma omp unregister\n");
            return;
        }

        ObjectList<Nodecl::NodeclBase> valid_expr_list;

        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = expr_list.begin();
                it != expr_list.end();
                it++)
        {
            // We only support symbols
            if (it->is<Nodecl::Symbol>())
            {
                valid_expr_list.append(*it);
            }
            else
            {
                error_printf_at(
                        directive.get_locus(),
                        "object specification '%s' in '#pragma omp unregister' "
                        "must be a variable-name\n",
                        it->prettyprint().c_str());
            }
        }

        if (valid_expr_list.empty())
            return;

        Nodecl::List list_expr = Nodecl::List::make(valid_expr_list);

        Nodecl::OmpSs::Unregister new_unregister_directive =
            Nodecl::OmpSs::Unregister::make(
                    list_expr,
                    directive.get_locus());

        pragma_line.diagnostic_unused_clauses();
        directive.replace(new_unregister_directive);
    }

    void Base::register_handler_pre(TL::PragmaCustomDirective) { }
    void Base::register_handler_post(TL::PragmaCustomDirective directive)
    {
        TL::PragmaCustomLine pragma_line = directive.get_pragma_line();
        PragmaCustomParameter parameter = pragma_line.get_parameter();

        if (!parameter.is_defined())
        {
            error_printf_at(directive.get_locus(),
                    "missing parameter clause in '#pragma omp register'\n");
            return;
        }

        ObjectList<Nodecl::NodeclBase> expr_list = parameter.get_arguments_as_expressions();
        if (expr_list.empty())
        {
            warn_printf_at(directive.get_locus(), "ignoring empty '#pragma omp register'\n");
            return;
        }

        ObjectList<Nodecl::NodeclBase> valid_expr_list;

        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = expr_list.begin();
                it != expr_list.end();
                it++)
        {
            if (it->is<Nodecl::Symbol>() // x
                    || (it->is<Nodecl::Shaping>() // [n1][n2] x
                        && it->as<Nodecl::Shaping>().get_postfix().is<Nodecl::Symbol>()))
            {
                valid_expr_list.append(*it);
            }
            else
            {
                error_printf_at(
                        directive.get_locus(),
                        "object specification '%s' in '#pragma omp register' "
                        "must be a variable-name or a shaping of a variable-name\n",
                        it->prettyprint().c_str());
            }
        }

        if (valid_expr_list.empty())
            return;

        Nodecl::List list_expr = Nodecl::List::make(valid_expr_list);

        Nodecl::OmpSs::Register new_register_directive =
            Nodecl::OmpSs::Register::make(
                    list_expr,
                    directive.get_locus());

        pragma_line.diagnostic_unused_clauses();
        directive.replace(new_register_directive);
    }

    struct SymbolBuilder
    {
        private:
            const locus_t* _locus;

        public:
            SymbolBuilder(const locus_t* locus)
                : _locus(locus)
            {
            }

            Nodecl::NodeclBase operator()(TL::Symbol arg) const
            {
                return arg.make_nodecl(/*set_ref*/true, _locus);
            }
    };

    struct SymbolReasonBuilder
    {
        private:
            const locus_t* _locus;

        public:
            SymbolReasonBuilder(const locus_t* locus)
                : _locus(locus)
            {
            }

            Nodecl::NodeclBase operator()(DataEnvironment::DataSharingInfoPair arg) const
            {
                return arg.first.make_nodecl(/*set_ref*/true, _locus);
            }
    };

    struct ReportSymbols
    {
        private:
            DataSharingAttribute _data_sharing;
            std::ofstream *_omp_report_file;

            std::string string_of_data_sharing(DataSharingAttribute data_attr) const
            {
                std::string result;

                switch (data_attr)
                {
#define CASE(x, str) case x : result += str; break;
                    CASE(DS_UNDEFINED, "<<undefined>>")
                    CASE(DS_SHARED, "shared")
                    CASE(DS_PRIVATE, "private")
                    CASE(DS_FIRSTPRIVATE, "firstprivate")
                    CASE(DS_LASTPRIVATE, "lastprivate")
                    CASE(DS_FIRSTLASTPRIVATE, "firstprivate and lastprivate")
                    CASE(DS_REDUCTION, "reduction")
                    CASE(DS_THREADPRIVATE, "threadprivate")
                    CASE(DS_COPYIN, "copyin")
                    CASE(DS_COPYPRIVATE, "copyprivate")
                    CASE(DS_NONE, "<<none>>")
                    CASE(DS_AUTO, "auto")
#undef CASE
                    default: result += "<<???unknown>>";
                }

                return result;
            }

        public:
            ReportSymbols(const locus_t*,
                    DataSharingAttribute data_sharing,
                    std::ofstream* omp_report_file)
                : _data_sharing(data_sharing),
                _omp_report_file(omp_report_file)
            {
            }

            void operator()(DataEnvironment::DataSharingInfoPair arg) const
            {
                // These variables confuse the user
                if (arg.first.is_saved_expression())
                    return;

                // Let's make sure this is properly aligned
                std::stringstream ss;
                ss
                    << OpenMP::Report::indent
                    << OpenMP::Report::indent
                    << arg.first.get_qualified_name()
                    ;

                int length = ss.str().size();
                int diff = 10 - length;
                if (diff > 0)
                    std::fill_n( std::ostream_iterator<const char*>(ss), diff, " ");

                ss << " " << string_of_data_sharing(_data_sharing);

                length = ss.str().size();
                diff = 20 - length;
                if (diff > 0)
                    std::fill_n( std::ostream_iterator<const char*>(ss), diff, " ");

                ss << " (" << arg.second << ")" << std::endl;

                *_omp_report_file << ss.str();
            }
    };

    struct ReductionSymbolBuilder
    {
        private:
            const locus_t* _locus;

        public:
            ReductionSymbolBuilder(const locus_t* locus)
                : _locus(locus)
            {
            }

            Nodecl::NodeclBase operator()(ReductionSymbol arg) const
            {
                return Nodecl::OpenMP::ReductionItem::make(
                        /* reductor */ Nodecl::Symbol::make(arg.get_reduction()->get_symbol(), _locus),
                        /* reduced symbol */ arg.get_symbol().make_nodecl(/* set_ref_type */ true, _locus),
                        /* reduction type */ Nodecl::Type::make(arg.get_reduction_type(), _locus),
                        _locus);
            }
    };

    struct ReportReductions
    {
        private:
            std::ofstream* _omp_report_file;

        public:
            ReportReductions(const locus_t*,
                    std::ofstream* omp_report_file)
                : _omp_report_file(omp_report_file)
            {
            }

            void operator()(ReductionSymbol arg) const
            {
                std::stringstream ss;
                ss
                    << OpenMP::Report::indent
                    << OpenMP::Report::indent
                    << arg.get_symbol().get_qualified_name()
                    ;

                int length = ss.str().size();
                int diff = 10 - length;
                if (diff > 0)
                    std::fill_n( std::ostream_iterator<const char*>(ss), diff, " ");

                ss << "reduction";

                length = ss.str().size();
                diff = 26 - length;
                if (diff > 0)
                    std::fill_n( std::ostream_iterator<const char*>(ss), diff, " ");

                ss
                    << " (explicitly declared as reduction in 'reduction' clause'."
                    " Using reduction declared in '"
                    << arg.get_reduction()->get_symbol().get_locus_str() << ")\n";

                *_omp_report_file
                    << ss.str();
            }
    };

    template <typename T>
    void Base::make_data_sharing_list(
            OpenMP::DataEnvironment &data_sharing_env,
            OpenMP::DataSharingAttribute data_attr,
            const locus_t* locus,
            ObjectList<Nodecl::NodeclBase>& result_list)
    {
        TL::ObjectList<DataEnvironment::DataSharingInfoPair> symbols;
        data_sharing_env.get_all_symbols_info(data_attr, symbols);

        if (!symbols.empty())
        {
            TL::ObjectList<Nodecl::NodeclBase> nodecl_symbols =
                symbols.map<Nodecl::NodeclBase>(SymbolReasonBuilder(locus));

            if (emit_omp_report())
            {
                symbols.map(ReportSymbols(locus, data_attr, this->_omp_report_file));
            }

            result_list.append(T::make(Nodecl::List::make(nodecl_symbols), locus));
        }
    }

    void Base::make_execution_environment_target_information(
            TL::OmpSs::TargetInfo &target_info,
            TL::Symbol called_symbol,
            const locus_t* locus,
            // out
            TL::ObjectList<Nodecl::NodeclBase> &result_list)
    {
        TL::ObjectList<Nodecl::NodeclBase> devices;
        TL::ObjectList<Nodecl::NodeclBase> target_items;

        ObjectList<std::string> device_list = target_info.get_device_list();
        for (TL::ObjectList<std::string>::iterator it = device_list.begin(); it != device_list.end(); ++it)
        {
            devices.append(Nodecl::Text::make(*it, locus));
        }

        ObjectList<TL::OmpSs::CopyItem> copy_in = target_info.get_copy_in();
        ObjectList<TL::OmpSs::CopyItem> copy_out = target_info.get_copy_out();
        ObjectList<TL::OmpSs::CopyItem> copy_inout = target_info.get_copy_inout();
        if (emit_omp_report())
        {
            if (!copy_in.empty()
                    || !copy_out.empty()
                    || !copy_inout.empty())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "Copies\n"
                    ;
            }
        }
        make_copy_list<Nodecl::OmpSs::CopyIn>(
                copy_in,
                TL::OmpSs::COPY_DIR_IN,
                locus,
                target_items);

        make_copy_list<Nodecl::OmpSs::CopyOut>(
                copy_out,
                TL::OmpSs::COPY_DIR_OUT,
                locus,
                target_items);

        make_copy_list<Nodecl::OmpSs::CopyInout>(
                copy_inout,
                TL::OmpSs::COPY_DIR_INOUT,
                locus,
                target_items);

        ObjectList<Nodecl::NodeclBase> ndrange_exprs = target_info.get_shallow_copy_of_ndrange();

        if (!ndrange_exprs.empty())
        {
            target_items.append(
                    Nodecl::OmpSs::NDRange::make(
                        Nodecl::List::make(ndrange_exprs),
                        locus));
        }

        ObjectList<Nodecl::NodeclBase> shmem_exprs = target_info.get_shallow_copy_of_shmem();
        if (!shmem_exprs.empty())
        {
            target_items.append(
                    Nodecl::OmpSs::ShMem::make(
                        Nodecl::List::make(shmem_exprs),
                        locus));
        }

        ObjectList<Nodecl::NodeclBase> onto_exprs = target_info.get_shallow_copy_of_onto();
        if (!onto_exprs.empty())
        {
            target_items.append(
                    Nodecl::OmpSs::Onto::make(
                        Nodecl::List::make(onto_exprs),
                        locus));
        }

        std::string file = target_info.get_file();
        if (!file.empty())
        {
            target_items.append(
                    Nodecl::OmpSs::File::make(
                        Nodecl::Text::make(file),
                        locus));
        }

        std::string name = target_info.get_name();
        if (!name.empty())
        {
            target_items.append(
                    Nodecl::OmpSs::Name::make(
                        Nodecl::Text::make(name),
                        locus));
        }

        TL::OmpSs::TargetInfo::implementation_table_t implementation_table = target_info.get_implementation_table();
        for (TL::OmpSs::TargetInfo::implementation_table_t::iterator it = implementation_table.begin();
                it != implementation_table.end(); ++it)
        {
            std::string device_name = it->first;
            TL::ObjectList<Symbol> &implementors = it->second;
            for (TL::ObjectList<Symbol>::iterator it2 = implementors.begin();
                    it2 != implementors.end();
                    it2++)
            {
                TL::Symbol implementor = *it2;
                target_items.append(
                        Nodecl::OmpSs::Implements::make(
                            Nodecl::Text::make(device_name),
                            Nodecl::Symbol::make(implementor, locus),
                            locus));
            }
        }

        result_list.append(
                Nodecl::OmpSs::Target::make(
                    Nodecl::List::make(devices),
                    Nodecl::List::make(target_items),
                    locus));
    }

    Nodecl::List Base::make_execution_environment_for_combined_worksharings(
            OpenMP::DataEnvironment &data_sharing_env,
            PragmaCustomLine pragma_line)
    {
        const locus_t* locus = pragma_line.get_locus();

        TL::ObjectList<Nodecl::NodeclBase> result_list;

        // We do not want a report for this sort of worksharings because it is confusing for users
        bool old_emit_omp_report = this->_omp_report;
        this->_omp_report = false;

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
                data_sharing_env, OpenMP::DS_SHARED,
                locus,
                result_list);

        make_data_sharing_list<Nodecl::OpenMP::Threadprivate>(
                data_sharing_env, OpenMP::DS_THREADPRIVATE,
                locus,
                result_list);

        TL::ObjectList<ReductionSymbol> reductions;
        data_sharing_env.get_all_reduction_symbols(reductions);
        TL::ObjectList<Symbol> reduction_symbols =
            reductions.map<TL::Symbol>(&ReductionSymbol::get_symbol);
        if (!reduction_symbols.empty())
        {
            TL::ObjectList<Nodecl::NodeclBase> nodecl_symbols =
                reduction_symbols.map<Nodecl::NodeclBase>(SymbolBuilder(locus));

            result_list.append(Nodecl::OpenMP::Shared::make(Nodecl::List::make(nodecl_symbols),
                        locus));
        }

        TL::OmpSs::TargetInfo& target_info = data_sharing_env.get_target_info();
        make_execution_environment_target_information(
                target_info,
                target_info.get_target_symbol(),
                locus,
                result_list);


        // FIXME - Dependences for combined worksharings???
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

        this->_omp_report = old_emit_omp_report;

        return Nodecl::List::make(result_list);
    }

    Nodecl::List Base::make_execution_environment(
            OpenMP::DataEnvironment &data_sharing_env,
            PragmaCustomLine pragma_line,
            bool ignore_target_info,
            bool is_inline_task)
    {
        const locus_t* locus = pragma_line.get_locus();

        TL::ObjectList<Nodecl::NodeclBase> result_list;

        if (emit_omp_report())
        {
            *_omp_report_file
                << OpenMP::Report::indent
                << "Data sharings of variables\n"
                ;
        }
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

        make_data_sharing_list<Nodecl::OpenMP::Shared>(
                data_sharing_env, OpenMP::DS_SHARED,
                locus,
                result_list);

        make_data_sharing_list<Nodecl::OpenMP::Threadprivate>(
                data_sharing_env, OpenMP::DS_THREADPRIVATE,
                locus,
                result_list);

        TL::ObjectList<ReductionSymbol> reductions;
        data_sharing_env.get_all_reduction_symbols(reductions);
        if (!reductions.empty())
        {
            TL::ObjectList<Nodecl::NodeclBase> reduction_nodes =
                reductions.map<Nodecl::NodeclBase>(ReductionSymbolBuilder(locus));

            if (emit_omp_report())
            {
                reductions.map(ReportReductions(locus, this->_omp_report_file));
            }

            if (is_inline_task)
            {
                result_list.append(
                        Nodecl::OpenMP::TaskReduction::make(Nodecl::List::make(reduction_nodes), locus));
            }
            else
            {
                result_list.append(
                        Nodecl::OpenMP::Reduction::make(Nodecl::List::make(reduction_nodes), locus));
            }
        }

        TL::ObjectList<ReductionSymbol> simd_reductions;
        data_sharing_env.get_all_simd_reduction_symbols(simd_reductions);
        if (!simd_reductions.empty())
        {
            TL::ObjectList<Nodecl::NodeclBase> simd_reduction_nodes =
                simd_reductions.map<Nodecl::NodeclBase>(ReductionSymbolBuilder(locus));

            result_list.append(
                    Nodecl::OpenMP::SimdReduction::make(Nodecl::List::make(simd_reduction_nodes),
                        locus)
                    );
        }

        if (emit_omp_report())
        {
            if (result_list.empty())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << OpenMP::Report::indent
                    << "There are no data sharings\n"
                    ;
            }
        }

        TL::ObjectList<OpenMP::DependencyItem> dependences;
        data_sharing_env.get_all_dependences(dependences);

        if (emit_omp_report())
        {
            *_omp_report_file
                << OpenMP::Report::indent
                << "Dependences\n"
                ;
            if (dependences.empty())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << OpenMP::Report::indent
                    << "There are no dependences\n"
                    ;
            }
        }

        make_dependency_list<Nodecl::OpenMP::DepIn>(
                dependences,
                OpenMP::DEP_DIR_IN,
                locus,
                result_list);

        make_dependency_list<Nodecl::OmpSs::DepWeakIn>(
                dependences,
                OpenMP::DEP_OMPSS_WEAK_IN,
                locus,
                result_list);

        make_dependency_list<Nodecl::OmpSs::DepInPrivate>(
                dependences,
                OpenMP::DEP_OMPSS_DIR_IN_PRIVATE,
                locus,
                result_list);

        make_dependency_list<Nodecl::OpenMP::DepOut>(
                dependences,
                OpenMP::DEP_DIR_OUT,
                locus,
                result_list);

        make_dependency_list<Nodecl::OmpSs::DepWeakOut>(
                dependences,
                OpenMP::DEP_OMPSS_WEAK_OUT,
                locus,
                result_list);

        make_dependency_list<Nodecl::OpenMP::DepInout>(
                dependences, OpenMP::DEP_DIR_INOUT,
                locus,
                result_list);

        make_dependency_list<Nodecl::OmpSs::DepWeakInout>(
                dependences,
                OpenMP::DEP_OMPSS_WEAK_INOUT,
                locus,
                result_list);

        make_dependency_list<Nodecl::OmpSs::Concurrent>(
                dependences, OpenMP::DEP_OMPSS_CONCURRENT,
                locus,
                result_list);

        make_dependency_list<Nodecl::OmpSs::Commutative>(
                dependences, OpenMP::DEP_OMPSS_COMMUTATIVE,
                locus,
                result_list);

        if (!ignore_target_info)
        {
            // Build the tree which contains the target information
            TL::OmpSs::TargetInfo& target_info = data_sharing_env.get_target_info();
            make_execution_environment_target_information(
                    target_info,
                    target_info.get_target_symbol(),
                    locus,
                    result_list);
        }

        return Nodecl::List::make(result_list);
    }



    // Note that num_tasks_sym and grainsize_adjustment_sym symbols are only created if
    // require_conversion_num_tasks_to_grainsize is true. Thus, they are only used to
    // transform the 'num_task' clause to the 'grainsize' clause.
    void create_auxiliar_symbols(
            int counter,
            TL::Type type,
            TL::Scope scope_of_directive,
            bool require_conversion_num_tasks_to_grainsize,
            // Out
            TL::Symbol& grainsize_sym,
            TL::Symbol &num_tasks_sym,
            TL::Symbol& grainsize_adjustment_sym)

    {
        std::stringstream ss;
        ss << "omp_grainsize_" << counter;
        grainsize_sym = scope_of_directive.new_symbol(ss.str());
        grainsize_sym.get_internal_symbol()->kind = SK_VARIABLE;
        grainsize_sym.set_type(type);
        symbol_entity_specs_set_is_user_declared(grainsize_sym.get_internal_symbol(), 1);

        if (require_conversion_num_tasks_to_grainsize)
        {
            ss.str("");
            ss << "omp_num_tasks_" << counter;
            num_tasks_sym = scope_of_directive.new_symbol(ss.str());
            num_tasks_sym.get_internal_symbol()->kind = SK_VARIABLE;
            num_tasks_sym.set_type(type);
            symbol_entity_specs_set_is_user_declared(num_tasks_sym.get_internal_symbol(), 1);

            ss.str("");
            ss << "omp_it_adjustment_" << counter;
            grainsize_adjustment_sym = scope_of_directive.new_symbol(ss.str());
            grainsize_adjustment_sym.get_internal_symbol()->kind = SK_VARIABLE;
            grainsize_adjustment_sym.set_type(type);
            symbol_entity_specs_set_is_user_declared(grainsize_adjustment_sym.get_internal_symbol(), 1);
        }
    }

    Nodecl::NodeclBase convert_num_task_expr_to_grainsize_expr(
            const TL::ForStatement& for_statement,
            Nodecl::NodeclBase num_tasks_expr,
            TL::Symbol grainsize_sym,
            TL::Symbol num_tasks_sym,
            TL::Symbol grainsize_adjustment_sym,
            // Out
            Nodecl::List& new_body)
    {
        // First: introduce the definitions of these extra symbols used to simplify the code generation
        if (IS_CXX_LANGUAGE)
        {
            new_body.append(Nodecl::CxxDef::make(/* context */ nodecl_null(),
                        num_tasks_sym, num_tasks_sym.get_locus()));

            new_body.append(Nodecl::CxxDef::make( /* context */ nodecl_null(),
                        grainsize_adjustment_sym, grainsize_adjustment_sym.get_locus()));
        }

        // Second: storing the value of the 'num_tasks_expr' in a new variable
        Nodecl::NodeclBase assign_num_tasks = Nodecl::ExpressionStatement::make(
                Nodecl::Assignment::make(
                    num_tasks_sym.make_nodecl(),
                    num_tasks_expr,
                    num_tasks_sym.get_type().get_lvalue_reference_to()));
        new_body.append(assign_num_tasks);

        // Third: computing the real number of iterations and storing it in the 'grainsize_sym' symbol
        //                grainsize = ((upper - lower) + step) / step
        Nodecl::NodeclBase real_num_iterations_assignment
            = Nodecl::ExpressionStatement::make(
                    Nodecl::Assignment::make(
                        grainsize_sym.make_nodecl(),
                        Nodecl::Div::make(
                            Nodecl::ParenthesizedExpression::make(
                                Nodecl::Add::make(
                                    Nodecl::Minus::make(
                                        for_statement.get_upper_bound().shallow_copy(),
                                        for_statement.get_lower_bound().shallow_copy(),
                                        for_statement.get_upper_bound().get_type()),
                                    for_statement.get_step().shallow_copy(),
                                    for_statement.get_step().get_type()),
                                for_statement.get_step().get_type()),
                            for_statement.get_step().shallow_copy(),
                            for_statement.get_step().get_type()),
                        grainsize_sym.get_type().get_lvalue_reference_to()));
        new_body.append(real_num_iterations_assignment);

        // Fourth: If (real_num_iterations % num_tasks != 0) then some tasks will have an additional iteration.
        // The 'grainsize_adjustment_sym' symbol holds the first index that must have this extra iteration.
        //
        //  grainsize_adjustment  = lower_bound +
        //                          (   (num_tasks - (num_iterations % num_tasks)) *
        //                              step *
        //                              (num_iterations / num_tasks)
        //                          )
        Nodecl::NodeclBase grainsize_adjustment
            = Nodecl::ExpressionStatement::make(
                    Nodecl::Assignment::make(
                        grainsize_adjustment_sym.make_nodecl(),
                        Nodecl::Add::make(
                            for_statement.get_lower_bound().shallow_copy(),
                            Nodecl::Mul::make(
                                Nodecl::ParenthesizedExpression::make(
                                Nodecl::Minus::make(
                                    num_tasks_sym.make_nodecl(),
                                    Nodecl::Mod::make(
                                        grainsize_sym.make_nodecl(),
                                        num_tasks_sym.make_nodecl(),
                                        grainsize_sym.get_type()),
                                    num_tasks_sym.get_type()),
                                num_tasks_sym.get_type()),
                                Nodecl::Mul::make(
                                    for_statement.get_step().shallow_copy(),
                                    Nodecl::ParenthesizedExpression::make(
                                    Nodecl::Div::make(
                                        grainsize_sym.make_nodecl(),
                                        num_tasks_sym.make_nodecl(),
                                        grainsize_sym.get_type()),
                                    grainsize_sym.get_type()),
                                    for_statement.get_step().get_type()),
                                num_tasks_sym.get_type()),
                            for_statement.get_lower_bound().get_type()),
                        grainsize_adjustment_sym.get_type().get_lvalue_reference_to()));
        new_body.append(grainsize_adjustment);

        // Fifth: Finally, we compute the 'grainsize_expr' expression
        // grainsize = (((upper - lower) + step) / step) / num_tasks
        Nodecl::NodeclBase grainsize_expr = Nodecl::Div::make(
                grainsize_sym.make_nodecl(),
                num_tasks_sym.make_nodecl(),
                grainsize_sym.get_type());

        return grainsize_expr;
    }

    Nodecl::NodeclBase taskloop_generate_outer_loop(
            int counter,
            const TL::ForStatement& for_statement,
            Nodecl::NodeclBase grainsize_expr,
            Nodecl::NodeclBase num_tasks_expr,
            TL::Symbol taskloop_ivar,
            TL::Symbol block_extent,
            Nodecl::NodeclBase new_task,
            TL::Scope new_outer_loop_context,
            TL::Scope new_outer_loop_body_context,
            TL::Scope scope_of_directive,
            const locus_t* locus)
    {
        bool require_conversion_num_tasks_to_grainsize = !num_tasks_expr.is_null();

        TL::Symbol grainsize_sym, num_tasks_sym, grainsize_adjustment_sym;
        create_auxiliar_symbols(
                counter,
                taskloop_ivar.get_type(),
                scope_of_directive,
                require_conversion_num_tasks_to_grainsize,
                /* out */
                grainsize_sym,
                num_tasks_sym,
                grainsize_adjustment_sym);


        Nodecl::List new_body;
        if (IS_CXX_LANGUAGE)
        {
            new_body.append(Nodecl::CxxDef::make(/* context */ nodecl_null(),
                        taskloop_ivar, taskloop_ivar.get_locus()));

            new_body.append(Nodecl::CxxDef::make(/* context */ nodecl_null(),
                        grainsize_sym, grainsize_sym.get_locus()));
        }

        if (require_conversion_num_tasks_to_grainsize)
        {
            grainsize_expr = convert_num_task_expr_to_grainsize_expr(
                    for_statement, num_tasks_expr, grainsize_sym, num_tasks_sym,
                    grainsize_adjustment_sym, /* out */ new_body);
        }

        // Initialize the 'grainsize_sym' symbol with the 'grainsize_expr' expression
        new_body.append(Nodecl::ExpressionStatement::make(
                    Nodecl::Assignment::make(
                        grainsize_sym.make_nodecl(),
                        grainsize_expr,
                        grainsize_sym.get_type().get_lvalue_reference_to())));

        Nodecl::NodeclBase init_block_extent;
        {
            // taskloop_ivar + (grainsize_expr * step)
            Nodecl::NodeclBase expr =
                Nodecl::Add::make(
                        taskloop_ivar.make_nodecl(),
                        Nodecl::Mul::make(
                            grainsize_sym.make_nodecl(),
                            for_statement.get_step().shallow_copy(),
                            for_statement.get_induction_variable().get_type()),
                        taskloop_ivar.get_type());

            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                if (for_statement.is_strictly_increasing_loop())
                {
                    expr = Nodecl::Minus::make(
                            expr,
                            const_value_to_nodecl(const_value_get_one(4, 1)),
                            expr.get_type());
                }
                else
                {
                    expr = Nodecl::Add::make(
                            expr,
                            const_value_to_nodecl(const_value_get_one(4, 1)),
                            expr.get_type());
                }

                init_block_extent = Nodecl::ExpressionStatement::make(
                        Nodecl::Assignment::make(
                            block_extent.make_nodecl(),
                            expr,
                            block_extent.get_type().get_lvalue_reference_to()));
            }
            else /* IS_FORTRAN_LANGUAGE */
            {
                init_block_extent =
                    Nodecl::IfElseStatement::make(

                            Nodecl::LowerThan::make(
                                const_value_to_nodecl(const_value_get_zero(4, 1)),
                                Nodecl::Mul::make(
                                    grainsize_expr.shallow_copy(),
                                    for_statement.get_step().shallow_copy(),
                                    for_statement.get_induction_variable().get_type()),
                                get_bool_type()),

                            Nodecl::List::make(
                                Nodecl::ExpressionStatement::make(
                                    Nodecl::Assignment::make(
                                        block_extent.make_nodecl(),
                                        Nodecl::Minus::make(
                                            expr,
                                            const_value_to_nodecl(const_value_get_signed_int(1)),
                                            taskloop_ivar.get_type()),
                                        block_extent.get_type().get_lvalue_reference_to()))),

                            Nodecl::List::make(
                                Nodecl::ExpressionStatement::make(
                                    Nodecl::Assignment::make(
                                        block_extent.make_nodecl(),
                                        Nodecl::Add::make(
                                            expr.shallow_copy(),
                                            const_value_to_nodecl(const_value_get_signed_int(1)),
                                            taskloop_ivar.get_type()),
                                        block_extent.get_type().get_lvalue_reference_to()))));
            }
        }


        Nodecl::NodeclBase adjust_block_extent;
        {
            Nodecl::NodeclBase condition;
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                typedef Nodecl::NodeclBase (*ptr_to_func_t)(Nodecl::NodeclBase, Nodecl::NodeclBase, TL::Type, const locus_t*);
                ptr_to_func_t make_relative_operator;
                if (for_statement.is_strictly_increasing_loop())
                    make_relative_operator = (ptr_to_func_t) &Nodecl::GreaterThan::make;
                else
                    make_relative_operator = (ptr_to_func_t) &Nodecl::LowerThan::make;

                condition = (*make_relative_operator)(
                        block_extent.make_nodecl(),
                        for_statement.get_upper_bound().shallow_copy(),
                        get_bool_type(),
                        0);
            }
            else /* IS_FORTRAN_LANGUAGE */
            {
                condition = Nodecl::LogicalOr::make(
                        Nodecl::LogicalAnd::make(
                            Nodecl::LowerThan::make(
                                const_value_to_nodecl(const_value_get_zero(4, 1)),
                                Nodecl::Mul::make(
                                    grainsize_sym.make_nodecl(),
                                    for_statement.get_step().shallow_copy(),
                                    for_statement.get_induction_variable().get_type()),
                                get_bool_type()),
                            Nodecl::LowerThan::make(
                                for_statement.get_upper_bound().shallow_copy(),
                                block_extent.make_nodecl(),
                                get_bool_type()),
                            get_bool_type()),
                        Nodecl::LogicalAnd::make(
                            Nodecl::GreaterThan::make(
                                const_value_to_nodecl(const_value_get_zero(4, 1)),
                                Nodecl::Mul::make(
                                    grainsize_sym.make_nodecl(),
                                    for_statement.get_step().shallow_copy(),
                                    for_statement.get_induction_variable().get_type()),
                                get_bool_type()),
                            Nodecl::GreaterThan::make(
                                for_statement.get_upper_bound().shallow_copy(),
                                block_extent.make_nodecl(),
                                get_bool_type()),
                            get_bool_type()),
                        get_bool_type());
            }

            adjust_block_extent =
                Nodecl::IfElseStatement::make(
                        condition,
                        Nodecl::List::make(
                            Nodecl::ExpressionStatement::make(
                                Nodecl::Assignment::make(
                                    block_extent.make_nodecl(),
                                    for_statement.get_upper_bound().shallow_copy(),
                                    block_extent.get_type().get_lvalue_reference_to()))),
                        Nodecl::NodeclBase::null());
        }

        Nodecl::Mul blocked_step =
            Nodecl::Mul::make(
                    grainsize_sym.make_nodecl(),
                    for_statement.get_step().shallow_copy(),
                    for_statement.get_induction_variable().get_type());

        if (blocked_step.get_lhs().is_constant()
                && blocked_step.get_rhs().is_constant())
        {
            blocked_step.set_constant(
                    const_value_mul(
                        blocked_step.get_lhs().get_constant(),
                        blocked_step.get_rhs().get_constant()));
        }

        Nodecl::NodeclBase new_outer_loop_control;
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            Nodecl::NodeclBase init =
                Nodecl::List::make(
                        Nodecl::Assignment::make(
                            taskloop_ivar.make_nodecl(),
                            for_statement.get_lower_bound(),
                            taskloop_ivar.get_type().get_lvalue_reference_to()));

            typedef Nodecl::NodeclBase (*ptr_to_func_t)(Nodecl::NodeclBase, Nodecl::NodeclBase, TL::Type, const locus_t*);
            ptr_to_func_t make_relative_operator;
            if (for_statement.is_strictly_increasing_loop())
                make_relative_operator = (ptr_to_func_t) &Nodecl::LowerOrEqualThan::make;
            else
                make_relative_operator = (ptr_to_func_t) &Nodecl::GreaterOrEqualThan::make;

            Nodecl::NodeclBase cond =
                (*make_relative_operator)(
                        taskloop_ivar.make_nodecl(),
                        for_statement.get_upper_bound(),
                        get_bool_type(),
                        0);

            Nodecl::NodeclBase next =
                Nodecl::AddAssignment::make(
                        taskloop_ivar.make_nodecl(),
                        blocked_step,
                        taskloop_ivar.get_type().get_lvalue_reference_to());

            new_outer_loop_control = Nodecl::LoopControl::make(init, cond, next);
        }
        else
        {
            new_outer_loop_control = Nodecl::RangeLoopControl::make(
                    taskloop_ivar.make_nodecl(),
                    for_statement.get_lower_bound(),
                    for_statement.get_upper_bound(),
                    blocked_step,
                    locus);
        }


        Nodecl::List outer_loop_body_statements;

        if (require_conversion_num_tasks_to_grainsize)
        {
            // If the number of iterations is not divisble by the number of
            // tasks some of them have to execute an extra iteration
            Nodecl::NodeclBase dyn_adjustment_grainsize =
                Nodecl::IfElseStatement::make(
                        // Cond: taskloop_ivar == grainsize_adjustment_sym
                        Nodecl::Equal::make(
                            taskloop_ivar.make_nodecl(),
                            grainsize_adjustment_sym.make_nodecl(),
                            get_bool_type()),
                        // Then: grainsize++;
                        Nodecl::List::make(
                            Nodecl::ExpressionStatement::make(
                                Nodecl::Assignment::make(
                                    grainsize_sym.make_nodecl(),
                                    Nodecl::Add::make(
                                        grainsize_sym.make_nodecl(),
                                        const_value_to_nodecl(const_value_get_signed_int(1)),
                                        grainsize_sym.get_type()),
                                    grainsize_sym.get_type().get_lvalue_reference_to()))),
                        // else:
                        nodecl_null());
            outer_loop_body_statements.append(dyn_adjustment_grainsize);
        }

        if (IS_CXX_LANGUAGE)
                outer_loop_body_statements.append(
                        Nodecl::CxxDef::make(nodecl_null(), block_extent, block_extent.get_locus()));

            outer_loop_body_statements.append(init_block_extent);
            outer_loop_body_statements.append(adjust_block_extent);
            outer_loop_body_statements.append(new_task);


            if (require_conversion_num_tasks_to_grainsize && IS_FORTRAN_LANGUAGE)
            {
                new_body.append(Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                taskloop_ivar.make_nodecl(),
                                for_statement.get_lower_bound().shallow_copy(),
                                taskloop_ivar.get_type().get_lvalue_reference_to())));

                outer_loop_body_statements.append(Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                taskloop_ivar.make_nodecl(),
                                Nodecl::Add::make(
                                    taskloop_ivar.make_nodecl(),
                                    Nodecl::Mul::make(
                                            grainsize_sym.make_nodecl(),
                                            for_statement.get_step().shallow_copy(),
                                            grainsize_sym.get_type()),
                                    taskloop_ivar.get_type()),
                                taskloop_ivar.get_type().get_lvalue_reference_to())));
            }

            Nodecl::List new_outer_loop_body = Nodecl::List::make(
                    Nodecl::Context::make(
                        Nodecl::List::make(
                            Nodecl::CompoundStatement::make(
                                outer_loop_body_statements,
                                /* finally */ Nodecl::NodeclBase::null())
                            ),
                        new_outer_loop_body_context)
                    );


            Nodecl::NodeclBase new_outer_loop;
            if (require_conversion_num_tasks_to_grainsize && IS_FORTRAN_LANGUAGE)
            {
                Nodecl::NodeclBase condition =
                    Nodecl::LogicalOr::make(
                            Nodecl::LogicalAnd::make(
                                Nodecl::GreaterThan::make(
                                    for_statement.get_step().shallow_copy(),
                                    const_value_to_nodecl(const_value_get_signed_int(0)),
                                    get_bool_type()),
                                Nodecl::LowerOrEqualThan::make(
                                    taskloop_ivar.make_nodecl(),
                                    for_statement.get_upper_bound().shallow_copy(),
                                    get_bool_type()),
                                get_bool_type()),

                            Nodecl::LogicalAnd::make(
                                Nodecl::LowerThan::make(
                                    for_statement.get_step().shallow_copy(),
                                    const_value_to_nodecl(const_value_get_signed_int(0)),
                                    get_bool_type()),
                                Nodecl::GreaterOrEqualThan::make(
                                    taskloop_ivar.make_nodecl(),
                                    for_statement.get_upper_bound().shallow_copy(),
                                    get_bool_type()),
                                get_bool_type()),

                            get_bool_type());


                new_outer_loop = Nodecl::WhileStatement::make(condition, new_outer_loop_body, /* loop name */ Nodecl::NodeclBase::null());
            }
            else
            {
                new_outer_loop =  Nodecl::ForStatement::make(
                        new_outer_loop_control,
                        new_outer_loop_body,
                        /* loop_name */ Nodecl::NodeclBase::null(),
                        locus);
            }


            new_body.append(new_outer_loop);

            Nodecl::NodeclBase new_statement =
                Nodecl::Context::make(new_body, new_outer_loop_context, locus);

            return new_statement;
        }

    Nodecl::NodeclBase taskloop_generate_inner_loop(
            const TL::ForStatement& for_statement,
            Nodecl::NodeclBase statement,
            TL::Symbol taskloop_ivar,
            TL::Symbol block_extent,
            TL::Scope new_outer_loop_body_context)
    {
        Nodecl::NodeclBase new_inner_loop =
            Nodecl::Utils::deep_copy(statement, new_outer_loop_body_context);

        Nodecl::ForStatement new_inner_for_statement(
                new_inner_loop.as<Nodecl::Context>()
                .get_in_context()
                .as<Nodecl::List>().front()
                .as<Nodecl::ForStatement>());

        TL::ForStatement new_for_statement(new_inner_for_statement);
        TL::Symbol new_inner_ind_var = new_for_statement.get_induction_variable();

        if (IS_CXX_LANGUAGE
                && new_inner_for_statement.get_loop_header().is<Nodecl::LoopControl>())
        {
            Nodecl::LoopControl lc = new_inner_for_statement.get_loop_header().as<Nodecl::LoopControl>();
            Nodecl::List lc_init_list = lc.get_init().as<Nodecl::List>();
            if (lc_init_list.begin()->is<Nodecl::ObjectInit>())
            {
                new_inner_ind_var.set_value(Nodecl::NodeclBase::null());

                Nodecl::Utils::prepend_items_before(new_inner_for_statement,
                        Nodecl::CxxDef::make(
                            /*context*/nodecl_null(),
                            new_inner_ind_var,
                            new_inner_ind_var.get_locus()));
            }
        }


        Nodecl::NodeclBase loop_control;
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            Nodecl::NodeclBase init =
                Nodecl::List::make(
                        Nodecl::Assignment::make(
                            new_inner_ind_var.make_nodecl(),
                            taskloop_ivar.make_nodecl(),
                            new_inner_ind_var.get_type().get_lvalue_reference_to()));

            typedef Nodecl::NodeclBase (*ptr_to_func_t)(Nodecl::NodeclBase, Nodecl::NodeclBase, TL::Type, const locus_t*);
            ptr_to_func_t make_relative_operator;
            if (for_statement.is_strictly_increasing_loop())
                make_relative_operator = (ptr_to_func_t) &Nodecl::LowerOrEqualThan::make;
            else
                make_relative_operator = (ptr_to_func_t) &Nodecl::GreaterOrEqualThan::make;

            Nodecl::NodeclBase cond =
                (*make_relative_operator)(
                        new_inner_ind_var.make_nodecl(),
                        block_extent.make_nodecl(),
                        get_bool_type(),
                        0);

            Nodecl::NodeclBase next =
                Nodecl::AddAssignment::make(
                        new_inner_ind_var.make_nodecl(),
                        new_for_statement.get_step(),
                        new_inner_ind_var.get_type().get_lvalue_reference_to());

            loop_control = Nodecl::LoopControl::make(init, cond, next);
        }
        else
        {
            loop_control = Nodecl::RangeLoopControl::make(
                    new_inner_ind_var.make_nodecl(),
                    taskloop_ivar.make_nodecl(),
                    block_extent.make_nodecl(),
                    new_for_statement.get_step(),
                    statement.get_locus());
        }


        new_inner_for_statement.set_loop_header(loop_control);

        return new_inner_loop;
    }

    void Base::taskloop_block_loop(
            Nodecl::NodeclBase directive,
            Nodecl::NodeclBase statement,
            Nodecl::NodeclBase execution_environment,
            Nodecl::NodeclBase grainsize_expr,
            Nodecl::NodeclBase num_tasks_expr)
    {
        ERROR_CONDITION(!statement.is<Nodecl::Context>(), "Invalid node", 0);

        TL::ForStatement for_statement(
                statement.as<Nodecl::Context>()
                .get_in_context()
                .as<Nodecl::List>().front()
                .as<Nodecl::ForStatement>());

        ERROR_CONDITION(!for_statement.is_omp_valid_loop(), "Invalid loop at this point", 0);

        TL::Scope scope_of_directive = directive.retrieve_context();
        TL::Scope scope_created_by_statement = statement.retrieve_context();


        // Creating a new symbol: induction variable
        Counter &c = TL::CounterManager::get_counter("taskloop");
        int counter = (int)c;
        c++;
        std::stringstream ss;
        ss << "omp_taskloop_" << counter;
        TL::Symbol taskloop_ivar = scope_of_directive.new_symbol(ss.str());
        taskloop_ivar.get_internal_symbol()->kind = SK_VARIABLE;
        taskloop_ivar.set_type(for_statement.get_induction_variable().get_type());
        symbol_entity_specs_set_is_user_declared(taskloop_ivar.get_internal_symbol(), 1);

        TL::Scope new_outer_loop_context = new_block_context(scope_of_directive.get_decl_context());
        // Properly nest the existing context to be contained in new_outer_loop_body_context
        // because we will put it inside a new compound statement
        TL::Scope new_outer_loop_body_context = new_block_context(new_outer_loop_context.get_decl_context());
        scope_created_by_statement.get_decl_context()->current_scope->contained_in =
            new_outer_loop_body_context.get_decl_context()->current_scope;

        // Creating a new symbol: upperbound loop
        ss.str("");
        ss << "omp_block_" << counter;
        TL::Symbol block_extent = new_outer_loop_body_context.new_symbol(ss.str());
        block_extent.get_internal_symbol()->kind = SK_VARIABLE;
        block_extent.set_type(for_statement.get_induction_variable().get_type());
        symbol_entity_specs_set_is_user_declared(block_extent.get_internal_symbol(), 1);
        block_extent.get_internal_symbol()->value = nodecl_null();


        Nodecl::NodeclBase new_inner_loop = taskloop_generate_inner_loop(
                for_statement, statement, taskloop_ivar, block_extent, new_outer_loop_body_context);

        // Add new vars as firstprivate
        execution_environment.as<Nodecl::List>().append(
                Nodecl::OpenMP::Firstprivate::make(
                    Nodecl::List::make(
                        Nodecl::Symbol::make(taskloop_ivar),
                        Nodecl::Symbol::make(block_extent))));

        taskloop_update_environment_renaming_induction_variable(
                execution_environment,
                for_statement.get_induction_variable(),
                taskloop_ivar);

        // taskloop_extend_dependences(
        //         execution_environment,
        //         taskloop_ivar,
        //         block_extent);

       Nodecl::NodeclBase new_task =
           Nodecl::OpenMP::Task::make(
                   execution_environment,
                   Nodecl::List::make(new_inner_loop),
                   statement.get_locus());

       Nodecl::NodeclBase new_outer_loop =
           taskloop_generate_outer_loop(
                   counter,
                   for_statement,
                   grainsize_expr,
                   num_tasks_expr,
                   taskloop_ivar,
                   block_extent,
                   new_task,
                   new_outer_loop_context, new_outer_loop_body_context, scope_of_directive,
                   statement.get_locus());

        statement.replace(new_outer_loop);
    }

    //   struct UpdateDependences : public Nodecl::ExhaustiveVisitor<void>
    //   {
    //       TL::Symbol _new_induction_var, _block_extent_var;

    //       UpdateDependences(
    //               TL::Symbol new_induction_var,
    //               TL::Symbol block_extent_var) :
    //            _new_induction_var(new_induction_var),
    //            _block_extent_var(block_extent_var) { }

    //       virtual void visit(const Nodecl::ArraySubscript& n)
    //       {
    //           Nodecl::List subscripts = n.get_subscripts().as<Nodecl::List>();

    //           for (Nodecl::List::iterator it = subscripts.begin();
    //                   it != subscripts.end();
    //                   it++)
    //           {
    //               TL::ObjectList<TL::Symbol> all_syms = Nodecl::Utils::get_all_symbols(*it);

    //               if (all_syms.contains(_new_induction_var))
    //               {
    //                   walk(*it);

    //                   if (it->is<Nodecl::Range>())
    //                   {
    //                       internal_error("Not yet implemented", 0);
    //                   }
    //                   else
    //                   {
    //                       it->replace(
    //                               Nodecl::Range::make(
    //                                   it->shallow_copy(),
    //                                   Nodecl::Minus::make(
    //                                       _block_extent_var.make_nodecl(),
    //                                       const_value_to_nodecl(const_value_get_signed_int(1)),
    //                                       _block_extent_var.get_type()),
    //                                   const_value_to_nodecl(const_value_get_signed_int(1)),
    //                                   get_signed_int_type()));
    //                   }
    //               }
    //           }
    //       }
    //   };

    //   struct UpdateDependencesEnvironment : public Nodecl::ExhaustiveVisitor<void>
    //   {
    //       TL::Symbol _new_induction_var, _block_extent_var;

    //       UpdateDependencesEnvironment(
    //               TL::Symbol new_induction_var,
    //               TL::Symbol block_extent_var)
    //            :
    //            _new_induction_var(new_induction_var),
    //            _block_extent_var(block_extent_var) { }

    //       virtual void visit(const Nodecl::OpenMP::DepIn& n)
    //       {
    //           common_dependency_handler(n);
    //       }

    //       virtual void visit(const Nodecl::OpenMP::DepOut& n)
    //       {
    //           common_dependency_handler(n);
    //       }

    //       virtual void visit(const Nodecl::OpenMP::DepInout& n)
    //       {
    //           common_dependency_handler(n);
    //       }

    //       virtual void visit(const Nodecl::OmpSs::Concurrent& n)
    //       {
    //           common_dependency_handler(n);
    //       }

    //       virtual void visit(const Nodecl::OmpSs::Commutative& n)
    //       {
    //           common_dependency_handler(n);
    //       }

    //       virtual void common_dependency_handler(Nodecl::NodeclBase n)
    //       {
    //           UpdateDependences update_dependences(
    //                   _new_induction_var,
    //                   _block_extent_var);
    //           update_dependences.walk(n);
    //       }
    //   };

    //   void Base::taskloop_extend_dependences(
    //               Nodecl::NodeclBase execution_environment,
    //               TL::Symbol new_induction_var,
    //               TL::Symbol block_extent_var)
    //   {
    //       UpdateDependencesEnvironment w(
    //               new_induction_var,
    //               block_extent_var);

    //       w.walk(execution_environment);
    //   }

    void Base::taskloop_update_environment_renaming_induction_variable(
            Nodecl::NodeclBase execution_environment,
            TL::Symbol ori_induction_var,
            TL::Symbol new_induction_var)
    {
        class RenameInductionVariable : public Nodecl::ExhaustiveVisitor<void>
        {
            TL::Symbol _ori_induction_var, _new_induction_var;
            bool _replacing_mode;
            public:
            RenameInductionVariable(
                    TL::Symbol ori_induction_var,
                    TL::Symbol new_induction_var) :
                _ori_induction_var(ori_induction_var),
                _new_induction_var(new_induction_var),
                _replacing_mode(false) {}

            virtual void visit(const Nodecl::OpenMP::DepIn& n)
            {
                _replacing_mode = true;
                Nodecl::ExhaustiveVisitor<void>::visit(n);
                _replacing_mode = false;
            }

            virtual void visit(const Nodecl::OpenMP::DepOut& n)
            {
                _replacing_mode = true;
                Nodecl::ExhaustiveVisitor<void>::visit(n);
                _replacing_mode = false;
            }

            virtual void visit(const Nodecl::OpenMP::DepInout& n)
            {
                _replacing_mode = true;
                Nodecl::ExhaustiveVisitor<void>::visit(n);
                _replacing_mode = false;
            }

            virtual void visit(const Nodecl::OmpSs::Concurrent& n)
            {
                _replacing_mode = true;
                Nodecl::ExhaustiveVisitor<void>::visit(n);
                _replacing_mode = false;
            }

            virtual void visit(const Nodecl::OmpSs::Commutative& n)
            {
                _replacing_mode = true;
                Nodecl::ExhaustiveVisitor<void>::visit(n);
                _replacing_mode = false;
            }

            virtual void visit(const Nodecl::OpenMP::If& n)
            {
                _replacing_mode = true;
                Nodecl::ExhaustiveVisitor<void>::visit(n);
                _replacing_mode = false;
            }

            virtual void visit(const Nodecl::OpenMP::Final& n)
            {
                _replacing_mode = true;
                Nodecl::ExhaustiveVisitor<void>::visit(n);
                _replacing_mode = false;
            }

            virtual void visit(const Nodecl::OpenMP::Priority& n)
            {
                _replacing_mode = true;
                Nodecl::ExhaustiveVisitor<void>::visit(n);
                _replacing_mode = false;
            }

            virtual void visit(const Nodecl::OpenMP::Firstprivate& n)
            {
                _replacing_mode = true;
                Nodecl::ExhaustiveVisitor<void>::visit(n);
                _replacing_mode = false;
            }

            virtual void visit(const Nodecl::Symbol& n)
            {
                if (!_replacing_mode)
                    return;

                if (n.get_symbol() == _ori_induction_var)
                {
                    // Kludge
                    const_cast<Nodecl::Symbol&>(n).set_symbol(_new_induction_var);
                }
            }

        };

        RenameInductionVariable w(ori_induction_var, new_induction_var);
        w.walk(execution_environment);
    }

    void Base::handle_task_if_clause(
            const TL::PragmaCustomStatement& directive,
            Nodecl::NodeclBase parsing_context,
            Nodecl::List& execution_environment)
    {
        PragmaCustomLine pragma_line = directive.get_pragma_line();
        PragmaCustomClause if_clause = pragma_line.get_clause("if");
        ObjectList<Nodecl::NodeclBase> expr_list = if_clause.get_arguments_as_expressions(parsing_context);
        if (if_clause.is_defined()
                && expr_list.size() == 1)
        {
            execution_environment.append(Nodecl::OpenMP::If::make(expr_list[0].shallow_copy()));

            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This task will be deferred only if expression '"
                    << expr_list[0].prettyprint() << "'\n"
                    // << OpenMP::Report::indent
                    // << OpenMP::Report::indent
                    // << "Note that this does not affect dependences: if "
                    // "the task is not deferred the current thread will block until they are satisfied\n"
                    // << OpenMP::Report::indent
                    // << "Note also that there is some unavoidable overhead "
                    // "caused by the required bookkeeping of the task context, even if the task is not deferred\n"
                    ;
            }
        }
        else
        {
            if (if_clause.is_defined())
            {
                error_printf_at(directive.get_locus(), "ignoring invalid 'if' clause\n");
            }

            // if (emit_omp_report())
            // {
            //     *_omp_report_file
            //         << OpenMP::Report::indent
            //         "This task may run deferred because it does not have any 'if' clause\n"
            //         // << OpenMP::Report::indent
            //         // << "Note that the runtime may still choose not to run the task deferredly by a number of reasons\n"
            //         ;
            // }
        }
    }

    void Base::handle_task_final_clause(
            const TL::PragmaCustomStatement& directive,
            Nodecl::NodeclBase parsing_context,
            Nodecl::List& execution_environment)
    {
        PragmaCustomLine pragma_line = directive.get_pragma_line();
        PragmaCustomClause final_clause = pragma_line.get_clause("final");
        ObjectList<Nodecl::NodeclBase> expr_list = final_clause.get_arguments_as_expressions(parsing_context);
        if (final_clause.is_defined()
                && expr_list.size() == 1)
        {
            execution_environment.append(Nodecl::OpenMP::Final::make(expr_list[0].shallow_copy()));

            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "When this task is executed it will be final "
                    "if the expression '" << expr_list[0].prettyprint() << "' holds\n"
                    // << OpenMP::Report::indent
                    // << OpenMP::Report::indent
                    // << "A final task does not create any deferred task when it is executed\n"
                    ;
            }
        }
        else
        {
            if (final_clause.is_defined())
            {
                error_printf_at(directive.get_locus(), "ignoring invalid 'final' clause\n");
            }
        }
    }

    void Base::handle_task_priority_clause(
            const TL::PragmaCustomStatement& directive,
            Nodecl::NodeclBase parsing_context,
            Nodecl::List& execution_environment)
    {
        PragmaCustomLine pragma_line = directive.get_pragma_line();
        PragmaCustomClause priority = pragma_line.get_clause("priority");
        TL::ObjectList<Nodecl::NodeclBase> expr_list = priority.get_arguments_as_expressions(parsing_context);

        if (priority.is_defined()
                && expr_list.size() == 1)
        {
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This task generating construct has priority '" << expr_list[0].prettyprint() << "'\n";
            }
            execution_environment.append(
                    Nodecl::OpenMP::Priority::make(
                        expr_list[0],
                        directive.get_locus()));
        }
        else
        {
            if (priority.is_defined())
            {
                warn_printf_at(directive.get_locus(), "ignoring invalid 'priority' clause\n");
            }
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "No priority was defined\n";
            }
        }
    }

    void Base::handle_label_clause(
            const TL::PragmaCustomStatement& directive,
            Nodecl::List& execution_environment)
    {
        PragmaCustomLine pragma_line = directive.get_pragma_line();
        PragmaCustomClause label_clause = pragma_line.get_clause("label");
        TL::ObjectList<std::string> str_list = label_clause.get_tokenized_arguments();
        if (label_clause.is_defined()
                && str_list.size() == 1)
        {
            execution_environment.append(
                    Nodecl::OmpSs::TaskLabel::make(
                        str_list[0],
                        directive.get_locus()));

            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "Parallel construct labeled '" << str_list[0] << "'\n";
            }
        }
        else
        {
            if (label_clause.is_defined())
            {
                warn_printf_at(directive.get_locus(), "ignoring invalid 'label' clause in 'parallel' construct\n");
            }
            if (emit_omp_report())
            {
                *_omp_report_file
                    << OpenMP::Report::indent
                    << "This parallel construct does not have any label\n";
            }
        }
    }


} }

EXPORT_PHASE(TL::OpenMP::Base)
