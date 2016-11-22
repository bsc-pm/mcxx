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

#ifndef TL_OMP_BASE_HPP
#define TL_OMP_BASE_HPP

#include "tl-pragmasupport.hpp"
#include "tl-omp-core.hpp"
#include <fstream>
#include <iterator>
#include <algorithm>

namespace TL
{
    namespace OpenMP
    {
        //! This class transforms OpenMP pragmas to the Nodecl representation of parallelism
        class Base : public TL::PragmaCustomCompilerPhase
        {
            public:
                Base();

                virtual void run(TL::DTO& dto);
                virtual void pre_run(TL::DTO& dto);

                virtual void phase_cleanup(TL::DTO& dto);
                virtual void phase_cleanup_end_of_pipeline(TL::DTO& dto);

                virtual ~Base() { }

            private:
                OpenMP::Core _core;

                std::shared_ptr<TL::OmpSs::FunctionTaskSet> _function_task_set;

                std::string _openmp_dry_run;

                std::string _simd_enabled_str;
                bool _simd_enabled;
                void set_simd(const std::string &simd_enabled_str);

                std::string _ompss_mode_str;
                bool _ompss_mode;
                void set_ompss_mode(const std::string &str);
                bool in_ompss_mode() const;

                std::ofstream* _omp_report_file;
                std::string _omp_report_str;
                bool _omp_report;
                void set_omp_report_parameter(const std::string &str);

                std::string _copy_deps_str;
                bool _copy_deps_by_default;
                void set_copy_deps_by_default(const std::string& str);
                bool copy_deps_by_default() const;

                std::string _untied_tasks_by_default_str;
                bool _untied_tasks_by_default;
                void set_untied_tasks_by_default(const std::string& str);
                bool untied_tasks_by_default() const;

                std::string _allow_shared_without_copies_str;
                void set_allow_shared_without_copies(const std::string &allow_shared_without_copies_str);

                std::string _discard_unused_data_sharings_str;
                void set_discard_unused_data_sharings(const std::string &discard_unused_data_sharings);

                std::string _allow_array_reductions_str;
                void set_allow_array_reductions(const std::string& allow_array_reductions);

                std::string _disable_task_expr_optim_str;

                std::string _enable_input_by_value_dependences;
                void set_enable_input_by_value_dependences(const std::string &enable_input_by_value);

                std::string _enable_nonvoid_function_tasks;
                void set_enable_nonvoid_function_tasks(const std::string &enable_nonvoid_function_tasks);

                // Handler functions
#define OMP_DIRECTIVE(_directive, _name, _pred) \
                void _name##_handler_pre(TL::PragmaCustomDirective); \
                void _name##_handler_post(TL::PragmaCustomDirective);
#define OMP_CONSTRUCT(_directive, _name, _pred) \
                void _name##_handler_pre(TL::PragmaCustomStatement); \
                void _name##_handler_post(TL::PragmaCustomStatement); \
                void _name##_handler_pre(TL::PragmaCustomDeclaration); \
                void _name##_handler_post(TL::PragmaCustomDeclaration);
#define OMP_CONSTRUCT_NOEND(_directive, _name, _pred) \
                OMP_CONSTRUCT(_directive, _name, _pred)
#include "tl-omp-constructs.def"
#undef OMP_CONSTRUCT
#undef OMP_CONSTRUCT_NOEND
#undef OMP_DIRECTIVE

                void ompss_target_handler_pre(TL::PragmaCustomStatement stmt);
                void ompss_target_handler_post(TL::PragmaCustomStatement stmt);

                void ompss_target_handler_pre(TL::PragmaCustomDeclaration decl);
                void ompss_target_handler_post(TL::PragmaCustomDeclaration decl);

                void omp_target_handler_pre(TL::PragmaCustomStatement stmt);
                void omp_target_handler_post(TL::PragmaCustomStatement stmt);

                void omp_target_handler_pre(TL::PragmaCustomDeclaration decl);
                void omp_target_handler_post(TL::PragmaCustomDeclaration decl);

                Nodecl::List make_execution_environment(
                        OpenMP::DataEnvironment&,
                        PragmaCustomLine,
                        bool ignore_targer_info,
                        bool is_inline_task);

                Nodecl::List make_execution_environment_for_combined_worksharings(OpenMP::DataEnvironment &data_sharing_env, 
                        PragmaCustomLine pragma_line);

                Nodecl::NodeclBase loop_handler_post(
                        TL::PragmaCustomStatement directive,
                        Nodecl::NodeclBase statement,
                        bool barrier_at_end,
                        bool is_combined_with_parallel);

                Nodecl::NodeclBase sections_handler_common(
                        TL::PragmaCustomStatement directive,
                        Nodecl::NodeclBase statement,
                        bool barrier_at_end,
                        bool is_combined_with_parallel);

                template <typename openmp_node>
                void process_symbol_list_colon_int_clause(
                        const TL::PragmaCustomLine& pragma_line,
                        const std::string& pragma_name,
                        const Nodecl::NodeclBase& ref_scope,
                        Nodecl::List& environment,
                        const int default_int);
                template <typename openmp_node>
                void process_symbol_list_clause(
                        const TL::PragmaCustomLine& pragma_line,
                        const std::string& pragma_name,
                        const Nodecl::NodeclBase& ref_scope,
                        Nodecl::List& environment);

                void process_common_simd_clauses(
                        const TL::PragmaCustomLine& pragma_line,
                        const Nodecl::NodeclBase& ref_scope,
                        Nodecl::List& environment);

                void nest_context_in_pragma(TL::PragmaCustomStatement directive);

                void taskloop_block_loop(
                        Nodecl::NodeclBase directive,
                        Nodecl::NodeclBase statement,
                        Nodecl::NodeclBase execution_environment,
                        Nodecl::NodeclBase grainsize_expr,
                        Nodecl::NodeclBase num_tasks_expr);

                void taskloop_update_environment_renaming_induction_variable(
                        Nodecl::NodeclBase execution_environment,
                        TL::Symbol ori_induction_var,
                        TL::Symbol new_induction_var);

                // void taskloop_extend_dependences(
                //         Nodecl::NodeclBase execution_environment,
                //         TL::Symbol new_induction_var,
                //         TL::Symbol block_extent_var);

                void handle_task_if_clause(
                        const TL::PragmaCustomStatement& directive,
                        Nodecl::NodeclBase parsing_context,
                        Nodecl::List& execution_environment);

                void handle_task_final_clause(
                        const TL::PragmaCustomStatement& directive,
                        Nodecl::NodeclBase parsing_context,
                        Nodecl::List& execution_environment);

                void handle_task_priority_clause(
                        const TL::PragmaCustomStatement& directive,
                        Nodecl::NodeclBase parsing_context,
                        Nodecl::List& execution_environment);

                void handle_label_clause(
                        const TL::PragmaCustomStatement& directive,
                        Nodecl::List& execution_environment);

                void register_omp();
                void register_ompss();

#ifndef VECTORIZATION_DISABLED
                void register_simd_function(
                        OpenMP::DataEnvironment& ds,
                        TL::Symbol sym,
                        Nodecl::NodeclBase context_of_parameters,
                        TL::PragmaCustomLine pragma_line,
                        const locus_t* locus);
#endif

            public:
                template <typename T>
                void make_data_sharing_list(
                        OpenMP::DataEnvironment &data_sharing_env,
                        OpenMP::DataSharingAttribute data_attr,
                        const locus_t* locus,
                        ObjectList<Nodecl::NodeclBase>& result_list);

                template <typename T, typename List>
                    void make_dependency_list(
                            List& dependences,
                            DependencyDirection kind,
                            const locus_t* locus,
                            ObjectList<Nodecl::NodeclBase>& result_list);

                template <typename T, typename List>
                    void make_copy_list(
                            List& dependences,
                            TL::OmpSs::CopyDirection kind,
                            const locus_t* locus,
                            ObjectList<Nodecl::NodeclBase>& result_list);

                void make_execution_environment_target_information(
                        TL::OmpSs::TargetInfo &target_info,
                        TL::Symbol sym,
                        const locus_t* locus,
                        // out
                        TL::ObjectList<Nodecl::NodeclBase> &result_list);

                bool emit_omp_report() const;
                std::ofstream* get_omp_report_file() const
                {
                    return _omp_report_file;
                }

                void set_omp_report(bool b)
                {
                    _omp_report = b;
                }

                Nodecl::NodeclBase _start_declare_target;
        };

        namespace Report
        {
            extern const char indent[];
        };
    }
}

#endif // TL_OMP_BASE_HPP
