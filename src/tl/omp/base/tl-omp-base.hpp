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

                std::ofstream* _omp_report_file;
                std::string _omp_report_str;
                bool _omp_report;
                void set_omp_report_parameter(const std::string &str);

                std::string _disable_task_expr_optim_str;

                std::string _taskloop_as_loop_of_tasks_str;
                bool _taskloop_as_loop_of_tasks;
                void set_taskloop_as_loop_of_tasks(const std::string &str);

                // Strings used to store the TL::Core phase flags
                std::string _ompss_mode_str;
                std::string _copy_deps_str;
                std::string _untied_tasks_by_default_str;
                std::string _allow_shared_without_copies_str;
                std::string _enable_input_by_value_dependences;
                std::string _enable_nonvoid_function_tasks;
                std::string _allow_array_reductions_str;
                std::string _discard_unused_data_sharings_str;


                // Handler functions
#define DECL_DIRECTIVE(_directive, _name, _pred, _func_prefix) \
                void _func_prefix##_name##_handler_pre(TL::PragmaCustomDirective); \
                void _func_prefix##_name##_handler_post(TL::PragmaCustomDirective);

#define DECL_CONSTRUCT(_directive, _name, _pred, _func_prefix) \
                void _func_prefix##_name##_handler_pre(TL::PragmaCustomStatement); \
                void _func_prefix##_name##_handler_post(TL::PragmaCustomStatement); \
                void _func_prefix##_name##_handler_pre(TL::PragmaCustomDeclaration); \
                void _func_prefix##_name##_handler_post(TL::PragmaCustomDeclaration);

#define OMP_DIRECTIVE(_directive, _name, _pred) DECL_DIRECTIVE(_directive, _name, _pred, /*empty_prefix*/ )
#define OMP_CONSTRUCT(_directive, _name, _pred) DECL_CONSTRUCT(_directive, _name, _pred, /*empty_prefix*/)
#define OMP_CONSTRUCT_NOEND(_directive, _name, _pred) OMP_CONSTRUCT(_directive, _name, _pred)
#include "tl-omp-constructs.def"
#undef OMP_CONSTRUCT
#undef OMP_CONSTRUCT_NOEND
#undef OMP_DIRECTIVE

#define OSS_DIRECTIVE(_directive, _name, _pred) DECL_DIRECTIVE(_directive, _name, _pred, oss_)
#define OSS_CONSTRUCT(_directive, _name, _pred) DECL_CONSTRUCT(_directive, _name, _pred, oss_)
#define OSS_CONSTRUCT_NOEND(_directive, _name, _pred) OSS_CONSTRUCT(_directive, _name, _pred)
#include "tl-oss-constructs.def"
#undef OSS_CONSTRUCT
#undef OSS_CONSTRUCT_NOEND
#undef OSS_DIRECTIVE

#undef DECL_DIRECTIVE
#undef DECL_CONSTRUCT

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
                        bool ignore_targer_info);

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

                template < typename T>
                void handle_generic_clause_with_one_argument(
                        const std::string &clause_name,
                        const std::string &omp_report_message,
                        const TL::PragmaCustomStatement& directive,
                        Nodecl::NodeclBase parsing_context,
                        Nodecl::List& execution_environment);

                void handle_label_clause(
                        const TL::PragmaCustomStatement& directive,
                        Nodecl::List& execution_environment);

                void bind_omp_constructs();
                void bind_oss_constructs();

                //! This function is called before executing the OpenMP::Core phase.
                //! It applies the OpenMP high level transformations, such as the collapse clause
                void apply_openmp_high_level_transformations(Nodecl::NodeclBase translation_unit);

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

                template <typename T>
                void make_data_sharing_list(
                        OpenMP::DataEnvironment &data_sharing_env,
                        OpenMP::DataSharingAttribute data_attr,
                        const std::function<bool(const DataEnvironment::DataSharingInfoPair&)>& filter_fun,
                        const locus_t* locus,
                        ObjectList<Nodecl::NodeclBase>& result_list);

                template <typename T, typename ItemDirection, typename List>
                    void make_item_list(
                            List& dependences,
                            ItemDirection kind,
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
