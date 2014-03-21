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

#ifndef TL_OMP_BASE_HPP
#define TL_OMP_BASE_HPP

#include "tl-pragmasupport.hpp"
#include "tl-omp-core.hpp"

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

                RefPtr<OpenMP::FunctionTaskSet> _function_task_set;

                std::string _openmp_dry_run;

                std::string _simd_enabled_str;
                bool _simd_enabled;
                void set_simd(const std::string &simd_enabled_str);

                std::string _ompss_mode_str;
                bool _ompss_mode;
                void set_ompss_mode(const std::string &str);
                bool in_ompss_mode() const;

                std::string _copy_deps_str;
                bool _copy_deps_by_default;
                void set_copy_deps_by_default(const std::string& str);
                bool copy_deps_by_default() const;

                std::string _allow_shared_without_copies_str;
                void set_allow_shared_without_copies(const std::string &allow_shared_without_copies_str);

                std::string _discard_unused_data_sharings_str;
                void set_discard_unused_data_sharings(const std::string &discard_unused_data_sharings);

                std::string _allow_array_reductions_str;
                void set_allow_array_reductions(const std::string& allow_array_reductions);

                std::string _disable_task_expr_optim_str;

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

                Nodecl::List make_execution_environment(OpenMP::DataSharingEnvironment&, PragmaCustomLine);

                Nodecl::List make_execution_environment_for_combined_worksharings(OpenMP::DataSharingEnvironment &data_sharing_env, 
                        PragmaCustomLine pragma_line);

                Nodecl::NodeclBase loop_handler_post(
                        TL::PragmaCustomStatement directive, 
                        Nodecl::NodeclBase statement,
                        bool barrier_at_end,
                        bool is_combined_worksharing);

                Nodecl::NodeclBase sections_handler_common(
                        TL::PragmaCustomStatement directive,
                        Nodecl::NodeclBase statement,
                        bool barrier_at_end,
                        bool is_combined_worksharing);

                void process_common_simd_clauses(
                        const Nodecl::NodeclBase& stmt,
                        const TL::PragmaCustomLine& pragma_line,
                        Nodecl::List& environment);

        };
    }
}

#endif // TL_OMP_BASE_HPP
