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

#ifndef TL_VECTORIZER_HPP
#define TL_VECTORIZER_HPP

#include <string>
#include <list>

#include "tl-nodecl-base.hpp"

#include "tl-vectorization-analysis-interface.hpp"
#include "tl-function-versioning.hpp"
#include "tl-vectorizer-prefetcher.hpp"
#include "tl-vectorization-common.hpp"


namespace TL
{
    namespace Vectorization
    {
        class Vectorizer
        {
            public:

            //private:
                static VectorizationAnalysisInterface* _vectorizer_analysis;
                static Vectorizer* _vectorizer;
                static FunctionVersioning _function_versioning;
                static bool _gathers_scatters_disabled;
                static TL::Symbol _analysis_func;

                bool _svml_sse_enabled;
                bool _svml_avx2_enabled;
                bool _svml_knc_enabled;
                bool _svml_knl_enabled;
                bool _fast_math_enabled;
                
                void enable_svml_common_avx512(std::string device);

                Vectorizer();

            //public:
                static Vectorizer& get_vectorizer();
                static void initialize_analysis(
                        const Nodecl::NodeclBase& function_code);
                static void finalize_analysis();

                ~Vectorizer();

                void preprocess_code(const Nodecl::NodeclBase& n);
                void postprocess_code(const Nodecl::NodeclBase& n);

                void vectorize_loop(Nodecl::NodeclBase& loop_statement,
                        VectorizerEnvironment& environment);
                void vectorize_function_header(Nodecl::FunctionCode& func_code,
                        VectorizerEnvironment& environment,
                        const bool masked_version);
                void vectorize_function(Nodecl::FunctionCode& func_code,
                        VectorizerEnvironment& environment,
                        const bool masked_version);
                void vectorize_parallel(Nodecl::NodeclBase& statements,
                        VectorizerEnvironment& environment);
                void opt_overlapped_accesses(Nodecl::NodeclBase& statements,
                        VectorizerEnvironment& environment,
                        const bool is_simd_for,
                        const bool is_epilog,
                        const bool overlap_in_place,
                        Nodecl::List& init_stmts);
                void prefetcher(const Nodecl::NodeclBase& statements,
                        const prefetch_info_t& pref_info,
                        const VectorizerEnvironment& environment);

                void process_epilog(Nodecl::NodeclBase& loop_statement,
                        VectorizerEnvironment& environment,
                        Nodecl::NodeclBase& net_epilog_node,
                        int epilog_iterations,
                        bool only_epilog,
                        bool is_parallel_loop);
                void clean_up_epilog(Nodecl::NodeclBase& net_epilog_node,
                        VectorizerEnvironment& environment,
                        int epilog_iterations,
                        bool only_epilog,
                        bool is_parallel_loop);
                int get_epilog_info(const Nodecl::NodeclBase& loop_statement,
                        VectorizerEnvironment& environment,
                        bool& only_epilog);

                bool is_supported_reduction(bool is_builtin,
                        const std::string& reduction_name,
                        const TL::Type& reduction_type,
                        const VectorizerEnvironment& environment);
                void vectorize_reduction(const TL::Symbol& scalar_symbol,
                        TL::Symbol& vector_symbol,
                        const Nodecl::NodeclBase& initializer,
                        const std::string& reduction_name,
                        const TL::Type& reduction_type,
                        const VectorizerEnvironment& environment,
                        Nodecl::List& pre_nodecls,
                        Nodecl::List& post_nodecls);

                void add_vector_function_version(TL::Symbol symbol,
                        const Nodecl::NodeclBase& func_version, const std::string& device,
                        const unsigned int vector_length, const TL::Type& target_type,
                        const bool masked, const FunctionPriority priority,
                        bool const is_svml_function);
                bool is_svml_function(TL::Symbol symbol,
                        const std::string& device,
                        const unsigned int vector_length,
                        const TL::Type& target_type,
                        const bool masked) const;

                void enable_svml_sse();
                void enable_svml_avx2();
                void enable_svml_knc();
                void enable_svml_knl();
                void enable_fast_math();
                void disable_gathers_scatters();
        };
   }
}

#endif // TL_VECTORIZER_HPP
