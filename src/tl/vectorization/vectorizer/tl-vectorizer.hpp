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
#include "tl-vectorizer-prefetcher.hpp"
#include "tl-vectorization-common.hpp"

#include "tl-function-versioning.hpp"


namespace TL
{
    namespace Vectorization
    {
        struct register_functions_info
        {
            const char* scalar_function;
            const char* vector_function;
            TL::Type return_type;
            bool masked;
        };

        class Vectorizer
        {
            public:

            //private:
                static VectorizationAnalysisInterface* _vectorizer_analysis;
                static Vectorizer* _vectorizer;
                static bool _gathers_scatters_disabled;
                static bool _unaligned_accesses_disabled;
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
                void vectorize_function_header(
                        Nodecl::FunctionCode& function_code,
                        VectorizerEnvironment& environment,
                        const TL::ObjectList<TL::Symbol> &uniform_symbols,
                        const std::map<TL::Symbol, int> &linear_symbols,
                        const bool masked_version);
                void vectorize_function(Nodecl::FunctionCode& func_code,
                        VectorizerEnvironment& environment,
                        const bool masked_version);
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

                void register_svml_functions(const register_functions_info* functions,
                        std::string device,
                        int vec_factor,
                        const TL::Scope& scope,
                        const std::string& vtype_str);

                void enable_svml_sse();
                void enable_svml_avx2();
                void enable_svml_knc();
                void enable_svml_knl();
                void enable_fast_math();
                void disable_gathers_scatters();
                void disable_unaligned_accesses();
        };
   }
}

#endif // TL_VECTORIZER_HPP
