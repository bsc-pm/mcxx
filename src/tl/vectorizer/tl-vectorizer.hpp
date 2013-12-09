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

#ifndef TL_VECTORIZER_HPP
#define TL_VECTORIZER_HPP

#include <string>
#include <list>
#include "tl-analysis-static-info.hpp"
#include "tl-nodecl-base.hpp"
#include "tl-function-versioning.hpp"
#include "tl-vectorizer-utils.hpp"
#include "tl-vectorizer-cache.hpp"

namespace TL 
{ 
    namespace Vectorization
    {
        class VectorizerEnvironment
        {
            private:
                const std::string& _device;
                const unsigned int _vector_length;
                const unsigned int _unroll_factor;
                const bool _support_masking;
                const unsigned int _mask_size;
                const bool _fast_math;
                const bool _prefer_gather_scatter;
                const bool _prefer_mask_gather_scatter;
                const TL::Type& _target_type;
                const TL::ObjectList<Nodecl::NodeclBase>* _suitable_expr_list;
                const VectorizerCache& _vectorizer_cache;

                const TL::ObjectList<TL::Symbol>* _reduction_list;
                std::map<TL::Symbol, TL::Symbol>* _new_external_vector_symbol_map;

                TL::Scope _external_scope;                      // Enclosing scope of the SIMD region (to add reduction symbols)
                std::list<TL::Scope> _local_scope_list;         // TL::Scopes used to determine if a variable is locally declared
                std::list<Nodecl::NodeclBase> _analysis_scopes; // Stack of useful scopes (If, FunctionCode and For) for the analysis
                Nodecl::NodeclBase _analysis_simd_scope;        // SIMD scope

                std::list<Nodecl::NodeclBase> _mask_list;       // Stack of masks
                std::list<bool> _inside_inner_masked_bb;        // TBD :)
                std::list<unsigned int> _mask_check_bb_cost;    // Costs of BB for early exist heuristic

                TL::Symbol _function_return;                    // Return symbol when return statement are present in masked code

           public:
                VectorizerEnvironment(const std::string& device,
                        const unsigned int vector_length,
                        const bool support_masking,
                        const unsigned int mask_size,
                        const bool fast_math,
                        const bool prefer_gather_scatter,
                        const bool prefer_mask_gather_scatter,
                        const TL::Type& target_type,
                        const TL::ObjectList<Nodecl::NodeclBase>* suitable_expr_list,
                        const TL::ObjectList<TL::Symbol>* reduction_list,
                        const VectorizerCache& vectorizer_cache,
                        std::map<TL::Symbol, TL::Symbol>* new_external_vector_symbol_map);

                ~VectorizerEnvironment();

                friend class Vectorizer;
                friend class VectorizerVisitorFor;
                friend class VectorizerVisitorForEpilog;
                friend class VectorizerVisitorLoopHeader;
                friend class VectorizerVisitorLoopCond;
                friend class VectorizerVisitorLoopNext;
                friend class VectorizerVisitorFunction;
                friend class VectorizerVisitorStatement;
                friend class VectorizerVisitorExpression;
                friend class VectorizerVectorReduction;

                friend bool Vectorization::Utils::is_nested_induction_variable_dependent_access(
                        const VectorizerEnvironment& environment,
                        const Nodecl::NodeclBase& n);
                friend bool Vectorization::Utils::is_nested_non_reduction_basic_induction_variable(
                        const VectorizerEnvironment& environment,
                        const Nodecl::NodeclBase& n);
        };

        class Vectorizer
        {
            private:
                static Vectorizer* _vectorizer;
                static FunctionVersioning _function_versioning;

                static Analysis::AnalysisStaticInfo *_analysis_info;

                bool _svml_sse_enabled;
                bool _svml_knc_enabled;
                bool _fast_math_enabled;

                Vectorizer();

            public:
                static Vectorizer& get_vectorizer();
                static void initialize_analysis(const Nodecl::FunctionCode& enclosing_function);
                static void finalize_analysis();

                ~Vectorizer();

                void vectorize(Nodecl::ForStatement& for_statement, 
                        VectorizerEnvironment& environment);
                void vectorize(Nodecl::FunctionCode& func_code,
                        VectorizerEnvironment& environment,
                        const bool masked_version);
 
                void process_epilog(Nodecl::ForStatement& for_statement, 
                        VectorizerEnvironment& environment,
                        Nodecl::NodeclBase& net_epilog_node,
                        int epilog_iterations,
                        bool only_epilog,
                        bool is_parallel_loop);
                int get_epilog_info(const Nodecl::ForStatement& for_statement,
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

                void add_vector_function_version(const std::string& func_name, 
                        const Nodecl::NodeclBase& func_version, const std::string& device, 
                        const unsigned int vector_length, const TL::Type& target_type, 
                        const bool masked, const FunctionPriority priority,
                        bool const is_svml_function);
                bool is_svml_function(const std::string& func_name, 
                        const std::string& device, 
                        const unsigned int vector_length, 
                        const TL::Type& target_type, 
                        const bool masked) const;

                void enable_svml_sse();
                void enable_svml_knc();
                void enable_fast_math();

                friend class VectorizerVisitorFor;
                friend class VectorizerVisitorForEpilog;
                friend class VectorizerVisitorLoopCond;
                friend class VectorizerVisitorLoopNext;
                friend class VectorizerVisitorFunction;
                friend class VectorizerVisitorStatement;
                friend class VectorizerVisitorExpression;
                friend class VectorizerVisitorLoopHeader;

                friend bool Vectorization::Utils::is_nested_induction_variable_dependent_access(
                        const VectorizerEnvironment& environment,
                        const Nodecl::NodeclBase& n);
                friend bool Vectorization::Utils::is_nested_non_reduction_basic_induction_variable(
                        const VectorizerEnvironment& environment,
                        const Nodecl::NodeclBase& n);
        };
   }
}

#endif // TL_VECTORIZER_HPP
