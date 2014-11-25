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

#ifndef TL_OMP_SIMD_HPP
#define TL_OMP_SIMD_HPP

#include "tl-pragmasupport.hpp"
#include "tl-vectorizer.hpp"
#include "tl-vectorizer-overlap.hpp"

namespace TL
{
    namespace OpenMP
    {
        //! This class transforms
        class Simd : public TL::PragmaCustomCompilerPhase
        {
            public:
                Simd();

                virtual void run(TL::DTO& dto);
                virtual void pre_run(TL::DTO& dto);

                virtual ~Simd() { }

            private:
                std::string _simd_enabled_str;
                std::string _svml_enabled_str;
                std::string _fast_math_enabled_str;
                std::string _avx2_enabled_str;
                std::string _knc_enabled_str;
                std::string _spml_enabled_str;
                std::string _only_adjacent_accesses_str;

                bool _simd_enabled;
                bool _svml_enabled;
                bool _fast_math_enabled;
                bool _avx2_enabled;
                bool _knc_enabled;
                bool _spml_enabled;
                bool _only_adjacent_accesses_enabled;

                void set_simd(const std::string simd_enabled_str);
                void set_svml(const std::string svml_enabled_str);
                void set_fast_math(const std::string fast_math_enabled_str);
                void set_avx2(const std::string avx2_enabled_str);
                void set_knc(const std::string knc_enabled_str);
                void set_spml(const std::string spml_enabled_str);
                void set_only_adjcent_accesses(
                        const std::string only_adjacent_accesses_str);
        };

        class SimdVisitor : public Nodecl::ExhaustiveVisitor<void>
        {
            protected:
                TL::Vectorization::Vectorizer& _vectorizer;

                std::string _device_name;
                unsigned int _vector_length;
                bool _support_masking;
                unsigned int _mask_size;
                bool _fast_math_enabled;

                void process_aligned_clause(const Nodecl::List& environment,
                        TL::Vectorization::map_tlsym_int_t& aligned_expressions_map);
                void process_linear_clause(const Nodecl::List& environment,
                        TL::Vectorization::map_tlsym_int_t& linear_symbols_map);
                void process_uniform_clause(const Nodecl::List& environment,
                        TL::Vectorization::objlist_tlsym_t& uniform_symbols);
                void process_suitable_clause(const Nodecl::List& environment,
                        TL::Vectorization::objlist_nodecl_t& suitable_expressions);
                void process_nontemporal_clause(const Nodecl::List& environment,
                        TL::Vectorization::map_tlsym_objlist_t& nontemporal_expressions);
                int process_unroll_clause(const Nodecl::List& environment);
                int process_unroll_and_jam_clause(const Nodecl::List& environment);
                void process_vectorlengthfor_clause(const Nodecl::List& environment,
                        TL::Type& vectorlengthfor_type);
                void process_overlap_clause(const Nodecl::List& environment,
                        TL::Vectorization::map_tlsym_objlist_int_t& overlap_expressions);

                Nodecl::List process_reduction_clause(const Nodecl::List& environment,
                        TL::ObjectList<TL::Symbol>& reductions,
                        std::map<TL::Symbol, TL::Symbol>& new_external_vector_symbol_map,
                        TL::Scope enclosing_scope);

                void common_simd_function(
                        const Nodecl::OpenMP::SimdFunction& simd_node,
                        const bool masked_version);

            public:
                SimdVisitor(Vectorization::SIMDInstructionSet simd_isa,
                        bool fast_math_enabled, bool svml_enabled,
                        bool only_adjacent_accesses);

                virtual void visit(const Nodecl::OpenMP::Simd& simd_node);
                virtual void visit(const Nodecl::OpenMP::SimdFor& simd_node);
                virtual void visit(const Nodecl::OpenMP::SimdFunction& simd_node);
        };

        class SimdSPMLVisitor : public SimdVisitor
        {
            public:
                SimdSPMLVisitor(Vectorization::SIMDInstructionSet simd_isa,
                        bool fast_math_enabled, bool svml_enabled,
                        bool only_adjacent_accesses);

                using SimdVisitor::visit;
                virtual void visit(const Nodecl::OpenMP::SimdParallel& simd_node);
        };



        class FunctionDeepCopyFixVisitor : public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                const TL::Symbol& _orig_symbol;
                const TL::Symbol& _new_symbol;

            public:
                FunctionDeepCopyFixVisitor(const TL::Symbol& orig_symbol,
                        const TL::Symbol& new_symbol);

                virtual void visit(const Nodecl::Symbol& n);
        };
    }
}

#endif // TL_OMP_SIMD_HPP
