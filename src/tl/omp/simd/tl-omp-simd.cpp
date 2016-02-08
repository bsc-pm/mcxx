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

#include "tl-omp-simd.hpp"
#include "tl-omp-simd-visitor.hpp"

#include "tl-vectorization-common.hpp"

using namespace TL::Vectorization;

namespace TL {
    namespace OpenMP {

        Simd::Simd()
            : PragmaCustomCompilerPhase(),
            _simd_enabled(false),
            _svml_enabled(false),
            _fast_math_enabled(false),
            _avx2_enabled(false),
            _neon_enabled(false),
            _romol_enabled(false),
            _knc_enabled(false),
            _knl_enabled(false),
            _only_adjacent_accesses_enabled(false),
            _only_aligned_accesses_enabled(false),
            _overlap_in_place(false)
        {
            set_phase_name("Vectorize OpenMP SIMD parallel IR");
            set_phase_description("This phase vectorize the OpenMP SIMD parallel IR");

            register_parameter("simd_enabled",
                    "If set to '1' enables simd constructs, otherwise it is disabled",
                    _simd_enabled_str,
                    "0").connect(std::bind(&Simd::set_simd, this, std::placeholders::_1));

            register_parameter("svml_enabled",
                    "If set to '1' enables svml math library, otherwise it is disabled",
                    _svml_enabled_str,
                    "0").connect(std::bind(&Simd::set_svml, this, std::placeholders::_1));

            register_parameter("fast_math_enabled",
                    "If set to '1' enables fast_math operations, otherwise it is disabled",
                    _fast_math_enabled_str,
                    "0").connect(std::bind(&Simd::set_fast_math, this, std::placeholders::_1));

            register_parameter("mic_enabled",
                    "If set to '1' enables compilation for KNC architecture, otherwise it is disabled",
                    _knc_enabled_str,
                    "0").connect(std::bind(&Simd::set_knc, this, std::placeholders::_1));
            register_parameter("knl_enabled",
                    "If set to '1' enables compilation for KNL architecture, otherwise it is disabled",
                    _knl_enabled_str,
                    "0").connect(std::bind(&Simd::set_knl, this, std::placeholders::_1));

            register_parameter("avx2_enabled",
                    "If set to '1' enables compilation for AVX2 instruction set, otherwise it is disabled",
                    _avx2_enabled_str,
                    "0").connect(std::bind(&Simd::set_avx2, this, std::placeholders::_1));

            register_parameter("neon_enabled",
                    "If set to '1' enables compilation for NEON instruction set, otherwise it is disabled",
                    _neon_enabled_str,
                    "0").connect(std::bind(&Simd::set_neon, this, std::placeholders::_1));

            register_parameter("romol_enabled",
                    "If set to '1' enables compilation for RoMoL instruction set, otherwise it is disabled",
                    _romol_enabled_str,
                    "0").connect(std::bind(&Simd::set_romol, this, std::placeholders::_1));

            register_parameter("only_adjacent_accesses",
                    "If set to '1' disables emission of gather/scatter vector instructions",
                    _only_adjacent_accesses_str,
                    "0").connect(std::bind(&Simd::set_only_adjcent_accesses, this, std::placeholders::_1));

            register_parameter("only_aligned_accesses",
                    "If set to '1' disables emission of unaligned memory accesses",
                    _only_aligned_accesses_str,
                    "0").connect(std::bind(&Simd::set_only_aligned_accesses, this, std::placeholders::_1));

            register_parameter("overlap_in_place",
                    "Enables overlap register cache update in place and not at the beginning of the BB",
                    _overlap_in_place_str,
                    "0").connect(std::bind(&Simd::set_overlap_in_place, this, std::placeholders::_1));

        }

        void Simd::set_simd(const std::string simd_enabled_str)
        {
            parse_boolean_option("simd_enabled", simd_enabled_str, _simd_enabled, "Invalid simd_enabled value");
        }

        void Simd::set_svml(const std::string svml_enabled_str)
        {
            parse_boolean_option("svml_enabled", svml_enabled_str, _svml_enabled, "Invalid svml_enabled value");
        }

        void Simd::set_fast_math(const std::string fast_math_enabled_str)
        {
            parse_boolean_option("fast_math_enabled", fast_math_enabled_str, _fast_math_enabled, "Invalid fast_math_enabled value");
        }

        void Simd::set_knc(const std::string knc_enabled_str)
        {
            parse_boolean_option("knc_enabled", knc_enabled_str, _knc_enabled, "Invalid knc_enabled value");
        }

        void Simd::set_knl(const std::string knl_enabled_str)
        {
            parse_boolean_option("knl_enabled", knl_enabled_str, _knl_enabled, "Invalid knl_enabled value");
        }

        void Simd::set_avx2(const std::string avx2_enabled_str)
        {
            parse_boolean_option("avx2_enabled", avx2_enabled_str, _avx2_enabled, "Invalid avx2_enabled value");
        }

        void Simd::set_neon(const std::string neon_enabled_str)
        {
            parse_boolean_option("neon_enabled", neon_enabled_str, _neon_enabled, "Invalid neon_enabled value");
        }

        void Simd::set_romol(const std::string romol_enabled_str)
        {
            parse_boolean_option("romol_enabled", romol_enabled_str, _romol_enabled, "Invalid romol_enabled value");
        }

        void Simd::set_only_adjcent_accesses(
                const std::string only_adjacent_accesses_str)
        {
            parse_boolean_option("only_adjacent_accesses",
                    only_adjacent_accesses_str,
                    _only_adjacent_accesses_enabled,
                    "Invalid only_adjacent_accesses value");
        }

        void Simd::set_only_aligned_accesses(
                const std::string only_aligned_accesses_str)
        {
            parse_boolean_option("only_aligned_accesses",
                    only_aligned_accesses_str,
                    _only_aligned_accesses_enabled,
                    "Invalid only_aligned_accesses value");
        }

        void Simd::set_overlap_in_place(const std::string overlap_in_place_str)
        {
            if (overlap_in_place_str == "1")
            {
                _overlap_in_place = true;
            }
        }

        void Simd::pre_run(TL::DTO& dto)
        {
            this->PragmaCustomCompilerPhase::pre_run(dto);
        }

        void Simd::run(TL::DTO& dto)
        {
            this->PragmaCustomCompilerPhase::run(dto);

            Nodecl::NodeclBase translation_unit = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);

            if (_simd_enabled)
            {
                TL::Vectorization::VectorInstructionSet simd_isa;

                struct isa_flag_t
                {
                    bool flag;
                    const char* name;
                    Vectorization::VectorInstructionSet isa;
                } isa_flag[] =
                {
                    { _avx2_enabled, "AVX2", AVX2_ISA, },
                    { _knc_enabled,  "KNC",  KNC_ISA, },
                    { _knl_enabled,  "KNL",  KNL_ISA, },
                    { _neon_enabled, "NEON", NEON_ISA },
                    { _romol_enabled, "RoMoL", ROMOL_ISA },
                };

                simd_isa = SSE4_2_ISA; // Default ISA is SSE 4.2

                const int N = sizeof(isa_flag) / sizeof(*isa_flag);
                for (int i = 0; i < N; i++)
                {
                    if (isa_flag[i].flag)
                    {
                        simd_isa = isa_flag[i].isa;
                        for (int j = i + 1; j < N; j++)
                        {
                            if ((isa_flag[i].flag)
                                    && (isa_flag[j].flag))
                            {
                                fatal_error("SIMD: requesting '%s' and '%s' SIMD instruction sets at the same time\n",
                                        isa_flag[i].name,
                                        isa_flag[j].name);
                            }
                        }
                    }
                }

                if (_svml_enabled && _neon_enabled)
                {
                    fatal_error("SVML cannot be used with NEON\n");
                }

                if (_svml_enabled && _romol_enabled)
                {
                    fatal_error("SVML cannot be used with RoMoL\n");
                }

                SimdPreregisterVisitor simd_preregister_visitor(
                    simd_isa,
                    _fast_math_enabled,
                    _svml_enabled,
                    _only_adjacent_accesses_enabled,
                    _only_aligned_accesses_enabled,
                    _overlap_in_place);
                simd_preregister_visitor.walk(translation_unit);

                SimdVisitor simd_visitor(simd_isa,
                                         _fast_math_enabled,
                                         _svml_enabled,
                                         _only_adjacent_accesses_enabled,
                                         _only_aligned_accesses_enabled,
                                         _overlap_in_place);
                simd_visitor.walk(translation_unit);
            }
        }

        void Simd::phase_cleanup(TL::DTO& dto)
        {
        }
    }
}
EXPORT_PHASE(TL::OpenMP::Simd)
