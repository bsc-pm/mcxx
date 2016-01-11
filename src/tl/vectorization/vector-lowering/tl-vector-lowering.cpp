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

#include "tl-vector-lowering.hpp"
#include "tl-vector-legalization-sse.hpp"
#include "tl-vector-backend-sse.hpp"
#include "tl-vector-legalization-knc.hpp"
#include "tl-vector-backend-knc.hpp"
#include "tl-vector-legalization-knl.hpp"
#include "tl-vector-backend-knl.hpp"
#include "tl-vector-legalization-avx2.hpp"
#include "tl-vector-backend-avx2.hpp"
#include "tl-vector-legalization-neon.hpp"
#include "tl-vector-backend-neon.hpp"
#include "tl-vector-legalization-romol.hpp"
#include "tl-vector-backend-romol.hpp"
#include "tl-vector-romol-regalloc.hpp"
#include "tl-vectorization-three-addresses.hpp"


namespace TL
{
    namespace Vectorization
    {
        VectorLoweringPhase::VectorLoweringPhase()
            : _knl_enabled(false),
            _knc_enabled(false),
            _avx2_enabled(false),
            _neon_enabled(false),
            _romol_enabled(false),
            _prefer_gather_scatter(false),
            _prefer_mask_gather_scatter(false),
            _valib_sim_header(false)
        {
            set_phase_name("Vector Lowering Phase");
            set_phase_description("This phase lowers Vector IR to builtin calls. "
                    "By default targets SSE but AVX, AVX2, KNC, KNL, NEON and RoMoL are implemented as well");

            register_parameter("knl_enabled",
                    "If set to '1' enables compilation for KNC architecture, otherwise it is disabled",
                    _knl_enabled_str,
                    "0").connect(std::bind(&VectorLoweringPhase::set_knl, this, std::placeholders::_1));

            register_parameter("mic_enabled",
                    "If set to '1' enables compilation for KNC architecture, otherwise it is disabled",
                    _knc_enabled_str,
                    "0").connect(std::bind(&VectorLoweringPhase::set_knc, this, std::placeholders::_1));

            register_parameter("avx2_enabled",
                    "If set to '1' enables compilation for AVX2 architecture, otherwise it is disabled",
                    _avx2_enabled_str,
                    "0").connect(std::bind(&VectorLoweringPhase::set_avx2, this, std::placeholders::_1));

            register_parameter("neon_enabled",
                    "If set to '1' enables compilation for NEON architecture, otherwise it is disabled",
                    _neon_enabled_str,
                    "0").connect(std::bind(&VectorLoweringPhase::set_neon, this, std::placeholders::_1));

            register_parameter("romol_enabled",
                    "If set to '1' enables compilation for RoMoL architecture, otherwise it is disabled",
                    _romol_enabled_str,
                    "0").connect(std::bind(&VectorLoweringPhase::set_romol, this, std::placeholders::_1));

            register_parameter("prefer_mask_gather_scatter",
                    "If set to '1' enables gather/scatter generation for unaligned load/stores with masks",
                    _prefer_mask_gather_scatter_str,
                    "0").connect(std::bind(&VectorLoweringPhase::set_prefer_mask_gather_scatter, this, std::placeholders::_1));

            register_parameter("prefer_gather_scatter",
                    "If set to '1' enables gather/scatter generation for unaligned load/stores",
                    _prefer_gather_scatter_str,
                    "0").connect(std::bind(&VectorLoweringPhase::set_prefer_gather_scatter, this, std::placeholders::_1));

            register_parameter("valib_sim_header",
                    "If set to '1' prepends at the beginning of the output '#include <valib-sim.h>'",
                    _valib_sim_header_str,
                    "0").connect(std::bind(&VectorLoweringPhase::set_valib_sim_header, this, std::placeholders::_1));
        }

        void VectorLoweringPhase::set_knl(const std::string& knl_enabled_str)
        {
            parse_boolean_option("knl_enabled", knl_enabled_str, _knl_enabled, "Invalid value for knl_enabled");
        }

        void VectorLoweringPhase::set_knc(const std::string& knc_enabled_str)
        {
            parse_boolean_option("knc_enabled", knc_enabled_str, _knc_enabled, "Invalid value for knc_enabled");
        }

        void VectorLoweringPhase::set_avx2(const std::string& avx2_enabled_str)
        {
            parse_boolean_option("avx2_enabled", avx2_enabled_str, _avx2_enabled, "Invalid value for avx2_enabled");
        }

        void VectorLoweringPhase::set_neon(const std::string& neon_enabled_str)
        {
            parse_boolean_option("neon_enabled", neon_enabled_str, _neon_enabled, "Invalid value for neon_enabled");
        }

        void VectorLoweringPhase::set_romol(const std::string& romol_enabled_str)
        {
            parse_boolean_option("romol_enabled", romol_enabled_str, _romol_enabled, "Invalid value for romol_enabled");
        }

        void VectorLoweringPhase::set_prefer_gather_scatter(
                const std::string& prefer_gather_scatter_str)
        {
            if (prefer_gather_scatter_str == "1")
            {
                _prefer_gather_scatter = true;
            }
        }

        void VectorLoweringPhase::set_valib_sim_header(
                const std::string& str)
        {
            parse_boolean_option("valib_sim_header", str, _valib_sim_header, "Invalid value for valib_sim_header");
        }

        void VectorLoweringPhase::set_prefer_mask_gather_scatter(
                const std::string& prefer_mask_gather_scatter_str)
        {
            if (prefer_mask_gather_scatter_str == "1")
            {
                _prefer_mask_gather_scatter = true;
            }
        }

        void VectorLoweringPhase::run(TL::DTO& dto)
        {
            Nodecl::NodeclBase translation_unit =
                *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);

            struct backend_flag_t
            {
                bool flag;
                const char* name;
            } backend_flag[] =
            {
                { _avx2_enabled, "AVX2" },
                { _knc_enabled, "KNC" },
                { _knl_enabled, "KNL" },
                { _neon_enabled, "NEON" },
                { _romol_enabled, "RoMoL" },
            };

            const int N = sizeof(backend_flag) / sizeof(*backend_flag);

            for (int i = 0; i < N; i++)
            {
                for (int j = i + 1; j < N; j++)
                {
                    if ((backend_flag[i].flag)
                            && (backend_flag[j].flag))
                    {
                        fatal_error("SIMD: requesting '%s' and '%s' SIMD instruction sets at the same time\n",
                                backend_flag[i].name,
                                backend_flag[j].name);
                    }
                }
            }

            if(_avx2_enabled)
            {
                // AVX2 Legalization phase
                AVX2VectorLegalization avx2_vector_legalization;
                avx2_vector_legalization.walk(translation_unit);

                VectorizationThreeAddresses three_addresses_visitor;
                three_addresses_visitor.walk(translation_unit);

                // AVX2 Lowering to intrinsics
                AVX2VectorLowering avx2_vector_lowering;
                avx2_vector_lowering.walk(translation_unit);
            }
            else if (_knc_enabled)
            {
                // KNC Legalization phase
                KNCVectorLegalization knc_vector_legalization(
                        _prefer_gather_scatter, _prefer_mask_gather_scatter);
                knc_vector_legalization.walk(translation_unit);

                VectorizationThreeAddresses three_addresses_visitor;
                three_addresses_visitor.walk(translation_unit);

                // Lowering to intrinsics
                KNCVectorBackend knc_vector_backend;
                knc_vector_backend.walk(translation_unit);
            }
            else if (_knl_enabled)
            {
                // KNL Legalization phase
                KNLVectorLegalization knl_vector_legalization(
                        _prefer_gather_scatter, _prefer_mask_gather_scatter);
                knl_vector_legalization.walk(translation_unit);

                VectorizationThreeAddresses three_addresses_visitor;
                three_addresses_visitor.walk(translation_unit);

                // Lowering to intrinsics
                KNLVectorBackend knl_vector_backend;
                knl_vector_backend.walk(translation_unit);
            }
            else if (_neon_enabled)
            {
                // NEON legalization
                NeonVectorLegalization neon_vector_legalization;
                neon_vector_legalization.walk(translation_unit);

                VectorizationThreeAddresses three_addresses_visitor;
                three_addresses_visitor.walk(translation_unit);

                // Lower to NEON intrinsics
                NeonVectorBackend neon_vector_backend;
                neon_vector_backend.walk(translation_unit);
            }
            else if (_romol_enabled)
            {
                RomolVectorLegalization romol_vector_legalization;
                romol_vector_legalization.walk(translation_unit);

                VectorizationThreeAddresses three_addresses_visitor;
                three_addresses_visitor.walk(translation_unit);

                RomolVectorRegAlloc romol_vector_ra;
                romol_vector_ra.walk(translation_unit);

                RomolVectorBackend romol_vector_backend;
                romol_vector_backend.walk(translation_unit);

                if (_valib_sim_header)
                {
                    Nodecl::Utils::prepend_to_top_level_nodecl(
                            Nodecl::PreprocessorLine::make(
                                "#include <valib-sim.h>",
                                /* locus */ 0));
                }
            }
            else
            {
                SSEVectorLegalization sse_vector_legalization;
                sse_vector_legalization.walk(translation_unit);

                VectorizationThreeAddresses three_addresses_visitor;
                three_addresses_visitor.walk(translation_unit);

                SSEVectorBackend sse_vector_backend;
                sse_vector_backend.walk(translation_unit);
            }
        }

        void VectorLoweringPhase::phase_cleanup(TL::DTO& dto)
        {
            delete Vectorizer::_vectorizer;
            Vectorizer::_vectorizer = NULL;
        }
    }
}

EXPORT_PHASE(TL::Vectorization::VectorLoweringPhase);
