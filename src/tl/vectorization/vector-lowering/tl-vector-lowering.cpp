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

#include "tl-vector-lowering.hpp"
#include "tl-vector-lowering-sse.hpp"
#include "tl-vector-legalization-knc.hpp"
#include "tl-vector-backend-knc.hpp"
#include "tl-vector-legalization-avx2.hpp"
#include "tl-vector-backend-avx2.hpp"


namespace TL
{
    namespace Vectorization
    {
        VectorLoweringPhase::VectorLoweringPhase() : _knc_enabled(false), _avx2_enabled(false)
        {
            register_parameter("mic_enabled",
                    "If set to '1' enables compilation for KNC architecture, otherwise it is disabled",
                    _knc_enabled_str,
                    "0").connect(functor(&VectorLoweringPhase::set_knc, *this));

            register_parameter("avx2_enabled",
                    "If set to '1' enables compilation for AVX2 architecture, otherwise it is disabled",
                    _avx2_enabled_str,
                    "0").connect(functor(&VectorLoweringPhase::set_avx2, *this));

            register_parameter("intel_compiler_profile",
                    "If set to '1' enables the Intel Compiler profile, otherwise GNU is assumed",
                    _intel_compiler_profile_str,
                    "0").connect(functor(&VectorLoweringPhase::set_intel_compiler_profile, *this));

            register_parameter("prefer_mask_gather_scatter",
                    "If set to '1' enables gather/scatter generation for unaligned load/stores with masks",
                    _prefer_mask_gather_scatter_str,
                    "0").connect(functor(&VectorLoweringPhase::set_prefer_mask_gather_scatter, *this));

            register_parameter("prefer_gather_scatter",
                    "If set to '1' enables gather/scatter generation for unaligned load/stores",
                    _prefer_gather_scatter_str,
                    "0").connect(functor(&VectorLoweringPhase::set_prefer_gather_scatter, *this));
        }

        void VectorLoweringPhase::set_knc(const std::string knc_enabled_str)
        {
            if (knc_enabled_str == "1")
            {
                _knc_enabled = true;
            }
        }

        void VectorLoweringPhase::set_avx2(const std::string avx2_enabled_str)
        {
            if (avx2_enabled_str == "1")
            {
                _avx2_enabled = true;
            }
        }

        void VectorLoweringPhase::set_intel_compiler_profile(
                const std::string intel_compiler_profile_str)
        {
            if (intel_compiler_profile_str == "1")
            {
                _intel_compiler_profile = true;
            }
        }

        void VectorLoweringPhase::set_prefer_gather_scatter(
                const std::string prefer_gather_scatter_str)
        {
            if (prefer_gather_scatter_str == "1")
            {
                _prefer_gather_scatter = true;
            }
        }

        void VectorLoweringPhase::set_prefer_mask_gather_scatter(
                const std::string prefer_mask_gather_scatter_str)
        {
            if (prefer_mask_gather_scatter_str == "1")
            {
                _prefer_mask_gather_scatter = true;
            }
        }

        void VectorLoweringPhase::run(TL::DTO& dto)
        {
            Nodecl::NodeclBase translation_unit = dto["nodecl"];

            if (_avx2_enabled && _knc_enabled)
            {
                running_error("SIMD: AVX2 and KNC SIMD instruction sets enabled at the same time");
            }

            if(_avx2_enabled)
            {
                // KNC Legalization phase
                AVX2VectorLegalization avx2_vector_legalization;
                avx2_vector_legalization.walk(translation_unit);

                // Lowering to intrinsics
                AVX2VectorLowering avx2_vector_lowering;
                avx2_vector_lowering.walk(translation_unit);
            }
            else if (_knc_enabled)
            {
                // KNC Legalization phase
                KNCVectorLegalization knc_vector_legalization(
                        _prefer_gather_scatter, _prefer_mask_gather_scatter);
                knc_vector_legalization.walk(translation_unit);

                // Lowering to intrinsics
                KNCVectorBackend knc_vector_backend;
                knc_vector_backend.walk(translation_unit);
            }
            else
            {
                SSEVectorLowering sse_vector_lowering(_intel_compiler_profile);
                sse_vector_lowering.walk(translation_unit);
            }
        }
    }
}

EXPORT_PHASE(TL::Vectorization::VectorLoweringPhase);
