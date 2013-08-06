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
#include "tl-vector-lowering-knc.hpp"

namespace TL
{
    namespace Vectorization
    {
        VectorLoweringPhase::VectorLoweringPhase() : _mic_enabled(false)
        {
            register_parameter("mic_enabled",
                    "If set to '1' enables compilation for MIC architecture, otherwise it is disabled",
                    _mic_enabled_str,
                    "0").connect(functor(&VectorLoweringPhase::set_mic, *this));
        }

        void VectorLoweringPhase::set_mic(const std::string mic_enabled_str)
        {
            if (mic_enabled_str == "1")
            {
                _mic_enabled = true;
            }
        }

        void VectorLoweringPhase::run(TL::DTO& dto)
        {
            Nodecl::NodeclBase translation_unit = dto["nodecl"];

           
            if(_mic_enabled)
            { 
                // KNC Legalization phase
                KNCVectorLegalization knc_vector_legalization;
                knc_vector_legalization.walk(translation_unit);

                // Lowering to intrinsics
                KNCVectorLowering knc_vector_lowering;
                knc_vector_lowering.walk(translation_unit);
            }
            else
            {
                SSEVectorLowering sse_vector_lowering;
                sse_vector_lowering.walk(translation_unit);
            }
        }
    }
}

EXPORT_PHASE(TL::Vectorization::VectorLoweringPhase);
