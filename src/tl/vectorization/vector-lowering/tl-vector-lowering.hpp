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

#ifndef VECTOR_LOWERING_PHASE_HPP
#define VECTOR_LOWERING_PHASE_HPP

#include "tl-compilerphase.hpp"

namespace TL
{
    namespace Vectorization
    {
        class VectorLoweringPhase : public TL::CompilerPhase
        {
            private:
                bool _knl_enabled;
                bool _knc_enabled;
                bool _avx2_enabled;
                bool _neon_enabled;
                bool _romol_enabled;
                bool _prefer_gather_scatter;
                bool _prefer_mask_gather_scatter;
                bool _valib_sim_header;

                std::string _knl_enabled_str;
                std::string _knc_enabled_str;
                std::string _avx2_enabled_str;
                std::string _neon_enabled_str;
                std::string _romol_enabled_str;
                std::string _intel_compiler_profile_str;
                std::string _prefer_gather_scatter_str;
                std::string _prefer_mask_gather_scatter_str;
                std::string _valib_sim_header_str;

                void set_knl(const std::string& knl_enabled_str);
                void set_knc(const std::string& knc_enabled_str);
                void set_avx2(const std::string& avx2_enabled_str);
                void set_neon(const std::string& neon_enabled_str);
                void set_romol(const std::string& romol_enabled_str);
                void set_intel_compiler_profile(
                        const std::string& intel_compiler_profile_str);
                void set_prefer_gather_scatter(
                        const std::string& prefer_gather_scatter_str);
                void set_prefer_mask_gather_scatter(
                        const std::string& prefer_mask_gather_scatter_str);
                void set_valib_sim_header(const std::string& str);

            public:
                VectorLoweringPhase();
                virtual void run(TL::DTO& dto);
                virtual void phase_cleanup(TL::DTO& dto);
        };
    }
}

#endif // VECTOR_LOWERING_PHASE_HPP
