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

#ifndef TL_OMP_SIMD_HPP
#define TL_OMP_SIMD_HPP

#include "tl-pragmasupport.hpp"

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
                virtual void phase_cleanup(TL::DTO& dto);

                virtual ~Simd() { }

            private:
                std::string _simd_enabled_str;
                std::string _svml_enabled_str;
                std::string _fast_math_enabled_str;
                std::string _avx2_enabled_str;
                std::string _neon_enabled_str;
                std::string _romol_enabled_str;
                std::string _knc_enabled_str;
                std::string _knl_enabled_str;
                std::string _only_adjacent_accesses_str;
                std::string _only_aligned_accesses_str;
                std::string _overlap_in_place_str;

                bool _simd_enabled;
                bool _svml_enabled;
                bool _fast_math_enabled;
                bool _avx2_enabled;
                bool _neon_enabled;
                bool _romol_enabled;
                bool _knc_enabled;
                bool _knl_enabled;
                bool _only_adjacent_accesses_enabled;
                bool _only_aligned_accesses_enabled;
                bool _overlap_in_place;

                void set_simd(const std::string simd_enabled_str);
                void set_svml(const std::string svml_enabled_str);
                void set_fast_math(const std::string fast_math_enabled_str);
                void set_avx2(const std::string avx2_enabled_str);
                void set_neon(const std::string neon_enabled_str);
                void set_romol(const std::string romol_enabled_str);
                void set_knc(const std::string knc_enabled_str);
                void set_knl(const std::string knl_enabled_str);
                void set_only_adjcent_accesses(const std::string only_adjacent_accesses_str);
                void set_only_aligned_accesses(const std::string only_aligned_accesses_str);
                void set_overlap_in_place(const std::string overlap_in_place_str);
        };
    }
}

#endif // TL_OMP_SIMD_HPP
