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

#ifndef TL_VECTORIZER_LOOP_INFO_HPP
#define TL_VECTORIZER_LOOP_INFO_HPP

#include "tl-nodecl-base.hpp"
#include "tl-analysis-utils.hpp"
#include "tl-vectorizer-environment.hpp"


namespace TL
{
namespace Vectorization
{
    class VectorizerLoopInfo
    {
        private:
            VectorizerEnvironment _environment;
            const Nodecl::NodeclBase _loop;
            const objlist_nodecl_t _ivs;
            Nodecl::NodeclBase _condition;

        public:
            VectorizerLoopInfo(const Nodecl::NodeclBase& n,
                    const VectorizerEnvironment& environment);
/*
            bool ivs_lb_depend_on_simd_iv();
            bool condition_depends_on_simd_iv();
            bool ivs_ub_depend_on_simd_iv();
            bool ivs_step_depend_on_simd_iv();
*/
            bool ivs_values_are_uniform_in_simd_scope();
            bool condition_is_uniform_in_simd_scope();

            int get_epilog_info(bool& only_epilog);
    };
}
}

#endif //TL_VECTORIZER_LOOP_INFO_HPP

