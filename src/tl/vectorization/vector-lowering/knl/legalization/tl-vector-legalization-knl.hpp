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

#ifndef KNL_VECTOR_LEGALIZATION_HPP
#define KNL_VECTOR_LEGALIZATION_HPP

#include "tl-vector-legalization-knc.hpp"
#include "tl-vectorization-analysis-interface.hpp"

#define MASK_BIT_SIZE 16
#define KNL_VECTOR_LENGTH 64

namespace TL
{
    namespace Vectorization
    {
        class KNLVectorLegalization : public KNCVectorLegalization
        {
            public:

                KNLVectorLegalization(bool prefer_gather_scatter,
                        bool prefer_mask_gather_scatter);

                virtual void visit(const Nodecl::VectorLoad& n);
                virtual void visit(const Nodecl::VectorStore& n);
        };
    }
}

#endif // KNL_VECTOR_LEGALIZATION_HPP
