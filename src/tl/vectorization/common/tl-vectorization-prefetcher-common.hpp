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

#ifndef TL_VECTORIZATION_PREFETCHER_COMMON_HPP
#define TL_VECTORIZATION_PREFETCHER_COMMON_HPP

namespace TL
{
namespace Vectorization
{
enum PrefetchKind
{
    L1_READ = 1,
    L2_READ = 2,
    L1_WRITE = 3,
    L2_WRITE = 4
};

typedef struct prefetch_info
{
    int distances[2];
    bool enabled;
    bool in_place;

    prefetch_info() : enabled(false), in_place(false)
    {
    }
} prefetch_info_t;
}
}

#endif //TL_VECTORIZATION_PREFETCHER_COMMON_HPP
