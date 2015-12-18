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

#include "tl-vectorizer-report.hpp"

#include "tl-nodecl-utils.hpp"

#include "cxx-diagnostic.h"

namespace TL
{
namespace Vectorization
{

VectorizerReport::VectorizerReport()
{
}

void VectorizerReport::reset_report()
{
    _vloads = 0;
    _aligned_vloads = 0;  
    _unaligned_vloads = 0;

    _vstores = 0;
    _aligned_vstores = 0;
    _unaligned_vstores = 0;

    _vgathers = 0;
    _vscatters = 0;

    _vpromotions = 0;
}

void VectorizerReport::print_report(const Nodecl::NodeclBase& n)
{
    reset_report();
    walk(n);

    info_printf_at(n.get_locus(),
            "Total loads: %d\n",
            _vloads);
    info_printf_at(n.get_locus(),
            "     - Aligned: %d\n",
            _aligned_vloads);
    info_printf_at(n.get_locus(),
            "    - Unaligned: %d\n",
            _unaligned_vloads);
    info_printf_at(n.get_locus(),
            "Total stores: %d\n",
            _vstores);
    info_printf_at(n.get_locus(),
            "    - Aligned: %d\n",
            _aligned_vstores);
    info_printf_at(n.get_locus(),
            "    - Unaligned: %d\n",
            _unaligned_vstores);
    info_printf_at(n.get_locus(),
            "Gathers: %d\n",
            _vgathers);
    info_printf_at(n.get_locus(),
            "Scatters: %d\n",
            _vscatters);
    info_printf_at(n.get_locus(),
            "Vector promotions: %d\n",
            _vpromotions);
}

void VectorizerReport::visit(const Nodecl::ObjectInit& n)
{
    TL::Symbol sym = n.get_symbol();
    Nodecl::NodeclBase init = sym.get_value();

    // Vectorizing initialization
    if(!init.is_null())
    {
        walk(init);
    }
}
 
void VectorizerReport::visit(const Nodecl::VectorLoad& n)
{
    _vloads++;

    Nodecl::List flags = n.get_flags().as<Nodecl::List>();

    bool aligned = !flags.find_first<Nodecl::AlignedFlag>().
            is_null();

    if (aligned)
        _aligned_vloads++;
    else
        _unaligned_vloads++;
 
    walk(n.get_rhs());
    walk(n.get_mask());

}

void VectorizerReport::visit(const Nodecl::VectorStore& n)
{
    _vstores++;

    Nodecl::List flags = n.get_flags().as<Nodecl::List>();

    bool aligned = !flags.find_first<Nodecl::AlignedFlag>().
            is_null();

    if (aligned)
        _aligned_vstores++;
    else
        _unaligned_vstores++;

    walk(n.get_lhs());
    walk(n.get_rhs());
    walk(n.get_mask());
}

void VectorizerReport::visit(const Nodecl::VectorGather& n)
{
    _vgathers++;

    walk(n.get_base());
    walk(n.get_strides());
    walk(n.get_mask());
}

void VectorizerReport::visit(const Nodecl::VectorScatter& n)
{
    _vscatters++;

    walk(n.get_base());
    walk(n.get_strides());
    walk(n.get_source());
    walk(n.get_mask());
}


void VectorizerReport::visit(const Nodecl::VectorPromotion& n)
{
    _vpromotions++;

    walk(n.get_rhs());
    walk(n.get_mask());
}


}
}
