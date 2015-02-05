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

#include "tl-vectorizer-target-type-heuristic.hpp"

namespace TL
{
namespace Vectorization
{
    VectorizerTargetTypeHeuristic::VectorizerTargetTypeHeuristic()
    {
    }

    TL::Type VectorizerTargetTypeHeuristic::get_target_type(
            const Nodecl::NodeclBase& n)
    {
        _1b = 0;
        _2b = 0;
        _4b = 0;
        _8b = 0;

        walk(n);

        // TODO
        if (_8b != 0) return TL::Type::get_double_type();
        else return TL::Type::get_float_type();
    }

    void VectorizerTargetTypeHeuristic::count_type(
            const TL::Type& type)
    {
        if (type.is_double() || type.is_signed_long_long_int()
                || type.is_unsigned_long_long_int()) _8b++;
        else if (type.is_float() || type.is_signed_int()
                    || type.is_unsigned_int()) _4b++;
        else if (type.is_unsigned_short_int() ||
                type.is_unsigned_short_int()) _2b++;
        else if (type.is_char()) _1b++;
    }

    void VectorizerTargetTypeHeuristic::visit(
            const Nodecl::ArraySubscript& n)
    {
        count_type(n.get_type().no_ref());
    }

    void VectorizerTargetTypeHeuristic::visit(
            const Nodecl::ObjectInit& n)
    {
        TL::Symbol sym = n.get_symbol();
        Nodecl::NodeclBase init = sym.get_value();

        if(!init.is_null())
        {
            walk(init);
        }
    }
}
}
