/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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


#include "tl-constants-folding.hpp"
#include "cxx-cexpr.h"

namespace TL {

    namespace {
        void fold_constants_in_place(Nodecl::NodeclBase expression)
        {
            if (expression.is_null())
                return;

            if (expression.is_constant())
            {
                const_value_t* cval = expression.get_constant();

                if (const_value_is_integer(cval)
                        || const_value_is_floating(cval))
                {
                    expression.replace(const_value_to_nodecl(cval));
                }
                return;
            }

            TL::ObjectList<Nodecl::NodeclBase> children = expression.children();

            for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
                    it != children.end();
                    it++)
            {
                fold_constants_in_place(*it);
            }
        }
    }

    Nodecl::NodeclBase Analysis::fold_constants(Nodecl::NodeclBase expression)
    {
        Nodecl::NodeclBase copy = expression.shallow_copy();
        fold_constants_in_place(expression);
        return copy;
    }

}
