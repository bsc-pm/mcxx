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




#ifndef TL_CONTEXT_HPP
#define TL_CONTEXT_HPP

#include "tl-common.hpp"
#include "tl-scopelink.hpp"
#include <string>

namespace TL
{
    //! Context class used in traversals
    /*!
     * Currently it only stores the scope link available when the traversal
     * started.
     */
    struct LIBTL_CLASS Context
    {
        public:
            ScopeLink scope_link;

            Context(ScopeLink _scope_link)
                : scope_link(_scope_link)
            {
            }
    };
}

#endif // TL_CONTEXT_HPP
