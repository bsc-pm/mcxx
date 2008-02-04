/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef TL_CONTEXT_HPP
#define TL_CONTEXT_HPP

#include "tl-scopelink.hpp"
#include <string>

namespace TL
{
    //! Context class used in traversals
    /*!
     * Currently it only stores the scope link available when the traversal
     * started.
     */
    struct Context
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
