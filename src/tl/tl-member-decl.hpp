/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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


#ifndef TL_MEMBER_DECL_HPP
#define TL_MEMBER_DECL_HPP

#include "tl-symbol.hpp"
#include "tl-scope.hpp"

namespace TL {

    // Auxiliar class used by TL::Type
    struct LIBTL_CLASS MemberDeclarationInfo
    {
        private:
            TL::Symbol _entry;
            TL::Scope _scope;
            bool _is_definition;
        public:
            MemberDeclarationInfo(TL::Symbol entry, TL::Scope scope, bool is_definition)
                : _entry(entry), _scope(scope), _is_definition(is_definition)
            {
            }

            TL::Symbol get_symbol() const
            {
                return _entry;
            }

            TL::Scope get_scope() const
            {
                return _scope;
            }

            bool get_is_definition() const
            {
                return _is_definition;
            }
    };

}

#endif // TL_MEMBER_DECL_HPP
