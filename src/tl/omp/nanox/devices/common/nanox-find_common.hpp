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

#ifndef NANOX_FIND_COMMON_HPP
#define NANOX_FIND_COMMON_HPP

#include "tl-devices.hpp"

namespace TL
{
    namespace Nanox
    {
        class FindFunction : public TL::Predicate<AST_t>
        {
            private:
                ScopeLink _sl;
                std::string _func_name;

            public:
                FindFunction(ScopeLink sl, std::string func_name) : _sl(sl), _func_name(func_name){};
                virtual bool do_(const AST_t& ast) const;
        };

        class FindAttribute : public TL::Predicate<AST_t>
        {
            private:
                ScopeLink _sl;
                std::string _attr_name;

            public:
                FindAttribute(ScopeLink sl, std::string attr_name) : _sl (sl), _attr_name(attr_name) {};
                virtual bool do_(const AST_t& ast) const;
        };
    }
}

#endif //NANOX_FIND_COMMON_HPP
