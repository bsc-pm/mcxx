/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#ifndef TL_OMP_TARGET_HPP
#define TL_OMP_TARGET_HPP

#include "tl-objectlist.hpp"
#include "tl-symbol.hpp"

namespace TL
{
    namespace OpenMP
    {
        struct TargetContext
        {
            ObjectList<std::string> device_list;

            ObjectList<std::string> copy_in;
            ObjectList<std::string> copy_out;

            bool has_implements;
            Symbol implements;

            TargetContext()
                : device_list(), copy_in(), copy_out(), has_implements(), implements() 
            {
            }
        };
    }
}

#endif // TL_OMP_TARGET_HPP
