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

#ifndef TL_NANOX_PTR_HPP
#define TL_NANOX_PTR_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-scope.hpp"

namespace TL { namespace Nanox {

    Symbol get_function_ptr_of(TL::Symbol sym, TL::Scope original_scope);
    Symbol get_function_ptr_of(TL::Type t, TL::Scope original_scope);

    Symbol get_function_ptr_conversion(
            TL::Type return_type,
            TL::Type argument_type,
            TL::Scope original_scope);

    Symbol get_copy_descriptor_function(TL::Symbol orig_sym,
            TL::Symbol dest_sym,
            TL::Scope original_scope);
} }

#endif // TL_NANOX_PTR_HPP
