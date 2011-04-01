/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#include "tl-omp-nanox.hpp"
#include "tl-nanos.hpp"

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::add_openmp_initializer(TL::DTO& dto)
{
    if (Nanos::Version::interface_is_at_least("openmp", 3))
    {
        AST_t a = dto["translation_unit"];
        ScopeLink sl = dto["scope_link"];

        Source src;

        if (!_static_weak_symbols)
        {
            src 
                << "__attribute__((weak, section(\"nanos_init\"))) nanos_init_desc_t __section__nanos_init = { nanos_omp_set_interface, (void*)0 };"
                ;
        }
        else
        {
            // Some compilers (like ICC) require this
            src 
                << "static __attribute__((section(\"nanos_init\"))) nanos_init_desc_t __section__nanos_init = { nanos_omp_set_interface, (void*)0 };"
                ;
        }

        AST_t tree = src.parse_global(a, sl);
        a.append_to_translation_unit(tree);
    }
}
