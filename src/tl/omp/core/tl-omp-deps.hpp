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


#ifndef TL_OMP_DEPS_HPP
#define TL_OMP_DEPS_HPP

// This header does not contain anything at the moment, it is just here for
// consistency with the other files

#define BITMAP(x) (1<<x)

namespace TL { namespace OpenMP {

enum DependencyDirection
{
    DEP_DIR_UNDEFINED = 0,
    // Input dependence
    DEP_DIR_INPUT = BITMAP(1),
    // Output dependence
    DEP_DIR_OUTPUT = BITMAP(2),
    // Inout dependence
    DEP_DIR_INOUT = DEP_DIR_INPUT | DEP_DIR_OUTPUT,
    // Reduction dependences
    DEP_REDUCTION = BITMAP(3),
};

} }

#undef BITMAP

#endif // TL_OMP_DEPS_HPP
