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



#ifndef TL_OMP_DEPS_HPP
#define TL_OMP_DEPS_HPP

#define BITMAP(x) (1<<x)

namespace TL { namespace OpenMP {

enum DependencyDirection
{
    DEP_DIR_UNDEFINED = 0,
    // Input dependence
    DEP_DIR_IN = BITMAP(1),
    // Input dependence in a parameter passed by value
    DEP_DIR_IN_VALUE = BITMAP(2),
    // Input dependence with firstprivate storage
    DEP_DIR_IN_PRIVATE = BITMAP(3),
    // Output dependence
    DEP_DIR_OUT = BITMAP(4),
    // Inout dependence
    DEP_DIR_INOUT = DEP_DIR_IN | DEP_DIR_OUT,
    // Concurrent dependences
    DEP_CONCURRENT = BITMAP(5),
    DEP_COMMUTATIVE = BITMAP(6),
};

std::string get_dependency_direction_name(DependencyDirection d);

} }

#undef BITMAP

#endif // TL_OMP_DEPS_HPP
