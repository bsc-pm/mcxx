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



#ifndef TL_OMP_DEPS_HPP
#define TL_OMP_DEPS_HPP

#include "tl-common.hpp"

#include "tl-object.hpp"
#include "tl-datareference.hpp"

#include <string>

#define BITMAP(x) (1<<x)

namespace TL { namespace OpenMP {

    enum DependencyDirection
    {
        DEP_DIR_UNDEFINED = 0,
        // Input dependence
        DEP_DIR_IN = BITMAP(1),
        // Output dependence
        DEP_DIR_OUT = BITMAP(2),
        // Inout dependence
        DEP_DIR_INOUT = DEP_DIR_IN | DEP_DIR_OUT,

        // OmpSs
        //   Input dependence with firstprivate storage
        DEP_OMPSS_DIR_IN_PRIVATE = BITMAP(3),
        //   Concurrent/Commutative dependences
        DEP_OMPSS_CONCURRENT = BITMAP(4),
        DEP_OMPSS_COMMUTATIVE = BITMAP(5),
        //   Weak
        DEP_OMPSS_WEAK_IN = BITMAP(6),
        DEP_OMPSS_WEAK_OUT = BITMAP(7),
        DEP_OMPSS_WEAK_INOUT = DEP_OMPSS_WEAK_IN | DEP_OMPSS_WEAK_OUT,
        //   Reduction dependence type
        DEP_OMPSS_REDUCTION = BITMAP(8)
    };

    bool is_strict_dependency(DependencyDirection dir);
    bool is_weak_dependency(DependencyDirection dir);

    std::string dependency_direction_to_str(DependencyDirection d);

    class LIBTL_CLASS DependencyItem : public TL::Object
    {
        private:
            DataReference _dep_expr;
            DependencyDirection _kind;
        public:
            DependencyItem() { }
            DependencyItem(DataReference dep_expr, DependencyDirection kind);

            DependencyDirection get_kind() const;
            DataReference get_dependency_expression() const;

            void module_write(ModuleWriter& mw);
            void module_read(ModuleReader& mw);
    };

} }

#undef BITMAP

#endif // TL_OMP_DEPS_HPP
