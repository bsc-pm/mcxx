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
#ifndef TL_OMP_BASE_UTILS_HPP
#define TL_OMP_BASE_UTILS_HPP

#include "tl-omp-base.hpp"
#include "tl-omp-core.hpp"

namespace TL { namespace OpenMP {

    std::string Base::dependence_direction_to_str(DependencyDirection kind)
    {
        switch (kind)
        {
            case OpenMP::DEP_DIR_IN:
                return "in";
            case OpenMP::DEP_DIR_OUT:
                return "out";
            case OpenMP::DEP_DIR_INOUT:
                return "inout";
                // OmpSs
            case OpenMP::DEP_OMPSS_DIR_IN_PRIVATE:
                return "in (private)";
            case OpenMP::DEP_OMPSS_CONCURRENT:
                return "concurrent";
            case OpenMP::DEP_OMPSS_COMMUTATIVE:
                return "commutative";
            default: ;
        }
        return "unknown dependency???";
    }

    // This belongs to OmpSs but it now a bit unwieldy to move
    std::string Base::copy_direction_to_str(TL::OmpSs::CopyDirection kind)
    {
        switch (kind)
        {
            case OmpSs::COPY_DIR_IN:
                return "copied in";
            case OmpSs::COPY_DIR_OUT:
                return "copied out";
            case OmpSs::COPY_DIR_INOUT:
                return "copied inout";
            default: ;
        }
        return "copied unknown???";
    }
}}
#endif // TL_OMP_BASE_UTILS_HPP
