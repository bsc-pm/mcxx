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



#ifndef TL_OMP_LOWERING_UTILS_HPP
#define TL_OMP_LOWERING_UTILS_HPP

#include "tl-nodecl.hpp"
#include "tl-datareference.hpp"
#include "tl-type.hpp"

namespace TL { namespace OpenMP { namespace Lowering { namespace Utils {

    namespace Fortran
    {
        //FIXME: ADD DESCRIPTIONS!
        Nodecl::NodeclBase get_lower_bound(Nodecl::NodeclBase expr, int dimension_num);

        Nodecl::NodeclBase get_upper_bound(Nodecl::NodeclBase expr, int dimension_num);

        Nodecl::NodeclBase get_size_for_dimension(const TL::DataReference& data_ref, TL::Type array_type, int dimension_num);

        //! It honours the C/C++ preprocessor flags in Fortran, executing them over an empty file that is
        //parsed by our C/C++ FE afterwards. It returns the parsed tree representing the C/C++ headers.
        Nodecl::NodeclBase preprocess_api(Nodecl::NodeclBase top_level);

        //! This function looks for all the function symbols specified as entry_points and mark them as BIND(C).
        //! A multidimensional_entry_point is a way to represent a set of functions that have the same name but
        //! with a number from [1..num_dims] as suffix.
        void fixup_entry_points(const char **entry_points, const char **multidimensional_entry_points, int num_dims);
    }

} } } }

#endif // TL_OMP_LOWERING_UTILS_HPP
