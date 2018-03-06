/*--------------------------------------------------------------------
  (C) Copyright 2015-2015 Barcelona Supercomputing Center
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

#ifndef TL_NANOS6_DEVICE_HPP
#define TL_NANOS6_DEVICE_HPP

#include "tl-symbol.hpp"
#include "tl-nodecl.hpp"

namespace TL { namespace Nanos6 {

    class Device
    {
        public:
            //! This function returns a symbol that represents the device type id
            virtual TL::Symbol get_device_type_id() const = 0;

            //! Generic implementation: it returns a shallow copy of the task body
            virtual Nodecl::NodeclBase compute_specific_task_body(Nodecl::NodeclBase task_body) const;
    };

} }

#endif // TL_NANOS6_DEVICE_HPP
