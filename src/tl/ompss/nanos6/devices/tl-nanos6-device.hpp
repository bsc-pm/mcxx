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

#include "tl-omp-lowering-directive-environment.hpp"

#include "tl-symbol.hpp"
#include "tl-nodecl.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL { namespace Nanos6 {

    using TL::OpenMP::Lowering::DirectiveEnvironment;

    class Device
    {
        public:
            //! This function returns a symbol that represents the device type id
            virtual TL::Symbol get_device_type_id() const = 0;

            //! This function returns whether the current device requires arguments translation
            virtual bool requires_arguments_translation() const = 0;

            //! Generic implementation: it returns a deep copy of the task_body
            //! in the 'unpacked_inside_scope' context applying the replacements
            //! defined in the 'symbol_map' map.
            //!
            //! @param task_body task statements
            //! @param env OpenMP task environment
            //! @param unpacked_function_code It's used to insert declarations in the right context
            //! @param unpacked_inside_scope It's used to deep_copy the task_body
            //! @param symbol_map It's used to deep_copy the task_body
            virtual Nodecl::NodeclBase compute_specific_task_body(
                    Nodecl::NodeclBase task_body,
                    const DirectiveEnvironment &env,
                    Nodecl::NodeclBase unpacked_function_code,
                    const TL::Scope &unpacked_inside_scope,
                    Nodecl::Utils::SimpleSymbolMap &symbol_map);

            //! Generic implementation: it appends the function code to the top level
            virtual void root_unpacked_function(
                    TL::Symbol unpacked_function, Nodecl::NodeclBase unpacked_function_code);

    };

} }

#endif // TL_NANOS6_DEVICE_HPP
