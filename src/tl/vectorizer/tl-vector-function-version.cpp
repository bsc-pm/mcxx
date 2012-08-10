/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "tl-vector-function-version.hpp"

namespace TL 
{ 
    namespace Vectorization 
    {
        VectorFunctionVersion::VectorFunctionVersion(const Nodecl::NodeclBase& version, 
                const std::string& device, 
                const unsigned int vector_length, 
                const TL::Type& target_type, 
                const int priority) :
            TL::Version<Nodecl::NodeclBase>(version, priority),
            _device(device), _vector_length(vector_length), _target_type(target_type)
        {

        }

        bool VectorFunctionVersion::passes_filter(const Version<Nodecl::NodeclBase>& filter)
        {
            try
            {
               const VectorFunctionVersion& function_filter = 
                    dynamic_cast<const VectorFunctionVersion&>(filter);

                return (_device.compare(function_filter._device) == 0) &&
                    (_vector_length == function_filter._vector_length) //&&
                    //(_target_type.is_same_type(function_filter._target_type) 
                    ;
            }
            catch (std::bad_cast& excep)
            {
                return false;
            }
        }

    }
}


