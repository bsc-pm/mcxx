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

#ifndef TL_VECTOR_FUNCTION_VERSION_HPP
#define TL_VECTOR_FUNCTION_VERSION_HPP

#include "tl-versioning.hpp"
#include "tl-nodecl-base.hpp"

namespace TL 
{ 
    namespace Vectorization 
    {
        const int SIMD_FUNC_PRIORITY = 2;
        const int DEFAULT_FUNC_PRIORITY = 1;
        const int NAIVE_FUNC_PRIORITY = 0;

        class VectorFunctionVersion : public TL::Version<Nodecl::NodeclBase>
        {
            private:
                const std::string _device;
                const unsigned int _vector_length;
                const TL::Type _target_type;

            public:
                VectorFunctionVersion(const Nodecl::NodeclBase& version, 
                        const std::string& device, 
                        const unsigned int vector_length, 
                        const TL::Type& _target_type,
                        const int priority);

                bool passes_filter(const Version<Nodecl::NodeclBase>& filter);
        };
    }
}

#endif //TL_VECTOR_FUNCTION_VERSION_HPP

