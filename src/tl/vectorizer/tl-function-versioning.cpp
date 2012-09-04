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

#include "tl-function-versioning.hpp"

namespace TL 
{ 
    namespace Vectorization 
    {
        VectorFunctionVersion::VectorFunctionVersion(const Nodecl::NodeclBase& func_version,
                const std::string& device, 
                const unsigned int vector_length, 
                const TL::Type& target_type, 
                const FunctionPriority priority) :
            _func_version(func_version), _priority(priority), _device(device),
            _vector_length(vector_length), _target_type(target_type)
        {
        }

        const Nodecl::NodeclBase VectorFunctionVersion::get_version() const
        {
            return _func_version;
        }

        bool VectorFunctionVersion::has_kind(const std::string& device,
                const unsigned int vector_length,
                const TL::Type& target_type) const
        {
            return (_device == device) &&
                (_vector_length == vector_length); // &&
                //_target_type.is_same_type(target_type);
        }

        bool VectorFunctionVersion::is_better_than(const VectorFunctionVersion& func_version) const
        {
            return _priority < func_version._priority;
        }


        FunctionVersioning::FunctionVersioning()
        {
        }

        void FunctionVersioning::add_version(const std::string& func_name, 
                const VectorFunctionVersion& value)
        {
            _versions.insert(std::pair<const std::string, const VectorFunctionVersion>(func_name, value));
        }

        const Nodecl::NodeclBase FunctionVersioning::get_best_version(const std::string& func_name,
                const std::string& device,
                const unsigned int vector_length,
                const Type& target_type) const
        {
            typename versions_map_t::const_iterator it = _versions.find(func_name);
            typename versions_map_t::const_iterator best_version = _versions.end();

            for (;it != _versions.end();
                    it++)
            {
                if (it->second.has_kind(device, vector_length, target_type))
                {
                    best_version = it;
                    break;
                }
            }

            for (;it != _versions.end();
                    it++)
            {
                if (it->second.has_kind(device, vector_length, target_type) &&
                        it->second.is_better_than(best_version->second))
                {
                    best_version = it;
                }
            }

            if (best_version == _versions.end())
            {
                // TODO
                // Generate Naive Function.
                // Get symbol from name.
                // Parse Naive Function Code.
                // Add Naive Function to versions
                // Append
            }

            ERROR_CONDITION(best_version == _versions.end(), 
                    "There are no versions of function '%s' for '%s' '%d'", 
                    func_name.c_str(), device.c_str(), vector_length);

            return best_version->second.get_version();
        }
    };
}


