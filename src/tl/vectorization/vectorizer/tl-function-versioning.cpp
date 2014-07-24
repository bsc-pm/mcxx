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

#include "tl-function-versioning.hpp"

namespace TL
{
    namespace Vectorization
    {
        VectorFunctionVersion::VectorFunctionVersion(const Nodecl::NodeclBase& func_version,
                const std::string& device,
                const unsigned int vector_length,
                const TL::Type& target_type,
                const bool masked,
                const FunctionPriority priority,
                const bool is_svml) :
            _func_version(func_version), _priority(priority), _device(device),
            _vector_length(vector_length), _target_type(target_type), _masked(masked),
            _is_svml(is_svml)
        {
        }

        const Nodecl::NodeclBase VectorFunctionVersion::get_version() const
        {
            return _func_version;
        }

        bool VectorFunctionVersion::has_kind(const std::string& device,
                const unsigned int vector_length,
                const TL::Type& target_type,
                const bool masked) const
        {
            bool compatible_type = (_target_type.get_size() == target_type.get_size());

            return (_device == device) &&
                (_vector_length == vector_length) &&
                compatible_type &&
                (_masked == masked);
        }

        bool VectorFunctionVersion::is_better_than(const VectorFunctionVersion& func_version) const
        {
            return _priority < func_version._priority;
        }

        bool VectorFunctionVersion::is_svml_function() const
        {
            return _is_svml;
        }


        FunctionVersioning::FunctionVersioning()
        {
        }

        void FunctionVersioning::add_version(const std::string& func_name,
                const VectorFunctionVersion& value)
        {
            _versions.insert(std::pair<const std::string, const VectorFunctionVersion>(func_name, value));
        }

        FunctionVersioning::versions_map_t::const_iterator 
            FunctionVersioning::find_best_function(
                const std::string& func_name,
                const std::string& device,
                const unsigned int vector_length,
                const Type& target_type,
                const bool masked) const
        {
            std::pair<versions_map_t::const_iterator, versions_map_t::const_iterator> func_range =
                _versions.equal_range(func_name);

            versions_map_t::const_iterator it;
            versions_map_t::const_iterator best_version = _versions.end();

            for (it = func_range.first;
                    it != func_range.second;
                    it++)
            {
                if (it->second.has_kind(device, vector_length, target_type, masked))
                {
                    best_version = it;
                    break;
                }
            }

            for (;it != func_range.second;
                    it++)
            {
                if (it->second.has_kind(device, vector_length, target_type, masked) &&
                        it->second.is_better_than(best_version->second))
                {
                    best_version = it;
                }
            }

            return best_version;
        }

        const VectorFunctionVersion FunctionVersioning::get_best_function_version(const std::string& func_name,
                const std::string& device,
                const unsigned int vector_length,
                const Type& target_type,
                const bool masked) const
        {
            versions_map_t::const_iterator best_version = 
                find_best_function(func_name, device, 
                    vector_length, target_type, masked);

            if (best_version == _versions.end())
            {
                // TODO
                // Generate Naive Function.
                // Get symbol from name.
                // Parse Naive Function Code.
                // Add Naive Function to versions
                // Append
            }

            if (best_version == _versions.end())
            {
                running_error("Error: There is no vector version of function '%s' for '%s', '%s', '%d', 'mask=%d'",
                    func_name.c_str(), device.c_str(),
                    target_type.get_simple_declaration(TL::Scope::get_global_scope() , "").c_str(),
                    vector_length, masked);
            }

            return best_version->second;
        }

        const Nodecl::NodeclBase FunctionVersioning::get_best_version(const std::string& func_name,
                const std::string& device,
                const unsigned int vector_length,
                const Type& target_type,
                const bool masked) const
        {
            return get_best_function_version(func_name,
                    device,
                    vector_length,
                    target_type,
                    masked).get_version();
        }

        bool FunctionVersioning::is_svml_function(const std::string& func_name,
                const std::string& device,
                const unsigned int vector_length,
                const Type& target_type,
                const bool masked) const
        {
            versions_map_t::const_iterator best_version = 
                find_best_function(func_name, device, 
                    vector_length, target_type, masked);

            if (best_version == _versions.end())
                return false;
            else
                return best_version->second
                    .is_svml_function();
        }
    };
}


