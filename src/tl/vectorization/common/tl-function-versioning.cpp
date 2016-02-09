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

#include "tl-function-versioning.hpp"
#include "tl-vectorization-utils.hpp"

namespace TL
{
    namespace Vectorization
    {
        FunctionVersioning vec_func_versioning;
        std::list<TL::Symbol> vec_math_library_funcs;

        VectorFunctionVersion::VectorFunctionVersion(
            const Nodecl::NodeclBase &func_version,
            const std::string &device,
            const unsigned int vec_factor,
            const bool masked,
            const FunctionPriority priority)
            : _func_version(func_version),
              _priority(priority),
              _device(device),
              _vec_factor(vec_factor),
              _masked(masked)
        {
        }

        const Nodecl::NodeclBase VectorFunctionVersion::get_version() const
        {
            return _func_version;
        }

        bool VectorFunctionVersion::has_kind(const std::string& device,
                const unsigned int vec_factor,
                const bool masked) const
        {
            return (_device == device) &&
                (_vec_factor == vec_factor) &&
                (_masked == masked);
        }

        bool VectorFunctionVersion::is_better_than(const VectorFunctionVersion& func_version) const
        {
            return _priority < func_version._priority;
        }

        FunctionVersioning::FunctionVersioning()
        {
        }

        void FunctionVersioning::clear()
        {
            _versions.clear();
        }

        void FunctionVersioning::add_version(
            TL::Symbol scalar_func_sym,
            const Nodecl::NodeclBase &func_version,
            const std::string &device,
            const unsigned int vec_factor,
            const bool masked,
            const FunctionPriority priority)
        {
            VECTORIZATION_DEBUG()
            {
                scope_entry_t *sym = scalar_func_sym.get_internal_symbol();
                fprintf(stderr,
                        "Function Versioning: Adding %p '%s' function version "
                        "(device=%s, vec_factor=%u, masked=%d,"
                        " priority=%d)\n",
                        sym,
                        print_decl_type_str(
                            sym->type_information,
                            sym->decl_context,
                            get_qualified_symbol_name(sym, sym->decl_context)),
                        device.c_str(),
                        vec_factor,
                        masked,
                        priority);
            }

            _versions.insert(std::make_pair(scalar_func_sym,
                                             VectorFunctionVersion(func_version,
                                                                   device,
                                                                   vec_factor,
                                                                   masked,
                                                                   priority)));
        }

        FunctionVersioning::versions_map_t::const_iterator 
            FunctionVersioning::find_best_function(
                TL::Symbol func_name,
                const std::string& device,
                const unsigned int vec_factor,
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
                if (it->second.has_kind(device, vec_factor, masked))
                {
                    best_version = it;
                    break;
                }
            }

            for (;it != func_range.second;
                    it++)
            {
                if (it->second.has_kind(device, vec_factor, masked) &&
                        it->second.is_better_than(best_version->second))
                {
                    best_version = it;
                }
            }

            return best_version;
        }

        const VectorFunctionVersion* FunctionVersioning::get_best_function_version(TL::Symbol func_name,
                const std::string& device,
                const unsigned int vec_factor,
                const bool masked) const
        {
            versions_map_t::const_iterator best_version = 
                find_best_function(func_name, device, 
                    vec_factor, masked);

            if (best_version == _versions.end())
            {
                fprintf(stderr,
                        "Warning: There is no vector version of function %p "
                        "'%s' for '%s', 'mask=%d'\n",
                        func_name.get_internal_symbol(),
                        func_name.get_qualified_name().c_str(),
                        device.c_str(),
                        masked);

                // TODO
                // Generate Naive Function.
                // Get symbol from name.
                // Parse Naive Function Code.
                // Add Naive Function to versions
                // Append
            }
            else
            {
                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr,
                            "Found vector version of function %p '%s' for "
                            "'%s', %d, 'mask=%d'\n",
                            func_name.get_internal_symbol(),
                            func_name.get_qualified_name().c_str(),
                            device.c_str(),
                            vec_factor,
                            masked);
                }
            }

            if (best_version == _versions.end())
            {
                return NULL;
                //fatal_error("Error: There is no vector version of function '%s' for '%s', '%s', '%d', 'mask=%d'",
                //    func_name.c_str(), device.c_str(),
                //    vec_factor, masked);
            }

            return &best_version->second;
        }

        const Nodecl::NodeclBase FunctionVersioning::get_best_version(TL::Symbol func_name,
                const std::string& device,
                const unsigned int vec_factor,
                const bool masked) const
        {
            const VectorFunctionVersion* best_func = get_best_function_version(func_name,
                    device, vec_factor,
                    masked);

            if (best_func == NULL)
                return Nodecl::NodeclBase::null();
            else
                return best_func->get_version();
        }
    };
}


