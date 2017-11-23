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

#ifndef TL_TARGET_INFORMATION_HPP
#define TL_TARGET_INFORMATION_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-nodecl.hpp"
#include "tl-nodecl-utils.hpp"
#include <string>
#include <sstream>

#include "tl-omp.hpp"

namespace TL
{
    namespace Nanox
    {

        class TargetInformation
        {
            private:

                // This name is the generic outline name. Every device of the list
                // '_device_name' should append its name to the generic  outline name.
                std::string _outline_name;

                // The value of the 'file' clause
                std::string _file;

                // The value of the 'name' clause
                std::string _name;

                ObjectList<Nodecl::NodeclBase> _ndrange_exprs;
                ObjectList<Nodecl::NodeclBase> _shmem_exprs;
                ObjectList<Nodecl::NodeclBase> _onto_exprs;

                // Devices information
                ObjectList<std::string> _device_names;

                Nodecl::Utils::SimpleSymbolMap _param_to_args;

            public:
                TargetInformation() {}

                void set_outline_name(std::string outline_name)
                {
                    _outline_name = outline_name;
                }

                std::string get_outline_name() const
                {
                    return _outline_name;
                }

                void add_device_name(std::string device_name)
                {
                    _device_names.append(device_name);
                }

                ObjectList<std::string> get_device_names() const
                {
                    return _device_names;
                }

                void set_file(std::string file)
                {
                    _file = file;
                }

                std::string get_file() const
                {
                    return _file;
                }

                void set_name(std::string name)
                {
                    _name = name;
                }

                std::string get_name() const
                {
                    return _name;
                }

                void set_ndrange(const ObjectList<Nodecl::NodeclBase>& ndrange)
                {
                    _ndrange_exprs = ndrange;
                }

                ObjectList<Nodecl::NodeclBase> get_ndrange() const
                {
                    return _ndrange_exprs;
                }

                void set_shmem(const ObjectList<Nodecl::NodeclBase>& shmem)
                {
                    _shmem_exprs = shmem;
                }

                ObjectList<Nodecl::NodeclBase> get_shmem() const
                {
                    return _shmem_exprs;
                }

                void set_onto(const ObjectList<Nodecl::NodeclBase>& onto)
                {
                    _onto_exprs.append(onto);
                }

                ObjectList<Nodecl::NodeclBase> get_onto() const
                {
                    return _onto_exprs;
                }

                Nodecl::Utils::SimpleSymbolMap& get_param_arg_map()
                {
                    return _param_to_args;
                }

                void set_param_arg_map(Nodecl::Utils::SimpleSymbolMap param_arg_map)
                {
                    _param_to_args = param_arg_map;
                }
        };
    }
}

#endif // TL_TARGET_INFO_HPP
