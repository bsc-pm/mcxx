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

                // Devices information
                ObjectList<std::string> _device_names;

                ObjectList<Nodecl::NodeclBase> _ndrange_exprs;
                ObjectList<Nodecl::NodeclBase> _shmem_exprs;
                ObjectList<Nodecl::NodeclBase> _onto_exprs;

                // The value of the 'file' clause
                std::string _file;

                // The value of the 'name' clause
                std::string _name;

                Nodecl::Utils::SimpleSymbolMap _param_to_args;

            public:

                // Several members of OutlineInfo fill this class, grant them access to the class!
                friend class OutlineInfo;

                const std::string& get_outline_name() const { return _outline_name; }
                const ObjectList<std::string>& get_device_names() const { return _device_names; }
                const std::string& get_file() const { return _file; }
                const std::string& get_name() const { return _name; }
                const ObjectList<Nodecl::NodeclBase>& get_ndrange() const { return _ndrange_exprs; }
                const ObjectList<Nodecl::NodeclBase>& get_shmem() const { return _shmem_exprs; }
                const ObjectList<Nodecl::NodeclBase>& get_onto() const { return _onto_exprs; }
                const Nodecl::Utils::SimpleSymbolMap& get_param_arg_map() const { return _param_to_args; }
        };
    }
}

#endif // TL_TARGET_INFO_HPP
