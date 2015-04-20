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


#include "tl-target-information.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-datareference.hpp"
#include "tl-counters.hpp"
#include "tl-predicateutils.hpp"
#include "codegen-phase.hpp"
#include "cxx-diagnostic.h"
#include "cxx-exprtype.h"
#include "fortran03-typeutils.h"

namespace TL { namespace Nanox {


    void TargetInformation::set_outline_name(std::string outline_name)
    {
        _outline_name = outline_name;
    }

    std::string TargetInformation::get_outline_name() const
    {
        return _outline_name;
    }

    void TargetInformation::add_device_name(std::string device_name)
    {
        _device_names.append(device_name);
    }

    ObjectList<std::string> TargetInformation::get_device_names() const
    {
        return _device_names;
    }

    void TargetInformation::set_file(std::string file)
    {
        _file = file;
    }

    std::string TargetInformation::get_file() const
    {
        return _file;
    }

    void TargetInformation::set_name(std::string name)
    {
        _name = name;
    }

    std::string TargetInformation::get_name() const
    {
        return _name;
    }

    void TargetInformation::set_ndrange(const ObjectList<Nodecl::NodeclBase>& ndrange_exprs)
    {
        _ndrange_exprs = ndrange_exprs;
    }

    ObjectList<Nodecl::NodeclBase> TargetInformation::get_ndrange() const
    {
        return _ndrange_exprs;
    }

    void TargetInformation::set_shmem(const ObjectList<Nodecl::NodeclBase>& shmem_exprs)
    {
        _shmem_exprs = shmem_exprs;
    }

    ObjectList<Nodecl::NodeclBase> TargetInformation::get_shmem() const
    {
        return _shmem_exprs;
    }

    void TargetInformation::set_onto(const ObjectList<Nodecl::NodeclBase>& onto_exprs)
    {
        _onto_exprs.append(onto_exprs);
    }

    ObjectList<Nodecl::NodeclBase> TargetInformation::get_onto() const
    {
        return _onto_exprs;
    }

    Nodecl::Utils::SimpleSymbolMap& TargetInformation::get_param_arg_map()
    {
        return _param_to_args;
    }

    void TargetInformation::set_param_arg_map(Nodecl::Utils::SimpleSymbolMap param_arg_map)
    {
        _param_to_args = param_arg_map;
    }
} }
