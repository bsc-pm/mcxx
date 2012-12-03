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



    void TargetInformation::add_device_name(std::string device_name)
    {
        _device_names.append(device_name);
    }

    ObjectList<std::string> TargetInformation::get_device_names()
    {
        return _device_names;
    }

    void TargetInformation::append_to_ndrange(const ObjectList<Nodecl::NodeclBase>& ndrange_exprs)
    {
        _ndrange_exprs.append(ndrange_exprs);
    }

    ObjectList<Nodecl::NodeclBase> TargetInformation::get_ndrange() const
    {
        return _ndrange_exprs;
    }
    
    void TargetInformation::append_to_onto(const ObjectList<Nodecl::NodeclBase>& onto_exprs)
    {
        _onto_exprs.append(onto_exprs);
    }

    ObjectList<Nodecl::NodeclBase> TargetInformation::get_onto() const
    {
        return _onto_exprs;
    }

} }
