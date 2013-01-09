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

#include "nanox-opencl.hpp"

namespace TL { namespace Nanox {

    DeviceOpenCL::DeviceOpenCL()
        : DeviceProvider("opencl")
    {
        set_phase_name("Nanox OpenCL support");
        set_phase_description("This phase is used by Nanox phases to implement OpenCL support");
    }

    void DeviceOpenCL::pre_run(DTO& dto)
    {
    }

    void DeviceOpenCL::run(DTO& dto)
    {
    }

    void DeviceOpenCL::phase_cleanup(DTO& data_flow)
    {
    }

    void DeviceOpenCL::create_outline(CreateOutlineInfo &info,
            Nodecl::NodeclBase &outline_placeholder,
            Nodecl::NodeclBase &output_statements,
            Nodecl::Utils::SymbolMap* &symbol_map)
    {
    }

    void DeviceOpenCL::get_device_descriptor(DeviceDescriptorInfo& info,
            Source &ancillary_device_description,
            Source &device_descriptor,
            Source &fortran_dynamic_init)
    {
    }

    void DeviceOpenCL::copy_stuff_to_device_file(const TL::ObjectList<Nodecl::NodeclBase>& stuff_to_be_copied)
    {
    }

} }

EXPORT_PHASE(TL::Nanox::DeviceOpenCL);
