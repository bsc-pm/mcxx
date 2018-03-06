/*--------------------------------------------------------------------
  (C) Copyright 2015-2015 Barcelona Supercomputing Center
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

#ifndef TL_NANOS6_DEVICE_FACTORY_HPP
#define TL_NANOS6_DEVICE_FACTORY_HPP

#include "tl-object.hpp"

#include "tl-nanos6-device.hpp"
#include "smp/tl-nanos6-smp-device.hpp"
#include "cuda/tl-nanos6-cuda-device.hpp"

#include "cxx-diagnostic.h"

namespace TL { namespace Nanos6 {

    class DeviceFactory
    {
        public:
            static std::shared_ptr<Device> get_device(const std::string &device_name) {
                if (device_name == "smp")
                {
                    return std::shared_ptr<SMPDevice>(new SMPDevice());
                }
                else if (device_name == "cuda")
                {
                    //FIXME: CHECK AT THIS POINT WHETHER CUDA WAS ENABLED!
                    return std::shared_ptr<CUDADevice>(new CUDADevice());
                }
                else
                {
                    fatal_error("unrecognized '%s' device name\n", device_name.c_str());
                }
            }
    };
}}

#endif // TL_NANOS6_DEVICE_FACTORY_HPP
