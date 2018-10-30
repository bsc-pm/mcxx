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

#ifndef TL_NANOS6_DEVICE_MANAGER_HPP
#define TL_NANOS6_DEVICE_MANAGER_HPP

#include"tl-nanos6-device-factory.hpp"
#include<map>

namespace TL { namespace Nanos6 {

    //! A DeviceManager is basically a map that caches the devices that have been used in the Lowering
    class DeviceManager
    {
        private:
            std::map<std::string, std::shared_ptr<Device>> _device_map;

        public:
            DeviceManager() {}

            std::shared_ptr<Device> get_device(const std::string& device_name)
            {
                std::map<std::string, std::shared_ptr<Device>>::iterator it = _device_map.find(device_name);
                if (it != _device_map.end())
                    return it->second;

                std::shared_ptr<Device> new_device = DeviceFactory::get_device(device_name);
                _device_map[device_name] = new_device;

                return new_device;
            }
    };

}}
#endif //TL_NANOS6_DEVICE_MANAGER_HPP
