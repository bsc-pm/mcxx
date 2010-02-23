#ifndef NANOX_DEVICES_H
#define NANOX_DEVICES_H

#include "tl-objectlist.hpp"
#include "tl-data-env.hpp"

namespace TL {

namespace Nanox
{

    class DeviceProvider
    {
        public:
            virtual void create_outline(const std::string& task_name,
                    const std::string& struct_typename,
                    DataEnvironInfo data_environ,
                    ScopeLink sl,
                    AST_t reference_tree) = 0;

            virtual void get_device_descriptor(const std::string& task_name,
                    DataEnvironInfo data_environ,
                    Source &ancillary_device_description,
                    Source &device_descriptor) = 0;

            virtual ~DeviceProvider() { }
    };

    class DeviceHandler
    {
        public:
            static DeviceHandler& get_device_handler();

            void register_device(const std::string& str, 
                    DeviceProvider* nanox_device_provider);

            DeviceProvider* get_device(const std::string& str);

        private:
            typedef std::map<std::string, DeviceProvider*> nanox_devices_map_t;
            nanox_devices_map_t _nanox_devices;
    };
}

}

#endif // NANOX_DEVICES_H
