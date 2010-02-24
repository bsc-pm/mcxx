#ifndef NANOX_DEVICES_H
#define NANOX_DEVICES_H

#include "tl-objectlist.hpp"
#include "tl-data-env.hpp"

namespace TL {

namespace Nanox
{
    struct OutlineFlags
    {
        bool barrier_at_end;
        bool leave_team;

        OutlineFlags()
            : barrier_at_end(false),
            leave_team(false)
        {
        }
    };

    class DeviceProvider
    {
        public:
            virtual void create_outline(
                    const std::string& task_name,
                    const std::string& struct_typename,
                    DataEnvironInfo &data_environ,
                    const OutlineFlags& outline_flags,
                    AST_t reference_tree,
                    ScopeLink sl,
                    Source initial_setup,
                    Source outline_body) = 0;

            virtual void do_replacements(DataEnvironInfo& data_environ,
                    AST_t body,
                    ScopeLink scope_link,
                    Source &replace_setup,
                    Source &replaced_src) = 0;

            virtual void get_device_descriptor(const std::string& task_name,
                    DataEnvironInfo &data_environ,
                    const OutlineFlags& outline_flags,
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
