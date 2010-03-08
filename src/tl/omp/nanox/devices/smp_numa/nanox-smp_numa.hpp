#ifndef NANOX_SMP_HPP
#define NANOX_SMP_CPP

#include "tl-compilerphase.hpp"
#include "tl-devices.hpp"

namespace TL
{

    namespace Nanox
    {

        class DeviceSMP_NUMA : public CompilerPhase, public DeviceProvider
        {
            public:

                // This phase does nothing
                virtual void run(DTO& dto) { }

                DeviceSMP_NUMA();

                virtual ~DeviceSMP_NUMA() { }

                virtual void create_outline(
                        const std::string& task_name,
                        const std::string& struct_typename,
                        DataEnvironInfo &data_environ,
                        const OutlineFlags& outline_flags,
                        AST_t reference_tree,
                        ScopeLink sl,
                        Source initial_setup,
                        Source outline_body);

                virtual void do_replacements(DataEnvironInfo& data_environ,
                        AST_t body,
                        ScopeLink scope_link,
                        Source &initial_setup,
                        Source &replace_src);

                virtual void get_device_descriptor(const std::string& task_name,
                        DataEnvironInfo &data_environ,
                        const OutlineFlags& outline_flags,
                        Source &ancillary_device_description,
                        Source &device_descriptor);
        };

    }

}

#endif // NANOX_SMP_CPP
