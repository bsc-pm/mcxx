#ifndef NANOX_SMP_HPP
#define NANOX_SMP_CPP

#include "tl-compilerphase.hpp"
#include "tl-devices.hpp"

namespace TL
{

    namespace Nanox
    {

        class DeviceSMP : public CompilerPhase, public DeviceProvider
        {
            public:

                // This phase does nothing
                virtual void run(DTO& dto) { }

                DeviceSMP();

                virtual ~DeviceSMP() { }


                virtual void create_outline(const std::string& task_name,
                        const std::string& struct_typename,
                        DataEnvironInfo data_environ,
                        ScopeLink sl,
                        AST_t reference_tree);

                virtual void get_device_descriptor(const std::string& task_name,
                        DataEnvironInfo data_environ,
                        Source &ancillary_device_description,
                        Source &device_descriptor);
        };

    }

}

#endif // NANOX_SMP_CPP
