#ifndef NANOX_GPU_HPP
#define NANOX_GPU_CPP

#include "tl-compilerphase.hpp"
#include "tl-devices.hpp"

namespace TL
{

    namespace Nanox
    {

        class DeviceGPU : public DeviceProvider
        {
            private:
                std::string _cudaFilename;
                AST_t _root;
                std::set<std::string> _taskSymbols;
            public:

                // This phase does nothing
                virtual void pre_run(DTO& dto);
                virtual void run(DTO& dto) { }

                DeviceGPU();

                virtual ~DeviceGPU() { }

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
                        AST_t reference_tree,
                        ScopeLink sl,
                        Source &ancillary_device_description,
                        Source &device_descriptor);

                virtual void phase_cleanup(DTO& data_flow);
        };

    }

}

#endif // NANOX_GPU_CPP
