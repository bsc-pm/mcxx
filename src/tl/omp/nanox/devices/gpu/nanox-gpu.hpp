#ifndef NANOX_GPU_HPP
#define NANOX_GPU_HPP

#include "tl-compilerphase.hpp"
#include "tl-devices.hpp"

namespace TL
{

    namespace Nanox
    {

        class DeviceGPU : public DeviceProvider
        {
            private:
                std::string _gpuFilename;
                AST_t _root;
                std::set<std::string> _taskSymbols;

            public:
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
        };

        class ReplaceSrcGPU : public ReplaceSrcIdExpression
        {
            private:
                unsigned char * num_generic_vectors;

            protected:
                static const char* prettyprint_callback (AST a, void* data);
                static const char* recursive_prettyprint(AST_t a, void* data);

            public:
                ReplaceSrcGPU(ScopeLink sl) : ReplaceSrcIdExpression(sl)
                {
                    num_generic_vectors = new unsigned char(0);
                }

                ~ReplaceSrcGPU()
                {
                    delete(num_generic_vectors);
                }

                Source replace(AST_t a) const;
        };
    }

}

#endif // NANOX_GPU_HPP
