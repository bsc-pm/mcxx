#include "tl-omp-nanox.hpp"
#include "tl-devices.hpp"

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::target_preorder(PragmaCustomConstruct ctr)
{
    PragmaCustomClause device = ctr.get_clause(device);
    if (device.is_defined())
    {
        DeviceHandler& device_handler(DeviceHandler::get_device_handler());

        ObjectList<std::string> arguments = device.get_arguments(ExpressionTokenizerTrim());

        ObjectList<std::string> valid;
        for (ObjectList<std::string>::iterator it = arguments.begin();
                it != arguments.end();
                it++)
        {
            if (device_handler.get_device(*it) != NULL)
            {
                valid.push_back(*it);
            }
            else
            {
                std::cerr << ctr.get_ast().get_locus() << ": warning: unknown device '" << *it << "' name, skipping" << std::endl;
            }
        }

        _target_ctx.push_back(valid);
    }
    else
    {
        std::cerr << ctr.get_ast().get_locus() << ": warning: '#pragma omp target' requires a 'device' clause" << std::endl;

        // In this case we will simply add 'smp' just not to break the stack later
        ObjectList<std::string> devices;
        devices.push_back("smp");

        _target_ctx.push_back(devices);
    }
}

void OMPTransform::target_postorder(PragmaCustomConstruct ctr)
{
    _target_ctx.pop_back();
}
