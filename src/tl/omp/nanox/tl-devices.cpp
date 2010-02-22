#include "tl-devices.hpp"

using namespace TL;
using namespace TL::Nanox;

static DeviceHandler* _nanox_handler = 0;

DeviceHandler::DeviceHandler& get_device_handler()
{
    if (_nanox_handler == 0)
    {
        _nanox_handler = new DeviceHandler();
    }
    return *_nanox_handler;
}

void DeviceHandler::register_device(const std::string& str, DeviceProvider* nanox_device_provider)
{
    _nanox_devices[str] = nanox_device_provider;
}

DeviceProvider* DeviceHandler::get_device(const std::string& str)
{
    nanox_devices_map_t::iterator it = _nanox_devices.find(str);

    if (it == _nanox_devices.end())
        return NULL;
    else
        return it->second;
}
