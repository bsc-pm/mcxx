/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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

#include "tl-devices.hpp"
#include "tl-nanos.hpp"

namespace TL { namespace Nanox {

    static DeviceHandler* _nanox_handler = 0;

    DeviceHandler& DeviceHandler::get_device_handler()
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

    DeviceProvider::DeviceProvider(const std::string& device_name)
        : _device_name(device_name),
          _enable_instrumentation(false),
          _enable_instrumentation_str("")
    {
             DeviceHandler &device_handler(DeviceHandler::get_device_handler());
             device_handler.register_device(device_name, this);

             common_constructor_code();
    }

    DeviceProvider* DeviceHandler::get_device(const std::string& str)
    {
        nanox_devices_map_t::iterator it = _nanox_devices.find(str);

        if (it == _nanox_devices.end())
            return NULL;
        else
            return it->second;
    }

    std::string DeviceProvider::get_name() const
    {
        return _device_name;
    }

    void DeviceProvider::set_instrumentation(const std::string& str)
    {
        _enable_instrumentation = false;
        parse_boolean_option(/* Parameter name */ "instrument",
                /* Given value */ str,
                /* Computed bool */ _enable_instrumentation,
                /* Error message */  "Instrumentation disabled");
    }

     bool DeviceProvider::instrumentation_enabled()
     {
         return _enable_instrumentation;
     }
    //
    //
    // bool DeviceProvider::do_not_create_translation_function()
    // {
    //     return !Nanos::Version::interface_is_at_least("master", 5003)
    //         || _do_not_create_translation_fun;
    // }
    // 
    // bool DeviceProvider::create_translation_function()
    // {
    //     return !do_not_create_translation_function();
    // }
    // 

     void DeviceProvider::common_constructor_code()
     {
         register_parameter("instrument",
                 "Enables instrumentation of the device provider if set to '1'",
                 _enable_instrumentation_str,
                 "0").connect(functor(&DeviceProvider::set_instrumentation, *this));

         //     register_parameter("do_not_create_translation_function",
         //             "Even if the runtime interface supports a translation function, it will not be generated",
         //             _do_not_create_translation_str,
         //             "0").connect(functor(&DeviceProvider::set_translation_function_flag, *this));
     }
    void DeviceProvider::get_instrumentation_code(
            const TL::Symbol& called_task,
            const TL::Symbol& outline_function,
            Nodecl::NodeclBase outline_function_body,
            std::string filename,
            int line,
            Source& instrumentation_before,
            Source& instrumentation_after)
    {
        if (Nanos::Version::interface_is_at_least("master", 5019))
        {
            Source extended_descr, extra_cast, instrument_before_c,
            instrument_after_c, function_name_instr;

            if (called_task.is_valid())
            {
                // It's a function task
                extended_descr << called_task.get_type().get_declaration(
                        called_task.get_scope(), called_task.get_qualified_name());
            }
            else
            {
                // It's an inline task
                std::string function_name =
                    outline_function.get_type().get_declaration(
                            outline_function.get_scope(), outline_function.get_qualified_name());

                // The character '@' will be used as a separator of the
                // description. Since the function name may contain one or
                // more '@' characters, we should replace them by an other
                // special char
                for (unsigned int i = 0; i < function_name.length(); i++)
                {
                    if (function_name[i] == '@')
                        function_name[i] = '#';
                }

                extended_descr << function_name;
            }

            // The description should contains:
            //  - FUNC_DECL: The declaration of the function. The function name shall be qualified
            //  - FILE: The filename
            //  - LINE: The line number
            //  We use '@' as a separator of fields: FUNC_DECL @ FILE @ LINE
            extended_descr << "@" << filename << "@" << line;

            // GCC complains if you convert a pointer to an integer of different
            // size. Since we target a unsigned long long, in architectures of 32
            // bits we first cast to an unsigned int
            if (CURRENT_CONFIGURATION->type_environment->sizeof_function_pointer == 4)
            {
                extra_cast << "(unsigned int)";
            }

            // FIXME: We may need an additional cast here (GCC bug solved in 4.5)
            function_name_instr << as_symbol(outline_function);

            instrument_before_c
                << "static int nanos_funct_id_init = 0;"
                << "static nanos_event_key_t nanos_instr_uf_location_key = 0;"
                << "if (nanos_funct_id_init == 0)"
                << "{"
                <<    "nanos_err_t err = nanos_instrument_get_key(\"user-funct-location\", &nanos_instr_uf_location_key);"
                <<    "if (err != NANOS_OK) nanos_handle_error(err);"
                <<    "err = nanos_instrument_register_value_with_val ((nanos_event_value_t) "<< extra_cast << function_name_instr << ","
                <<               " \"user-funct-location\", \"" << outline_function.get_name() << "\", \"" << extended_descr << "\", 0);"
                <<    "if (err != NANOS_OK) nanos_handle_error(err);"
                <<    "nanos_funct_id_init = 1;"
                << "}"
                << "nanos_event_t event;"
                << "event.type = NANOS_BURST_START;"
                << "event.key = nanos_instr_uf_location_key;"
                << "event.value = (nanos_event_value_t) " << extra_cast << function_name_instr << ";"
                << "nanos_instrument_events(1, &event);"
                ;

            if (is_gpu_device())
            {
                instrument_after_c << "nanos_instrument_close_user_fun_event();";
            }
            else
            {
                instrument_after_c
                    << "event.type = NANOS_BURST_END;"
                    << "event.key = nanos_instr_uf_location_key;"
                    << "event.value = (nanos_event_value_t) " << extra_cast << function_name_instr << ";"
                    << "nanos_instrument_events(1, &event);"
                    ;
            }

            if (IS_FORTRAN_LANGUAGE)
                Source::source_language = SourceLanguage::C;

            Nodecl::NodeclBase instr_before = instrument_before_c.parse_statement(outline_function_body);
            Nodecl::NodeclBase instr_after = instrument_after_c.parse_statement(outline_function_body);

            if (IS_FORTRAN_LANGUAGE)
                Source::source_language = SourceLanguage::Current;

            instrumentation_before << as_statement(instr_before);
            instrumentation_after << as_statement(instr_after);
        }
        else
        {
            internal_error("Unsupported nanox version for instrumentation", 0);
        }
    }

    bool DeviceProvider::is_gpu_device() const
    {
        return false;
    }

} }
