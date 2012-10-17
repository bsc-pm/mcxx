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


#include "tl-nanos.hpp"
#include "tl-devices.hpp"

using namespace TL;
using namespace TL::Nanox;

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

bool DeviceProvider::instrumentation_enabled()
{
    return _enable_instrumentation;
}

bool DeviceProvider::do_not_create_translation_function()
{
    return !Nanos::Version::interface_is_at_least("master", 5003)
        || _do_not_create_translation_fun;
}

bool DeviceProvider::create_translation_function()
{
    return !do_not_create_translation_function();
}

void DeviceProvider::common_constructor_code()
{
    register_parameter("instrument", 
            "Enables instrumentation of the device provider if set to '1'",
            _enable_instrumentation_str,
            "0").connect(functor(&DeviceProvider::set_instrumentation, *this));

    register_parameter("do_not_create_translation_function",
            "Even if the runtime interface supports a translation function, it will not be generated", 
            _do_not_create_translation_str,
            "0").connect(functor(&DeviceProvider::set_translation_function_flag, *this));
}

// shared between all devices!
void DeviceProvider::get_instrumentation_code(
        const std::string& task_name,
        const std::string& struct_typename,
        const std::string& full_outline_name,
        const OutlineFlags& outline_flags,
        AST_t reference_tree,
        ScopeLink sl,
        Source& instrument_before,
        Source& instrument_after)
{
    ERROR_CONDITION(!_enable_instrumentation, "Instrumentation is not enabled\n", 0);

    AST_t function_def_tree = reference_tree.get_enclosing_function_definition();
    FunctionDefinition enclosing_function(function_def_tree, sl);
    Symbol function_symbol = enclosing_function.get_function_symbol();

    std::string outline_name = get_outline_name(task_name);

    Source uf_name_id, uf_name_descr;
    Source uf_location_id, uf_location_descr;

    if (Nanos::Version::interface_is_at_least("master", 5019))
    {
        // The function name used by instrumentantion may contain template arguments
        std::string function_name_instr =
            get_function_name_for_instrumentation(task_name, struct_typename, enclosing_function);

        // The description should contains:
        //  - FUNC_DECL: The declaration of the function. The function name shall be qualified
        //  - FILE: The filename
        //  - LINE: The line number
        //  We use '@' as a separator of fields: FUNC_DECL @ FILE @ LINE
        //
        Source extended_descr;
        if (outline_flags.task_symbol != NULL)
        {
            extended_descr << outline_flags.task_symbol.get_type().get_declaration(
                    outline_flags.task_symbol.get_scope(), outline_flags.task_symbol.get_qualified_name());
        }
        else
        {
            extended_descr << "void " << "::" << full_outline_name << "("<< struct_typename << ")";
        }

        extended_descr
            << "@" << reference_tree.get_file()
            << "@" << reference_tree.get_line()
            ;

        instrument_before
            << "static int nanos_funct_id_init = 0;"
            << "static nanos_event_key_t nanos_instr_uf_location_key = 0;"
            << "if (nanos_funct_id_init == 0)"
            << "{"
            <<    "nanos_err_t err = nanos_instrument_get_key(\"user-funct-location\", &nanos_instr_uf_location_key);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "err = nanos_instrument_register_value_with_val ((nanos_event_value_t) " << function_name_instr << ","
            <<               " \"user-funct-location\", \"" << outline_name << "\", \"" << extended_descr << "\", 0);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "nanos_funct_id_init = 1;"
            << "}"
            << "nanos_event_t event;"
            << "event.type = NANOS_BURST_START;"
            << "event.key = nanos_instr_uf_location_key;"
            << "nanos_instrument_events(1, &event);"
            ;

        if (!is_accelerator_device())
        {
            instrument_after
                << "event.type = NANOS_BURST_END;"
                << "event.key = nanos_instr_uf_location_key;"
                << "nanos_instrument_events(1, &event);"
                ;
        }
    }
    else if (Nanos::Version::interface_is_at_least("master", 5017))
    {
        instrument_before
            << "static int nanos_funct_id_init = 0;"
            << "static nanos_event_key_t nanos_instr_uf_name_key = 0;"
            << "static nanos_event_value_t nanos_instr_uf_name_value = 0;"
            << "static nanos_event_key_t nanos_instr_uf_location_key = 0;"
            << "static nanos_event_value_t nanos_instr_uf_location_value = 0;"
            << "if (nanos_funct_id_init == 0)"
            << "{"
            <<    "nanos_err_t err = nanos_instrument_get_key(\"user-funct-name\", &nanos_instr_uf_name_key);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_name_value, \"user-funct-name\", "
            <<               uf_name_id << "," << uf_name_descr << ", 0);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "err = nanos_instrument_get_key(\"user-funct-location\", &nanos_instr_uf_location_key);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_location_value, \"user-funct-location\","
            <<               uf_location_id << "," << uf_location_descr << ", 0);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "nanos_funct_id_init = 1;"
            << "}"
            << "nanos_event_t events_before[2];"
            << "events_before[0].type = NANOS_BURST_START;"
            << "events_before[1].type = NANOS_BURST_START;"
            << "events_before[0].key = nanos_instr_uf_name_key;"
            << "events_before[0].value = nanos_instr_uf_name_value;"
            << "events_before[1].key = nanos_instr_uf_location_key;"
            << "events_before[1].value = nanos_instr_uf_location_value;"
            << "nanos_instrument_events(2, events_before);"
            ;
        if (!is_accelerator_device())
        {
            instrument_after
                << "nanos_event_t events_after[2];"
                << "events_after[0].type = NANOS_BURST_END;"
                << "events_after[1].type = NANOS_BURST_END;"
                << "events_after[0].key = nanos_instr_uf_name_key;"
                << "events_after[0].value = nanos_instr_uf_name_value;"
                << "events_after[1].key = nanos_instr_uf_location_key;"
                << "events_after[1].value = nanos_instr_uf_location_value;"
                << "nanos_instrument_events(2, events_after);"
                ;
        }
    }
    else
    {
        instrument_before
            << "static int nanos_funct_id_init = 0;"
            << "static nanos_event_key_t nanos_instr_uf_name_key = 0;"
            << "static nanos_event_value_t nanos_instr_uf_name_value = 0;"
            << "static nanos_event_key_t nanos_instr_uf_location_key = 0;"
            << "static nanos_event_value_t nanos_instr_uf_location_value = 0;"
            << "if (nanos_funct_id_init == 0)"
            << "{"
            <<    "nanos_err_t err = nanos_instrument_get_key(\"user-funct-name\", &nanos_instr_uf_name_key);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_name_value, \"user-funct-name\", "
            <<               uf_name_id << "," << uf_name_descr << ", 0);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "err = nanos_instrument_get_key(\"user-funct-location\", &nanos_instr_uf_location_key);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_location_value, \"user-funct-location\","
            <<               uf_location_id << "," << uf_location_descr << ", 0);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "nanos_funct_id_init = 1;"
            << "}"
            << "nanos_event_t events_before[2];"
            << "events_before[0].type = NANOS_BURST_START;"
            << "events_before[1].type = NANOS_BURST_START;"
            << "events_before[0].info.burst.key = nanos_instr_uf_name_key;"
            << "events_before[0].info.burst.value = nanos_instr_uf_name_value;"
            << "events_before[1].info.burst.key = nanos_instr_uf_location_key;"
            << "events_before[1].info.burst.value = nanos_instr_uf_location_value;"
            << "nanos_instrument_events(2, events_before);"
            ;

        if (!is_accelerator_device())
        {
            instrument_after
                << "nanos_event_t events_after[2];"
                << "events_after[0].type = NANOS_BURST_END;"
                << "events_after[1].type = NANOS_BURST_END;"
                << "events_after[0].info.burst.key = nanos_instr_uf_name_key;"
                << "events_after[0].info.burst.value = nanos_instr_uf_name_value;"
                << "events_after[1].info.burst.key = nanos_instr_uf_location_key;"
                << "events_after[1].info.burst.value = nanos_instr_uf_location_value;"
                << "nanos_instrument_events(2, events_after);"
                ;
        }
    }

    if (is_accelerator_device())
    {
        instrument_after << "nanos_instrument_close_user_fun_event();";
    }


    if (outline_flags.task_symbol != NULL)
    {
        uf_name_id
            << "\"" << outline_flags.task_symbol.get_name() << "\""
            ;
        uf_location_id
            << "\"" << outline_name << ":" << reference_tree.get_locus() << "\""
            ;

        uf_name_descr
            << "\"Task '" << outline_flags.task_symbol.get_name() << "'\""
            ;
        uf_location_descr
            << "\"It was invoked from function '" << function_symbol.get_qualified_name() << "'"
            << " in construct at '" << reference_tree.get_locus() << "'\""
            ;
    }
    else
    {
        uf_name_id
            << uf_location_id
            ;
        uf_location_id
            << "\"" << outline_name << ":" << reference_tree.get_locus() << "\""
            ;

        uf_name_descr
            << uf_location_descr
            ;
        uf_location_descr
            << "\"Outline from '"
            << reference_tree.get_locus()
            << "' in '" << function_symbol.get_qualified_name() << "'\""
            ;
    }
}

