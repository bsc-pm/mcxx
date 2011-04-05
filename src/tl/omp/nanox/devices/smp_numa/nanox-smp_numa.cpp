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
#include "nanox-smp_numa.hpp"

using namespace TL;
using namespace TL::Nanox;

static std::string smp_outline_name(const std::string &task_name)
{
    return "_smp_numa_" + task_name;
}

void DeviceSMP_NUMA::do_smp_numa_inline_get_addresses(
        const Scope& sc,
        const DataEnvironInfo& data_env_info,
        Source &copy_setup,
        ReplaceSrcIdExpression& replace_src,
        bool &err_declared)
{
    Source current_wd_param;
    if (Nanos::Version::interface_is_at_least("master", 5005))
    {
        copy_setup
            << "nanos_wd_t current_wd = nanos_current_wd();"
            ;
        current_wd_param
            << ", current_wd"
            ;
    }

    ObjectList<OpenMP::CopyItem> copies = data_env_info.get_copy_items();
    unsigned int j = 0;
    for (ObjectList<OpenMP::CopyItem>::iterator it = copies.begin();
            it != copies.end();
            it++, j++)
    {
        DataReference data_ref = it->get_copy_expression();
        Symbol sym = data_ref.get_base_symbol();

        OpenMP::DataSharingEnvironment &data_sharing = data_env_info.get_data_sharing();
        OpenMP::DataSharingAttribute data_sharing_attr = data_sharing.get_data_sharing(sym);

        bool is_private = !((data_sharing_attr & OpenMP::DS_SHARED) == OpenMP::DS_SHARED);

        Type type = sym.get_type();

        bool requires_indirect = false;

        if (type.is_array())
        {
            type = type.array_element().get_pointer_to();
        }
        if (is_private
                || !type.is_pointer())
        {
            requires_indirect = true;
            type = type.get_pointer_to();
        }

        if (!is_private
                && (data_ref.is_array_section_range()
                    || data_ref.is_array_section_size()))
        {
            // Array sections have a scalar type, but the data type will be array
            // See ticket #290
            type = data_ref.get_data_type().array_element().get_pointer_to();
            requires_indirect = false;
        }

        std::string copy_name = "_cp_" + sym.get_name();

        if (!err_declared)
        {
            copy_setup
                << "nanos_err_t cp_err;"
                ;
            err_declared = true;
        }

        DataEnvironItem data_env_item = data_env_info.get_data_of_symbol(sym);

        ERROR_CONDITION(!data_env_item.get_symbol().is_valid(),
                "Invalid data for copy symbol", 0);

        // std::string field_addr = "_args->" + data_env_item.get_field_name();

        copy_setup
            << type.get_declaration(sc, copy_name) << ";"
            << "cp_err = nanos_get_addr(" << j << ", (void**)&" << copy_name << current_wd_param << ");"
            << "if (cp_err != NANOS_OK) nanos_handle_error(cp_err);"
            ;

        if (!requires_indirect)
        {
            replace_src.add_replacement(sym, copy_name);
        }
        else
        {
            replace_src.add_replacement(sym, "(*" + copy_name + ")");
        }
    }
}

void DeviceSMP_NUMA::do_smp_numa_outline_replacements(
        AST_t body,
        ScopeLink scope_link,
        const DataEnvironInfo& data_env_info,
        Source &initial_code,
        Source &replaced_outline)
{
    Source copy_setup;
    Scope sc = scope_link.get_scope(body);

    initial_code
        << copy_setup
        ;

    ReplaceSrcIdExpression replace_src(scope_link);

    replace_src.add_this_replacement("_args->_this");

    OpenMP::DataSharingEnvironment& data_sharing_env = data_env_info.get_data_sharing();

    ObjectList<DataEnvironItem> data_env_items = data_env_info.get_items();
    for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
            it != data_env_items.end();
            it++)
    {
        DataEnvironItem& data_env_item(*it);

        Symbol sym = data_env_item.get_symbol();

        const std::string field_name = data_env_item.get_field_name();

        if (data_env_item.is_private())
            continue;

        if (data_env_item.is_copy()
                || create_translation_function())
        {
            OpenMP::DataSharingAttribute data_sharing_attr = data_sharing_env.get_data_sharing(sym);

            bool copy_is_shared = (data_sharing_attr & OpenMP::DS_SHARED) == OpenMP::DS_SHARED;
            if (copy_is_shared)
            {
                replace_src.add_replacement(sym, "(*_args->" + field_name + ")");
            }
            else
            {
                replace_src.add_replacement(sym, "_args->" + field_name);
            }
        }
    }

    if (create_translation_function())
    {
        // We already created a function that performs the translation in the runtime
        copy_setup
            << comment("Translation is done by the runtime")
            ;
    }
    else
    {
        bool err_declared = false;
        do_smp_numa_inline_get_addresses(
                sc,
                data_env_info,
                copy_setup,
                replace_src,
                err_declared);
    }

    replaced_outline << replace_src.replace(body);
}

DeviceSMP_NUMA::DeviceSMP_NUMA()
    : DeviceProvider("smp_numa")
{
    set_phase_name("Nanox SMP NUMA support");
    set_phase_description("This phase is used by Nanox phases to implement SMP NUMA device support");
}

void DeviceSMP_NUMA::create_outline(
        const std::string& task_name,
        const std::string& struct_typename,
        DataEnvironInfo &data_environ,
        const OutlineFlags& outline_flags,
        AST_t reference_tree,
        ScopeLink sl,
        Source initial_setup,
        Source outline_body)
{
    AST_t function_def_tree = reference_tree.get_enclosing_function_definition();
    FunctionDefinition enclosing_function(function_def_tree, sl);

    Source result, body, outline_name, parameter_list;

    Source forward_declaration;
    Symbol function_symbol = enclosing_function.get_function_symbol();

    Source template_header, linkage_specifiers;
    if (enclosing_function.is_templated())
    {
        Source template_params;
        template_header
            << "template <" << template_params << ">"
            ;
        ObjectList<TemplateHeader> template_header_list = enclosing_function.get_template_header();
        for (ObjectList<TemplateHeader>::iterator it = template_header_list.begin();
                it != template_header_list.end();
                it++)
        {
            ObjectList<TemplateParameterConstruct> tpl_params = it->get_parameters();
            for (ObjectList<TemplateParameterConstruct>::iterator it2 = tpl_params.begin();
                    it2 != tpl_params.end();
                    it2++)
            {
                template_params.append_with_separator(it2->prettyprint(), ",");
            }
        }
    }
    else if (enclosing_function.has_linkage_specifier())
    {
        linkage_specifiers << concat_strings(enclosing_function.get_linkage_specifier(), " ");
    }

    if (!function_symbol.is_member())
    {
        Source template_header, linkage_specifiers;

        IdExpression function_name = enclosing_function.get_function_name();
        Declaration point_of_decl = function_name.get_declaration();
        DeclarationSpec decl_specs = point_of_decl.get_declaration_specifiers();
        ObjectList<DeclaredEntity> declared_entities = point_of_decl.get_declared_entities();
        DeclaredEntity declared_entity = *(declared_entities.begin());

        forward_declaration 
            << linkage_specifiers
            << template_header
            << decl_specs.prettyprint()
            << " "
            << declared_entity.prettyprint()
            << ";";
    }

    Source instrument_before, instrument_after;

    result
        << forward_declaration
        << "static void " << outline_name << "(" << parameter_list << ")"
        << "{"
        << instrument_before
        << body
        << instrument_after
        << "}"
        ;

    if (instrumentation_enabled())
    {
        Source uf_name_id, uf_name_descr;
        Source uf_location_id, uf_location_descr;
        Symbol function_symbol = enclosing_function.get_function_symbol();

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
            << "events_before[0].info.burst.key = nanos_instr_uf_name_key;"
            << "events_before[0].info.burst.value = nanos_instr_uf_name_value;"
            << "events_before[1].type = NANOS_BURST_START;"
            << "events_before[1].info.burst.key = nanos_instr_uf_location_key;"
            << "events_before[1].info.burst.value = nanos_instr_uf_location_value;"
            << "nanos_instrument_events(2, events_before);"
            // << "nanos_instrument_point_event(1, &nanos_instr_uf_location_key, &nanos_instr_uf_location_value);"
            // << "nanos_instrument_enter_burst(nanos_instr_uf_name_key, nanos_instr_uf_name_value);"
            ;

        instrument_after
            << "nanos_event_t events_after[2];"
            << "events_after[0].type = NANOS_BURST_END;"
            << "events_after[0].info.burst.key = nanos_instr_uf_name_key;"
            << "events_after[0].info.burst.value = nanos_instr_uf_name_value;"
            << "events_after[1].type = NANOS_BURST_END;"
            << "events_after[1].info.burst.key = nanos_instr_uf_location_key;"
            << "events_after[1].info.burst.value = nanos_instr_uf_location_value;"
            << "nanos_instrument_events(2, events_after);"
//            << "nanos_instrument_leave_burst(nanos_instr_uf_name_key);"
            ;

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
                << "\"'" << function_symbol.get_qualified_name() << "'"
                << " invoked at '" << reference_tree.get_locus() << "'\""
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

    parameter_list
        << struct_typename << "* const _args"
        ;

    outline_name
        << smp_outline_name(task_name)
        ;

    Source private_vars, final_code;

    body
        << private_vars
        << initial_setup
        << outline_body
        << final_code
        ;

    ObjectList<DataEnvironItem> data_env_items = data_environ.get_items();

    for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
            it != data_env_items.end();
            it++)
    {
        if (it->is_private())
        {
            Symbol sym = it->get_symbol();
            Type type = sym.get_type();

            if (type.is_reference())
            {
                type = type.references_to();
            }

            private_vars
                << type.get_declaration(sym.get_scope(), sym.get_name()) << ";"
                ;
        }
        else if (it->is_raw_buffer())
        {
            Symbol sym = it->get_symbol();
            Type type = sym.get_type();
            std::string field_name = it->get_field_name();

            if (type.is_reference())
            {
                type = type.references_to();
            }

            if (!type.is_named_class())
            {
                internal_error("invalid class type in field of raw buffer", 0);
            }

            final_code
                << field_name << ".~" << type.get_symbol().get_name() << "();"
                ;
        }
    }

    if (outline_flags.barrier_at_end)
    {
        final_code
            << "nanos_team_barrier();"
            ;
    }

    if (outline_flags.leave_team)
    {
        final_code
            << "nanos_leave_team();"
            ;
    }

    // Parse it in a sibling function context
    AST_t outline_code_tree
        = result.parse_declaration(enclosing_function.get_ast(), sl);
    reference_tree.prepend_sibling_function(outline_code_tree);
}

void DeviceSMP_NUMA::get_device_descriptor(const std::string& task_name,
        DataEnvironInfo &data_environ,
        const OutlineFlags& outline_flags,
        AST_t reference_tree,
        ScopeLink sl,
        Source &ancillary_device_description,
        Source &device_descriptor)
{
    Source outline_name;
    if (!outline_flags.implemented_outline)
    {
        outline_name
            << smp_outline_name(task_name);
        ;
    }
    else
    {
        outline_name << task_name;
    }

    ancillary_device_description
        << comment("SMP device descriptor")
        << "nanos_smp_args_t " << task_name << "_smp_numa_args = { (void(*)(void*))" << outline_name << "};"
        ;

    device_descriptor
        << "{ nanos_smp_factory, nanos_smp_dd_size, &" << task_name << "_smp_numa_args },"
        ;
}

void DeviceSMP_NUMA::do_replacements(DataEnvironInfo& data_environ,
        AST_t body,
        ScopeLink scope_link,
        Source &initial_setup,
        Source &replaced_src)
{
    do_smp_numa_outline_replacements(body,
            scope_link,
            data_environ,
            initial_setup,
            replaced_src);
}

EXPORT_PHASE(TL::Nanox::DeviceSMP_NUMA);
