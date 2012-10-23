/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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
#include "nanox-fpga.hpp"

namespace TL { namespace Nanox {

    void DeviceFPGA::create_outline(CreateOutlineInfo& info,
            Nodecl::NodeclBase& outline_placeholder,
            Nodecl::Utils::SymbolMap* &symbol_map)
    {
    //    //Unpack DTO
    //    const std::string& outline_name = smp_outline_name(info._outline_name);
    //    OutlineInfo& outline_info = info._outline_info;
    //    Nodecl::NodeclBase& original_statements = info._original_statements;
    //    TL::Symbol& arguments_struct = info._arguments_struct;

    //    TL::Symbol current_function = original_statements.retrieve_context().get_decl_context().current_scope->related_entry;

    //    if (current_function.is_nested_function())
    //    {
    //        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    //            running_error("%s: error: nested functions are not supported\n",
    //                    original_statements.get_locus().c_str());
    //        if (IS_FORTRAN_LANGUAGE)
    //            running_error("%s: error: internal subprograms are not supported\n",
    //                    original_statements.get_locus().c_str());
    //    }

    //    Source unpacked_arguments, cleanup_code, private_entities, extra_declarations;

    //    int lower_bound_index = 0;
    //    int upper_bound_index = 0;

    //    TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
    //    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
    //            it != data_items.end();
    //            it++)
    //    {
    //        switch ((*it)->get_sharing())
    //        {
    //            case OutlineDataItem::SHARING_PRIVATE:
    //                {
    //                    // Do nothing
    //                    if ((*it)->get_symbol().is_valid()
    //                            && (*it)->get_symbol().is_allocatable())
    //                    {
    //                        private_entities << emit_allocate_statement((*it)->get_symbol(), lower_bound_index, upper_bound_index);
    //                    }
    //                    break;
    //                }
    //            case OutlineDataItem::SHARING_SHARED:
    //            case OutlineDataItem::SHARING_CAPTURE:
    //            case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
    //            case OutlineDataItem::SHARING_SHARED_PRIVATE:
    //            case OutlineDataItem::SHARING_SHARED_CAPTURED_PRIVATE:
    //                {
    //                    TL::Type param_type = (*it)->get_in_outline_type();

    //                    switch ((*it)->get_item_kind())
    //                    {
    //                        case OutlineDataItem::ITEM_KIND_NORMAL:
    //                        case OutlineDataItem::ITEM_KIND_DATA_DIMENSION:
    //                            {
    //                                break;
    //                            }
    //                        case OutlineDataItem::ITEM_KIND_DATA_ADDRESS:
    //                            {
    //                                param_type = TL::Type::get_void_type().get_pointer_to();

    //                                break;
    //                            }
    //                        default:
    //                            {
    //                                internal_error("Code unreachable", 0);
    //                            }
    //                    }

    //                    Source argument;
    //                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    //                    {
    //                        // Normal shared items are passed by reference from a pointer,
    //                        // derreference here
    //                        if (((*it)->get_sharing() == OutlineDataItem::SHARING_SHARED
    //                                    || (*it)->get_sharing() == OutlineDataItem::SHARING_SHARED_CAPTURED_PRIVATE
    //                                    || (*it)->get_sharing() == OutlineDataItem::SHARING_SHARED_PRIVATE)
    //                                && (*it)->get_item_kind() == OutlineDataItem::ITEM_KIND_NORMAL
    //                                && !(IS_CXX_LANGUAGE && (*it)->get_symbol().get_name() == "this"))
    //                        {
    //                            argument << "*(args." << (*it)->get_field_name() << ")";
    //                        }
    //                        // Any other thing is passed by value
    //                        else
    //                        {
    //                            argument << "args." << (*it)->get_field_name();
    //                        }

    //                        if (IS_CXX_LANGUAGE
    //                                && (*it)->get_allocation_policy() == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY)
    //                        {
    //                            internal_error("Not yet implemented: call the destructor", 0);
    //                        }
    //                    }
    //                    else if (IS_FORTRAN_LANGUAGE)
    //                    {
    //                        argument << "args % " << (*it)->get_field_name();

    //                        bool is_allocatable = (*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE;
    //                        bool is_pointer = (*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_POINTER;

    //                        if (is_allocatable
    //                                || is_pointer)
    //                        {
    //                            cleanup_code
    //                                << "DEALLOCATE(args % " << (*it)->get_field_name() << ")\n"
    //                                ;
    //                        }
    //                    }
    //                    else
    //                    {
    //                        internal_error("running error", 0);
    //                    }

    //                    if  ((*it)->get_sharing() == OutlineDataItem::SHARING_SHARED_CAPTURED_PRIVATE)
    //                    {
    //                        std::string name = (*it)->get_symbol().get_name();

    //                        private_entities
    //                            << "p_" << name << " = " << name << ";"
    //                            ;
    //                    }

    //                    unpacked_arguments.append_with_separator(argument, ", ");
    //                    break;
    //                }
    //            case OutlineDataItem::SHARING_REDUCTION:
    //                {
    //                    // Pass the original reduced variable as if it were a shared
    //                    Source argument;
    //                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    //                    {
    //                        argument << "*(args." << (*it)->get_field_name() << ")";
    //                    }
    //                    else if (IS_FORTRAN_LANGUAGE)
    //                    {
    //                        argument << "args % " << (*it)->get_field_name();
    //                    }
    //                    unpacked_arguments.append_with_separator(argument, ", ");

    //                    std::string name = (*it)->get_symbol().get_name();

    //                    private_entities
    //                        << "rdp_" << name << " = " << as_expression( (*it)->get_reduction_info()->get_identity()) << ";"
    //                        ;

    //                    break;
    //                }
    //            default:
    //                {
    //                    internal_error("Unexpected data sharing kind", 0);
    //                }
    //        }
    //    }

    //    TL::Symbol unpacked_function, forward_function;
    //    if (IS_FORTRAN_LANGUAGE)
    //    {
    //        forward_function = new_function_symbol_forward(
    //                current_function,
    //                outline_name + "_forward",
    //                outline_info);
    //        unpacked_function = new_function_symbol_unpacked(
    //                current_function,
    //                outline_name + "_unpack",
    //                outline_info,
    //                symbol_map);
    //    }
    //    else
    //    {
    //        unpacked_function = new_function_symbol_unpacked(
    //                current_function,
    //                outline_name + "_unpacked",
    //                outline_info,
    //                symbol_map);
    //    }

    //    outline_info.set_unpacked_function_symbol(unpacked_function);

    //    ObjectList<std::string> structure_name;
    //    structure_name.append("args");
    //    ObjectList<TL::Type> structure_type;
    //    structure_type.append(
    //            TL::Type(get_user_defined_type( arguments_struct.get_internal_symbol())).get_lvalue_reference_to()
    //            );

    //    TL::Symbol outline_function = new_function_symbol(
    //            current_function,
    //            outline_name,
    //            TL::Type::get_void_type(),
    //            structure_name,
    //            structure_type);

    //    if (IS_FORTRAN_LANGUAGE
    //            && current_function.is_in_module())
    //    {
    //        scope_entry_t* module_sym = current_function.in_module().get_internal_symbol();

    //        unpacked_function.get_internal_symbol()->entity_specs.in_module = module_sym;
    //        P_LIST_ADD(
    //                module_sym->entity_specs.related_symbols,
    //                module_sym->entity_specs.num_related_symbols,
    //                unpacked_function.get_internal_symbol());

    //        unpacked_function.get_internal_symbol()->entity_specs.is_module_procedure = 1;

    //        outline_function.get_internal_symbol()->entity_specs.in_module = module_sym;
    //        P_LIST_ADD(
    //                module_sym->entity_specs.related_symbols,
    //                module_sym->entity_specs.num_related_symbols,
    //                outline_function.get_internal_symbol());
    //        outline_function.get_internal_symbol()->entity_specs.is_module_procedure = 1;
    //    }

    //    Nodecl::NodeclBase unpacked_function_code, unpacked_function_body;
    //    build_empty_body_for_function(unpacked_function, 
    //            unpacked_function_code,
    //            unpacked_function_body);

    //    if (IS_FORTRAN_LANGUAGE)
    //    {
    //        // Copy FUNCTIONs and other local stuff
    //        symbol_map = new Nodecl::Utils::FortranProgramUnitSymbolMap(symbol_map,
    //                current_function,
    //                unpacked_function);

    //        // Replicate internal functions
    //        Nodecl::FunctionCode function_code = current_function.get_function_code().as<Nodecl::FunctionCode>();
    //        Nodecl::NodeclBase internal_functions = function_code.get_internal_functions();

    //        unpacked_function_code.as<Nodecl::FunctionCode>().set_internal_functions(
    //                Nodecl::Utils::deep_copy(internal_functions, unpacked_function.get_related_scope(), *symbol_map));
    //    }

    //    Nodecl::Utils::append_to_top_level_nodecl(unpacked_function_code);

    //    Source unpacked_source;
    //    if (!IS_FORTRAN_LANGUAGE)
    //    {
    //        unpacked_source
    //            << "{";
    //    }
    //    unpacked_source
    //        << extra_declarations
    //        << private_entities
    //        << statement_placeholder(outline_placeholder)
    //        ;
    //    if (!IS_FORTRAN_LANGUAGE)
    //    {
    //        unpacked_source
    //            << "}";
    //    }

    //    // Fortran may require more symbols
    //    if (IS_FORTRAN_LANGUAGE)
    //    {
    //        FortranExtraDeclsVisitor fun_visitor;
    //        fun_visitor.walk(original_statements);

    //        extra_declarations
    //            << "IMPLICIT NONE\n";

    //        // Insert extra symbols
    //        TL::ReferenceScope ref_scope(unpacked_function_body);
    //        decl_context_t decl_context = ref_scope.get_scope().get_decl_context();

    //        for (ObjectList<Symbol>::iterator it = fun_visitor.extra_decl_sym.begin();
    //                it != fun_visitor.extra_decl_sym.end();
    //                it++)
    //        {
    //            // Insert the name in the context...
    //            TL::Scope sc = ref_scope.get_scope();
    //            ::insert_entry(decl_context.current_scope, it->get_internal_symbol());
    //        }

    //        // Copy USEd information
    //        scope_entry_t* original_used_modules_info
    //            = original_statements.retrieve_context().get_related_symbol().get_used_modules().get_internal_symbol();
    //        if (original_used_modules_info != NULL)
    //        {
    //            scope_entry_t* new_used_modules_info
    //                = get_or_create_used_modules_symbol_info(decl_context);
    //            int i;
    //            for (i = 0 ; i< original_used_modules_info->entity_specs.num_related_symbols; i++)
    //            {
    //                P_LIST_ADD(new_used_modules_info->entity_specs.related_symbols,
    //                        new_used_modules_info->entity_specs.num_related_symbols,
    //                        original_used_modules_info->entity_specs.related_symbols[i]);
    //            }
    //        }
    //    }
    //    else if (IS_CXX_LANGUAGE)
    //    {
    //        if (!unpacked_function.is_member())
    //        {
    //            Nodecl::NodeclBase nodecl_decl = Nodecl::CxxDecl::make(
    //                    /* optative context */ nodecl_null(),
    //                    unpacked_function,
    //                    original_statements.get_filename(),
    //                    original_statements.get_line());
    //            Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, nodecl_decl);
    //        }
    //    }

    //    Nodecl::NodeclBase new_unpacked_body = unpacked_source.parse_statement(unpacked_function_body);
    //    unpacked_function_body.replace(new_unpacked_body);

    //    Nodecl::NodeclBase outline_function_code, outline_function_body;
    //    build_empty_body_for_function(outline_function,
    //            outline_function_code,
    //            outline_function_body);
    //    Nodecl::Utils::append_to_top_level_nodecl(outline_function_code);

    //    Source outline_src,
    //           instrument_before,
    //           instrument_after;

    //    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    //    {
    //        outline_src
    //            << "{"
    //            <<      instrument_before
    //            <<      outline_name << "_unpacked(" << unpacked_arguments << ");"
    //            <<      cleanup_code
    //            <<      instrument_after
    //            << "}"
    //            ;

    //        if (IS_CXX_LANGUAGE)
    //        {
    //            if (!outline_function.is_member())
    //            {
    //                Nodecl::NodeclBase nodecl_decl = Nodecl::CxxDecl::make(
    //                        /* optative context */ nodecl_null(),
    //                        outline_function,
    //                        original_statements.get_filename(),
    //                        original_statements.get_line());
    //                Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, nodecl_decl);
    //            }
    //        }
    //    }
    //    else if (IS_FORTRAN_LANGUAGE)
    //    {
    //        Source outline_function_addr;

    //        outline_src
    //            << instrument_before << "\n"
    //            << "CALL " << outline_name << "_forward(" << outline_function_addr << unpacked_arguments << ")\n"
    //            << instrument_after << "\n"
    //            ;

    //        outline_function_addr << "LOC(" << unpacked_function.get_name() << ")";

    //        if (!unpacked_arguments.empty())
    //        {
    //            outline_function_addr << ", ";
    //        }

    //        TL::ReferenceScope ref_scope(outline_function_body);
    //        decl_context_t decl_context = ref_scope.get_scope().get_decl_context();

    //        // Copy USEd information
    //        scope_entry_t* original_used_modules_info
    //            = original_statements.retrieve_context().get_related_symbol().get_used_modules().get_internal_symbol();

    //        if (original_used_modules_info != NULL)
    //        {
    //            scope_entry_t* new_used_modules_info
    //                = get_or_create_used_modules_symbol_info(decl_context);
    //            int i;
    //            for (i = 0 ; i< original_used_modules_info->entity_specs.num_related_symbols; i++)
    //            {
    //                P_LIST_ADD(new_used_modules_info->entity_specs.related_symbols,
    //                        new_used_modules_info->entity_specs.num_related_symbols,
    //                        original_used_modules_info->entity_specs.related_symbols[i]);
    //            }
    //        }

    //        // Generate ancillary code in C
    //        add_forward_code_to_extra_c_code(outline_name, data_items, outline_placeholder);
    //    }
    //    else
    //    {
    //        internal_error("Code unreachable", 0);
    //    }

    //    if (instrumentation_enabled())
    //    {
    //        Source uf_name_id, uf_name_descr,
    //               uf_location_id, uf_location_descr,
    //               instrument_before_c, instrument_after_c;

    //        instrument_before_c
    //            << "static int nanos_funct_id_init = 0;"
    //            << "static nanos_event_key_t nanos_instr_uf_name_key = 0;"
    //            << "static nanos_event_value_t nanos_instr_uf_name_value = 0;"
    //            << "static nanos_event_key_t nanos_instr_uf_location_key = 0;"
    //            << "static nanos_event_value_t nanos_instr_uf_location_value = 0;"
    //            << "nanos_err_t err; "
    //            << "if (nanos_funct_id_init == 0)"
    //            << "{"
    //            <<    "err = nanos_instrument_get_key(\"user-funct-name\", &nanos_instr_uf_name_key);"
    //            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
    //            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_name_value, \"user-funct-name\", "
    //            <<               uf_name_id << "," << uf_name_descr << ", 0);"
    //            <<    "if (err != NANOS_OK) nanos_handle_error(err);"

    //            <<    "err = nanos_instrument_get_key(\"user-funct-location\", &nanos_instr_uf_location_key);"
    //            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
    //            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_location_value, \"user-funct-location\","
    //            <<               uf_location_id << "," << uf_location_descr << ", 0);"
    //            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
    //            <<    "nanos_funct_id_init = 1;"
    //            << "}"
    //            << "nanos_event_t events_before[2];"
    //            << "events_before[0].type = NANOS_BURST_START;"
    //            << "events_before[0].key = nanos_instr_uf_name_key;"
    //            << "events_before[0].value = nanos_instr_uf_name_value;"
    //            << "events_before[1].type = NANOS_BURST_START;"
    //            << "events_before[1].key = nanos_instr_uf_location_key;"
    //            << "events_before[1].value = nanos_instr_uf_location_value;"
    //            << "err = nanos_instrument_events(2, events_before);"
    //            << "if (err != NANOS_OK) nanos_handle_error(err);"
    //            ;

    //        instrument_after_c
    //            << "nanos_event_t events_after[2];"
    //            << "events_after[0].type = NANOS_BURST_END;"
    //            << "events_after[0].key = nanos_instr_uf_name_key;"
    //            << "events_after[0].value = nanos_instr_uf_name_value;"
    //            << "events_after[1].type = NANOS_BURST_END;"
    //            << "events_after[1].key = nanos_instr_uf_location_key;"
    //            << "events_after[1].value = nanos_instr_uf_location_value;"
    //            << "err = nanos_instrument_events(2, events_after);"
    //            << "if (err != NANOS_OK) nanos_handle_error(err);"
    //            ;


    //        uf_name_id << uf_location_id;
    //        uf_location_id << "\"" << outline_name << ":" << original_statements.get_locus() << "\"";

    //        uf_name_descr << uf_location_descr;
    //        uf_location_descr
    //            << "\"Outline from '"
    //            << original_statements.get_locus()
    //            << "' in '" << outline_function.get_qualified_name() << "'\"";


    //        if (IS_FORTRAN_LANGUAGE)
    //            Source::source_language = SourceLanguage::C;

    //        Nodecl::NodeclBase instr_before = instrument_before_c.parse_statement(outline_function_body);
    //        Nodecl::NodeclBase instr_after = instrument_after_c.parse_statement(outline_function_body);

    //        if (IS_FORTRAN_LANGUAGE)
    //            Source::source_language = SourceLanguage::Current;

    //        instrument_before << as_statement(instr_before);
    //        instrument_after << as_statement(instr_after);
    //    }

    //    Nodecl::NodeclBase new_outline_body = outline_src.parse_statement(outline_function_body);
    //    outline_function_body.replace(new_outline_body);
    }


    DeviceFPGA::DeviceFPGA()
        : DeviceProvider(/* device_name */ std::string("smp"))
    {
        set_phase_name("Nanox SMP support");
        set_phase_description("This phase is used by Nanox phases to implement SMP device support");
    }

    void DeviceFPGA::pre_run(DTO& dto)
    {
    }

    void DeviceFPGA::run(DTO& dto)
    {
        DeviceProvider::run(dto);
    }

    void DeviceFPGA::get_device_descriptor(DeviceDescriptorInfo& info,
            Source &ancillary_device_description,
            Source &device_descriptor,
            Source &fortran_dynamic_init)
    {
    //    std::string outline_name = smp_outline_name(info._outline_name);
    //    if (!IS_FORTRAN_LANGUAGE)
    //    {
    //        ancillary_device_description
    //            << "static nanos_smp_args_t " << outline_name << "_args = {"
    //            << ".outline = (void(*)(void*))&" << outline_name
    //            << "};"
    //            ;
    //        device_descriptor
    //            << "{"
    //            << /* factory */ "&nanos_smp_factory, &" << outline_name << "_args"
    //            << "}"
    //            ;
    //    }
    //    else
    //    {
    //        ancillary_device_description
    //            << "static nanos_smp_args_t " << outline_name << "_args;"
    //            ;

    //        device_descriptor
    //            << "{"
    //            // factory, arg
    //            << "0, 0"
    //            << "}"
    //            ;

    //        fortran_dynamic_init
    //            << outline_name << "_args.outline = (void(*)(void*))&" << outline_name << ";"
    //            << "nanos_wd_const_data.devices[0].factory = &nanos_smp_factory;"
    //            << "nanos_wd_const_data.devices[0].arg = &" << outline_name << "_args;"
    //            ;
    //    }
    }

    void DeviceFPGA::phase_cleanup(DTO& data_flow)
    {
    }

} }

EXPORT_PHASE(TL::Nanox::DeviceFPGA);
