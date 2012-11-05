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

#include "tl-nanos.hpp"



namespace TL { namespace Nanox {

    void DeviceFPGA::create_outline(CreateOutlineInfo& info,
            Nodecl::NodeclBase& outline_placeholder,
            Nodecl::Utils::SymbolMap* &symbol_map)
    {

        if (IS_FORTRAN_LANGUAGE)
            running_error("Fortran for FPGA devices is not supported yet\n", 0);

        // Unpack DTO
        const std::string& device_outline_name = fpga_outline_name(info._outline_name);
        OutlineInfo& outline_info = info._outline_info;
        Nodecl::NodeclBase& original_statements = info._original_statements;
        TL::Symbol& arguments_struct = info._arguments_struct;
        TL::Symbol& called_task = info._called_task;

        TL::Symbol current_function =
            original_statements.retrieve_context().get_decl_context().current_scope->related_entry;

        if (current_function.is_nested_function())
        {
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                running_error("%s: error: nested functions are not supported\n",
                        original_statements.get_locus().c_str());
        }

        Source unpacked_arguments, private_entities;

        TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            switch ((*it)->get_sharing())
            {
                case OutlineDataItem::SHARING_PRIVATE:
                    {
                        break;
                    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_CAPTURE:
                case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                //case OutlineDataItem::SHARING_SHARED_PRIVATE:
                //case OutlineDataItem::SHARING_SHARED_CAPTURED_PRIVATE:
                    {
                        TL::Type param_type = (*it)->get_in_outline_type();

                        Source argument;
                        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                        {
                            // Normal shared items are passed by reference from a pointer,
                            // derreference here
                            if (((*it)->get_sharing() == OutlineDataItem::SHARING_SHARED
                                        //|| (*it)->get_sharing() == OutlineDataItem::SHARING_SHARED_CAPTURED_PRIVATE
                                        //|| (*it)->get_sharing() == OutlineDataItem::SHARING_SHARED_PRIVATE
                                        )
                                    && !(IS_CXX_LANGUAGE && (*it)->get_symbol().get_name() == "this"))
                            {
                                argument << "*(args." << (*it)->get_field_name() << ")";
                            }
                            // Any other thing is passed by value
                            else
                            {
                                argument << "args." << (*it)->get_field_name();
                            }

                            if (IS_CXX_LANGUAGE
                                    && (*it)->get_allocation_policy() == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY)
                            {
                                internal_error("Not yet implemented: call the destructor", 0);
                            }
                        }
                        else
                        {
                            internal_error("running error fortran not supported", 0);
                        }

                       // if  ((*it)->get_sharing() == OutlineDataItem::SHARING_SHARED_CAPTURED_PRIVATE)
                       // {
                       //     std::string name = (*it)->get_symbol().get_name();

                       //     private_entities
                       //         << "p_" << name << " = " << name << ";"
                       //         ;
                       // }

                        unpacked_arguments.append_with_separator(argument, ", ");
                        break;
                    }
                case OutlineDataItem::SHARING_REDUCTION:
                    {
                    //    // Pass the original reduced variable as if it were a shared
                    //    Source argument;
                    //    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    //    {
                    //        argument << "*(args." << (*it)->get_field_name() << ")";
                    //    }
                    //    else
                    //    {
                    //        internal_error("running error", 0);
                    //    }
                    //    unpacked_arguments.append_with_separator(argument, ", ");

                    //    std::string name = (*it)->get_symbol().get_name();

                    //    private_entities
                    //        << "rdp_" << name << " = " << as_expression( (*it)->get_reduction_info()->get_identity()) << ";"
                    //        ;
                        internal_error("Reductions are not supported yet", 0);
                        break;
                    }
                default:
                    {
                        internal_error("Unexpected data sharing kind", 0);
                    }
            }
        }

        TL::Symbol unpacked_function = new_function_symbol_unpacked(
                current_function,
                device_outline_name + "_unpacked",
                outline_info,
                symbol_map);

        Nodecl::NodeclBase unpacked_function_code, unpacked_function_body;
        build_empty_body_for_function(unpacked_function,
                unpacked_function_code,
                unpacked_function_body);

        Source unpacked_source;
        unpacked_source
            << "{"
            << private_entities
            << statement_placeholder(outline_placeholder)
            << "}"
            ;

        // Creating the symbol related to the outline function
        //The outline function has always only one parameter which name is 'args'
        ObjectList<std::string> structure_name;
        structure_name.append("args");

        //The type of this parameter is an struct (i. e. user defined type)
        ObjectList<TL::Type> structure_type;
        structure_type.append(TL::Type(
                    get_user_defined_type(
                        arguments_struct.get_internal_symbol())).get_lvalue_reference_to());

        TL::Symbol outline_function = new_function_symbol(
                current_function,
                device_outline_name,
                TL::Type::get_void_type(),
                structure_name,
                structure_type);

        // The outline function must not be static because this function is called
        // from mnvcc_filename.c and it is defined in cudacc_filename.cu
        outline_function.get_internal_symbol()->entity_specs.is_static = 0;

        Nodecl::NodeclBase outline_function_code, outline_function_body;
        build_empty_body_for_function(outline_function,
                outline_function_code,
                outline_function_body);

        Source outline_src,
               instrument_before,
               instrument_after;
        outline_src
            << "{"
            <<      instrument_before
            <<      device_outline_name << "_unpacked(" << unpacked_arguments << ");"
            <<      instrument_after
            << "}"
            ;

        Nodecl::NodeclBase new_unpacked_body =
            unpacked_source.parse_statement(unpacked_function_body);
        unpacked_function_body.replace(new_unpacked_body);

        // Add the unpacked function to the cuda file
        _fpga_file_code.push_back(unpacked_function_code);

        if (IS_CXX_LANGUAGE)
        {
            if (!outline_function.is_member())
            {
                Nodecl::NodeclBase nodecl_decl = Nodecl::CxxDecl::make(
                        /* optative context */ nodecl_null(),
                        outline_function,
                        original_statements.get_filename(),
                        original_statements.get_line());
                Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, nodecl_decl);
            }
        }
        Nodecl::NodeclBase new_outline_body = outline_src.parse_statement(outline_function_body);
        outline_function_body.replace(new_outline_body);

        // Add the outline function to the cuda file
        _fpga_file_code.push_back(outline_function_code);

        if (called_task.is_valid())
        {
            _fpga_file_code.push_back(Nodecl::Utils::deep_copy(called_task.get_function_code(), called_task.get_scope()));
        }

    }


    DeviceFPGA::DeviceFPGA()
        : DeviceProvider(std::string("fpga"))
    {
        set_phase_name("Nanox FPGA support");
        set_phase_description("This phase is used by Nanox phases to implement FPGA device support");
    }

    void DeviceFPGA::pre_run(DTO& dto)
    {
    }

    void DeviceFPGA::run(DTO& dto)
    {
        DeviceProvider::run(dto);
    }

    std::string DeviceFPGA::fpga_outline_name(const std::string &name)
    {
        return "_fpga_" + name;
    }


    void DeviceFPGA::get_device_descriptor(DeviceDescriptorInfo& info,
            Source &ancillary_device_description,
            Source &device_descriptor,
            Source &fortran_dynamic_init)
    {

    std::string outline_name = info._outline_name;
    Source device_outline_name;

    device_outline_name << fpga_outline_name(outline_name);
//*
    if (Nanos::Version::interface_is_at_least("master", 5012))
    {
        ancillary_device_description
            << comment("FPGA device descriptor")
            << "static nanos_smp_args_t "
            << outline_name << "_args = { (void(*)(void*))" << device_outline_name << "};"
            ;
    }
    else
    {
        internal_error("Unsupported Nanos version.", 0);
    }
//*/
    device_descriptor
        << "{ nanos_fpga_factory,  &" << outline_name << "_args },";
        ;

    }

    //write/close intermediate files, free temporal nodes, etc.
    void DeviceFPGA::phase_cleanup(DTO& data_flow)
    {
    }

    TL::Symbol DeviceFPGA::new_function_symbol(
                TL::Symbol current_function,
            const std::string& name,
            TL::Type return_type,
            ObjectList<std::string> parameter_names,
            ObjectList<TL::Type> parameter_types)
    {
        Scope sc = current_function.get_scope();

        // FIXME - Wrap
        decl_context_t decl_context = sc.get_decl_context();

        scope_entry_t* entry = new_symbol(decl_context, decl_context.current_scope, name.c_str());
        entry->entity_specs.is_user_declared = 1;

        entry->kind = SK_FUNCTION;
        entry->file = "";
        entry->line = 0;

        // Make it static
        entry->entity_specs.is_static = 1;

        // Make it member if the enclosing function is member
        if (current_function.is_member())
        {
            entry->entity_specs.is_member = 1;
            entry->entity_specs.class_type = current_function.get_class_type().get_internal_type();

            entry->entity_specs.access = AS_PUBLIC;

            ::class_type_add_member(entry->entity_specs.class_type, entry);
        }

        ERROR_CONDITION(parameter_names.size() != parameter_types.size(), "Mismatch between names and types", 0);

        decl_context_t function_context ;
        function_context = new_function_context(decl_context);
        function_context = new_block_context(function_context);

        function_context.function_scope->related_entry = entry;
        function_context.block_scope->related_entry = entry;

        entry->related_decl_context = function_context;

        parameter_info_t* p_types = new parameter_info_t[parameter_types.size()];

        parameter_info_t* it_ptypes = &(p_types[0]);
        ObjectList<TL::Type>::iterator type_it = parameter_types.begin();
        for (ObjectList<std::string>::iterator it = parameter_names.begin();
                it != parameter_names.end();
                it++, it_ptypes++, type_it++)
        {
            scope_entry_t* param = new_symbol(function_context, function_context.current_scope, it->c_str());
            param->entity_specs.is_user_declared = 1;
            param->kind = SK_VARIABLE;
            param->file = "";
            param->line = 0;

            param->defined = 1;

            symbol_set_as_parameter_of_function(param, entry, entry->entity_specs.num_related_symbols);

            param->type_information = get_unqualified_type(type_it->get_internal_type());

            P_LIST_ADD(entry->entity_specs.related_symbols,
                    entry->entity_specs.num_related_symbols,
                    param);

            it_ptypes->is_ellipsis = 0;
            it_ptypes->nonadjusted_type_info = NULL;
            it_ptypes->type_info = get_indirect_type(param);
        }

        type_t *function_type = get_new_function_type(
                return_type.get_internal_type(),
                p_types,
                parameter_types.size());

        entry->type_information = function_type;

        delete[] p_types;

        return entry;
    }

    TL::Symbol DeviceFPGA::new_function_symbol_unpacked(
            TL::Symbol current_function,
            const std::string& function_name,
            OutlineInfo& outline_info,
            Nodecl::Utils::SymbolMap*& out_symbol_map)
    {
        Scope sc = current_function.get_scope();

        decl_context_t decl_context = sc.get_decl_context();
        decl_context_t function_context;

        function_context = new_function_context(decl_context);
        function_context = new_block_context(function_context);

        // Create all the symbols and an appropiate mapping

        Nodecl::Utils::SimpleSymbolMap *symbol_map = new Nodecl::Utils::SimpleSymbolMap();

        TL::ObjectList<TL::Symbol> parameter_symbols, private_symbols;

        TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            TL::Symbol sym = (*it)->get_symbol();

            std::string name;
            if (sym.is_valid())
            {
                name = sym.get_name();
                if (IS_CXX_LANGUAGE
                        && name == "this")
                {
                    name = "this_";
                }
            }
            else
            {
                name = (*it)->get_field_name();
            }

            bool already_mapped = false;

            switch ((*it)->get_sharing())
            {
                case OutlineDataItem::SHARING_PRIVATE:
                    {
                        scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope, name.c_str());
                        private_sym->kind = SK_VARIABLE;
                        private_sym->type_information = (*it)->get_in_outline_type().get_internal_type();
                        private_sym->defined = private_sym->entity_specs.is_user_declared = 1;

                        if (sym.is_valid())
                        {
                            symbol_map->add_map(sym, private_sym);

                            // Copy attributes that must be preserved
                            private_sym->entity_specs.is_allocatable = !sym.is_member() && sym.is_allocatable();
                        }

                        private_symbols.append(private_sym);
                        break;
                    }
                //case OutlineDataItem::SHARING_SHARED_PRIVATE:
                //case OutlineDataItem::SHARING_SHARED_CAPTURED_PRIVATE:
                //    {
                //        scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope, 
                //                ("p_" + name).c_str());
                //        private_sym->kind = SK_VARIABLE;
                //        private_sym->type_information = (*it)->get_private_type().get_internal_type();
                //        private_sym->defined = private_sym->entity_specs.is_user_declared = 1;

                //        if (sym.is_valid())
                //        {
                //            symbol_map->add_map(sym, private_sym);

                //            // Copy attributes that must be preserved
                //            private_sym->entity_specs.is_allocatable = !sym.is_member() && sym.is_allocatable();

                //            // We do not want it be mapped again
                //            // in the fall-through branch
                //            already_mapped = true;
                //        }

                //        private_symbols.append(private_sym);

                //        /* FALL THROUGH */
                //    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_CAPTURE:
                case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                    {
                        scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope,
                                name.c_str());
                        private_sym->kind = SK_VARIABLE;
                        private_sym->type_information = (*it)->get_in_outline_type().get_internal_type();
                        private_sym->defined = private_sym->entity_specs.is_user_declared = 1;


                        if (sym.is_valid())
                        {
                            private_sym->entity_specs.is_optional = sym.is_optional();
                            private_sym->entity_specs.is_allocatable =
                                !sym.is_member() && sym.is_allocatable();
                            if (!already_mapped)
                            {
                                symbol_map->add_map(sym, private_sym);
                            }
                        }

                        private_sym->entity_specs.is_allocatable = 
                            sym.is_allocatable() ||
                            (((*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE) 
                             == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE);

                        parameter_symbols.append(private_sym);

                        break;
                    }
                case OutlineDataItem::SHARING_REDUCTION:
                    {
                        // Original reduced variable. Passed as we pass shared parameters
                        TL::Type param_type = (*it)->get_in_outline_type();
                        scope_entry_t* shared_reduction_sym = ::new_symbol(function_context, function_context.current_scope,
                                (*it)->get_field_name().c_str());
                        shared_reduction_sym->kind = SK_VARIABLE;
                        shared_reduction_sym->type_information = param_type.get_internal_type();
                        shared_reduction_sym->defined = shared_reduction_sym->entity_specs.is_user_declared = 1;
                        parameter_symbols.append(shared_reduction_sym);

                        shared_reduction_sym->entity_specs.is_allocatable = sym.is_valid()
                            && !sym.is_member()
                            && sym.is_allocatable();

                        // Private vector of partial reductions. This is a local pointer variable
                        // rdv stands for reduction vector
                        TL::Type private_reduction_vector_type = (*it)->get_private_type();
                        if (IS_C_LANGUAGE
                                || IS_CXX_LANGUAGE)
                        {
                            // T*
                            private_reduction_vector_type = private_reduction_vector_type.get_pointer_to();
                        }
                        else
                        {
                            internal_error("Code unreachable", 0);
                        }

                        scope_entry_t* private_reduction_vector_sym = ::new_symbol(function_context, function_context.current_scope,
                                ("rdv_" + name).c_str());
                        private_reduction_vector_sym->kind = SK_VARIABLE;
                        private_reduction_vector_sym->type_information = private_reduction_vector_type.get_internal_type();
                        private_reduction_vector_sym->defined = private_reduction_vector_sym->entity_specs.is_user_declared = 1;

                        // Local variable (rdp stands for reduction private)
                        // This variable must be initialized properly
                        scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope,
                                ("rdp_" + name).c_str());
                        private_sym->kind = SK_VARIABLE;
                        private_sym->type_information = (*it)->get_private_type().get_internal_type();
                        private_sym->defined = private_sym->entity_specs.is_user_declared = 1;

                        if (sym.is_valid())
                        {
                            symbol_map->add_map(sym, private_sym);
                        }

                        break;
                    }
                default:
                    {
                        internal_error("Unexpected data sharing kind", 0);
                    }
            }
        }

        // Update types of parameters (this is needed by VLAs)
        for (TL::ObjectList<TL::Symbol>::iterator it = parameter_symbols.begin();
                it != parameter_symbols.end();
                it++)
        {
            it->get_internal_symbol()->type_information =
                type_deep_copy(it->get_internal_symbol()->type_information,
                        function_context,
                        symbol_map->get_symbol_map());
        }
        // Update types of privates (this is needed by VLAs)
        for (TL::ObjectList<TL::Symbol>::iterator it = private_symbols.begin();
                it != private_symbols.end();
                it++)
        {
            it->get_internal_symbol()->type_information =
                type_deep_copy(it->get_internal_symbol()->type_information,
                        function_context,
                        symbol_map->get_symbol_map());
        }

        // Now everything is set to register the function
        scope_entry_t* new_function_sym = new_symbol(decl_context, decl_context.current_scope, function_name.c_str());
        new_function_sym->entity_specs.is_user_declared = 1;

        new_function_sym->kind = SK_FUNCTION;
        new_function_sym->file = "";
        new_function_sym->line = 0;

        // Make it static
        new_function_sym->entity_specs.is_static = 1;

        // Make it member if the enclosing function is member
        if (current_function.is_member())
        {
            new_function_sym->entity_specs.is_member = 1;
            new_function_sym->entity_specs.class_type = current_function.get_class_type().get_internal_type();

            new_function_sym->entity_specs.access = AS_PUBLIC;

            ::class_type_add_member(new_function_sym->entity_specs.class_type,
                    new_function_sym);
        }

        function_context.function_scope->related_entry = new_function_sym;
        function_context.block_scope->related_entry = new_function_sym;

        new_function_sym->related_decl_context = function_context;

        parameter_info_t* p_types = new parameter_info_t[parameter_symbols.size()];

        parameter_info_t* it_ptypes = &(p_types[0]);
        for (ObjectList<TL::Symbol>::iterator it = parameter_symbols.begin();
                it != parameter_symbols.end();
                it++, it_ptypes++)
        {
            scope_entry_t* param = it->get_internal_symbol();

            symbol_set_as_parameter_of_function(param, new_function_sym, new_function_sym->entity_specs.num_related_symbols);

            P_LIST_ADD(new_function_sym->entity_specs.related_symbols,
                    new_function_sym->entity_specs.num_related_symbols,
                    param);

            it_ptypes->is_ellipsis = 0;
            it_ptypes->nonadjusted_type_info = NULL;

            // FIXME - We should do all the remaining lvalue adjustments
            type_t* param_type = get_unqualified_type(param->type_information);
            it_ptypes->type_info = param_type;
        }

        type_t *function_type = get_new_function_type(
                get_void_type(),
                p_types,
                parameter_symbols.size());

        new_function_sym->type_information = function_type;

        delete[] p_types;

        out_symbol_map = symbol_map;
        return new_function_sym;
    }

    void DeviceFPGA::build_empty_body_for_function(
            TL::Symbol function_symbol,
            Nodecl::NodeclBase &function_code,
            Nodecl::NodeclBase &empty_stmt
            )
    {
        empty_stmt = Nodecl::EmptyStatement::make("", 0);
        Nodecl::List stmt_list = Nodecl::List::make(empty_stmt);

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            Nodecl::CompoundStatement compound_statement =
                Nodecl::CompoundStatement::make(stmt_list,
                        /* destructors */ Nodecl::NodeclBase::null(),
                        "", 0);
            stmt_list = Nodecl::List::make(compound_statement);
        }

        Nodecl::NodeclBase context = Nodecl::Context::make(
                stmt_list,
                function_symbol.get_related_scope(), "", 0);

        function_symbol.get_internal_symbol()->defined = 1;

        function_code = Nodecl::FunctionCode::make(context,
                // Initializers
                Nodecl::NodeclBase::null(),
                // Internal functions
                Nodecl::NodeclBase::null(),
                function_symbol,
                "", 0);
    }

} }

EXPORT_PHASE(TL::Nanox::DeviceFPGA);
