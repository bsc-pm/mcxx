/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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
#include "tl-nodecl-utils-fortran.hpp"

#include "fortran03-scope.h"
#include "fortran03-typeutils.h"
#include "fortran03-buildscope.h"

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
        _nanox_devices[strtolower(str.c_str())] = nanox_device_provider;
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
        nanox_devices_map_t::iterator it = _nanox_devices.find(strtolower(str.c_str()));

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

    void DeviceProvider::common_constructor_code()
    {
        register_parameter("instrument",
                "Enables instrumentation of the device provider if set to '1'",
                _enable_instrumentation_str,
                "0").connect(std::bind(&DeviceProvider::set_instrumentation, this, std::placeholders::_1));
    }

    void DeviceProvider::get_instrumentation_code(
            const TL::Symbol& called_task,
            const TL::Symbol& outline_function,
            Nodecl::NodeclBase outline_function_body,
            Nodecl::NodeclBase task_label,
            const locus_t* locus,
            Source& instrumentation_before,
            Source& instrumentation_after)
    {
        if (Nanos::Version::interface_is_at_least("master", 5019)
                || Nanos::Version::interface_is_at_least("instrumentation_api", 1001))
        {
            Source val, extended_descr, extra_cast, instrument_before_c,
            instrument_after_c, function_name_instr, val_type;

            // In some cases, the outline_function name is the same for two different tasks.
            // For this reason we add also the filename and the line
            val << outline_function.get_name()
                << "@" << locus_get_filename(locus)
                << "@" << locus_get_line(locus)
                << "@" << val_type;

            std::string function_name;
            if (task_label.is_null())
            {
                if (called_task.is_valid())
                {
                    // It's a function task
                    function_name =
                        called_task.get_type().get_declaration(
                                called_task.get_scope(), called_task.get_qualified_name());
                }
                else
                {
                    // It's an inline task
                    function_name =
                        outline_function.get_type().fix_references().get_declaration(
                                outline_function.get_scope(), outline_function.get_qualified_name());
                }

                extended_descr << function_name;
                val_type << "FUNCTION";
            }
            else
            {
                extended_descr = task_label.get_text();
                val_type << "LABEL";
            }

            // The description should contains:
            //  - FUNC_DECL: The declaration of the function. The function name shall be qualified
            //  - FILE: The filename
            //  - LINE: The line number
            //  We use '@' as a separator of fields: FUNC_DECL @ FILE @ LINE
            extended_descr << "@" << locus_get_filename(locus) << "@" << locus_get_line(locus) << "@" << val_type;

            // GCC complains if you convert a pointer to an integer of different
            // size. Since we target an unsigned long long, in architectures of 32
            // bits we first cast to an unsigned int
            if (CURRENT_CONFIGURATION->type_environment->sizeof_function_pointer == 4)
            {
                extra_cast << "(unsigned int)";
            }

            // FIXME: We may need an additional cast here (GCC bug solved in 4.5)
            function_name_instr << "(void (*)(void*))"
				<< "(" << as_type(outline_function.get_type().get_pointer_to()) << " )"
				<< outline_function.get_qualified_name();

            instrument_before_c
                << "static int nanos_funct_id_init = 0;"
                << "static nanos_event_key_t nanos_instr_uf_location_key = 0;"
                << "nanos_err_t nanos_err;"
                << "if (nanos_funct_id_init == 0)"
                << "{"
                <<    "nanos_err = nanos_instrument_get_key(\"user-funct-location\", &nanos_instr_uf_location_key);"
                <<    "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
                <<    "nanos_err = nanos_instrument_register_value_with_val("
                <<          "(nanos_event_value_t) " << extra_cast << function_name_instr << ","
                <<          "\"user-funct-location\","
                <<          "\"" << val << "\","
                <<          "\"" << extended_descr << "\","
                <<          /* abort_when_registered */ "0);"

                <<    "if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);"
                <<    "nanos_funct_id_init = 1;"
                << "}"
                ;

            generate_outline_events_before(function_name_instr, extra_cast, instrument_before_c);
            generate_outline_events_after(function_name_instr, extra_cast, instrument_after_c);

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

    void DeviceProvider::generate_outline_events_before(
            Source& function_name_instr,
            Source& extra_cast,
            Source& instrumentation_before)
    {
        instrumentation_before
            << "nanos_event_t event;"
            << "event.type = NANOS_BURST_START;"
            << "event.key = nanos_instr_uf_location_key;"
            << "event.value = (nanos_event_value_t) " << extra_cast << function_name_instr << ";"
            << "nanos_err = nanos_instrument_events(1, &event);"
            ;
    }

    void DeviceProvider::generate_outline_events_after(
            Source& function_name_instr,
            Source& extra_cast,
            Source& instrumentation_after)
    {
        instrumentation_after
            << "event.type = NANOS_BURST_END;"
            << "event.key = nanos_instr_uf_location_key;"
            << "event.value = (nanos_event_value_t) " << extra_cast << function_name_instr << ";"
            << "nanos_err = nanos_instrument_events(1, &event);"
            ;
    }

    // This is only for Fortran!
    TL::Symbol DeviceProvider::new_function_symbol_forward(
            TL::Symbol current_function,
            const std::string& function_name,
            CreateOutlineInfo& info)
    {
        if (IS_FORTRAN_LANGUAGE && current_function.is_nested_function())
        {
            // Get the enclosing function
            current_function = current_function.get_scope().get_related_symbol();
        }

        // This is only for Fortran!
        Scope sc = current_function.get_scope();

        const decl_context_t* decl_context = sc.get_decl_context();
        const decl_context_t* function_context = new_program_unit_context(decl_context);

        TL::ObjectList<TL::Symbol> parameter_symbols, private_symbols;

        // Pointer to the real unpack function
        scope_entry_t* ptr_to_outline = ::new_symbol(function_context, function_context->current_scope,
                UNIQUESTR_LITERAL("outline_ptr"));
        ptr_to_outline->kind = SK_VARIABLE;
        ptr_to_outline->type_information = fortran_choose_int_type_from_kind(CURRENT_CONFIGURATION->type_environment->sizeof_pointer);
        parameter_symbols.append(ptr_to_outline);

        TL::ObjectList<OutlineDataItem*> data_items = info._data_items;
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

            switch ((*it)->get_sharing())
            {
                case OutlineDataItem::SHARING_PRIVATE:
                case OutlineDataItem::SHARING_ALLOCA:
                    {
                        break;
                    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_CAPTURE:
                case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                case OutlineDataItem::SHARING_REDUCTION:
                    {
                        scope_entry_t* private_sym = ::new_symbol(function_context, function_context->current_scope,
                                uniquestr(name.c_str()));
                        private_sym->kind = SK_VARIABLE;
                        if ((*it)->get_field_type().is_pointer()
                                && (*it)->get_field_type().points_to().is_void())
                        {
                            // Preserve void*
                            private_sym->type_information = (*it)
                                ->get_field_type()
                                .get_internal_type();
                        }
                        else
                        {
                            private_sym->type_information = (*it)
                                ->get_field_type()
                                .get_lvalue_reference_to()
                                .get_internal_type();
                        }

                        symbol_entity_specs_set_is_user_declared(private_sym, 1);
                        private_sym->defined = 1;

                        symbol_entity_specs_set_is_allocatable(private_sym,
                                (((*it)->get_allocation_policy()
                                  & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE)
                                 == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE));

                        parameter_symbols.append(private_sym);
                        break;
                    }
                default:
                    {
                        internal_error("Unexpected data sharing kind", 0);
                    }
            }
        }

        // Now everything is set to register the function
        scope_entry_t* new_function_sym = new_symbol(decl_context, decl_context->current_scope, uniquestr(function_name.c_str()));
        symbol_entity_specs_set_is_user_declared(new_function_sym, 1);

        new_function_sym->kind = SK_FUNCTION;
        new_function_sym->locus = make_locus("", 0, 0);

        // Make it static
        symbol_entity_specs_set_is_static(new_function_sym, 1);

        // Make it member if the enclosing function is member
        if (current_function.is_member())
        {
            symbol_entity_specs_set_is_member(new_function_sym, 1);
            symbol_entity_specs_set_class_type(new_function_sym, current_function.get_class_type().get_internal_type());

            symbol_entity_specs_set_access(new_function_sym, AS_PUBLIC);

            ::class_type_add_member(symbol_entity_specs_get_class_type(new_function_sym),
                    new_function_sym,
                    new_function_sym->decl_context,
                    /* is_definition */ 0);
        }

        function_context->function_scope->related_entry = new_function_sym;
        function_context->block_scope->related_entry = new_function_sym;

        new_function_sym->related_decl_context = function_context;

        parameter_info_t* p_types = new parameter_info_t[parameter_symbols.size()];

        parameter_info_t* it_ptypes = &(p_types[0]);
        for (ObjectList<TL::Symbol>::iterator it = parameter_symbols.begin();
                it != parameter_symbols.end();
                it++, it_ptypes++)
        {
            scope_entry_t* param = it->get_internal_symbol();

            symbol_set_as_parameter_of_function(param, new_function_sym,
                    /* nesting */ 0,
                    /* position */ symbol_entity_specs_get_num_related_symbols(new_function_sym));

            symbol_entity_specs_add_related_symbols(new_function_sym, param);

            it_ptypes->is_ellipsis = 0;
            it_ptypes->nonadjusted_type_info = NULL;

            // FIXME - We should do all the remaining lvalue adjustments
            type_t* param_type = get_unqualified_type(param->type_information);
            it_ptypes->type_info = param_type;
        }

        type_t *function_type = get_new_function_type(
                get_void_type(),
                p_types, parameter_symbols.size(),
                REF_QUALIFIER_NONE);

        new_function_sym->type_information = function_type;

        delete[] p_types;

        return new_function_sym;
    }

    static Source emit_allocate_statement(TL::Symbol sym, int &is_allocated_index, int &lower_bound_index, int &upper_bound_index)
    {
        Source result;

        TL::Type t = sym.get_type();
        if (t.is_any_reference())
            t = t.references_to();

        struct Aux
        {
            static void aux_rec(Source &array_shape, TL::Type t_aux, int rank, int current_rank,
                    int &lower_bound_index_aux, int &upper_bound_index_aux)
            {
                Source current_arg;
                if (t_aux.is_fortran_array())
                {
                    Source curent_arg;
                    Nodecl::NodeclBase lower, upper;
                    t_aux.array_get_bounds(lower, upper);

                    if (lower.is_null())
                    {
                        current_arg << "mcc_lower_bound_" << lower_bound_index_aux << ":";
                        lower_bound_index_aux++;
                    }

                    if (upper.is_null())
                    {
                        current_arg << "mcc_upper_bound_" << upper_bound_index_aux;
                        upper_bound_index_aux++;
                    }

                    aux_rec(array_shape, t_aux.array_element(), rank-1, current_rank, lower_bound_index_aux, upper_bound_index_aux);

                    array_shape.append_with_separator(current_arg, ",");
                }
            }

            static void fill_array_shape(Source &array_shape, TL::Type t_aux, int &lower_bound_index_aux, int &upper_bound_index_aux)
            {
                aux_rec(array_shape,
                        t_aux, t_aux.get_num_dimensions(), t_aux.get_num_dimensions(),
                        lower_bound_index_aux, upper_bound_index_aux);
            }
        };

        Source array_shape;
        Aux::fill_array_shape(array_shape, t, lower_bound_index, upper_bound_index);

        result
            << "IF (mcc_is_allocated_" << is_allocated_index << ") THEN\n"
            << "   ALLOCATE(" << sym.get_name() << "(" << array_shape <<  "));\n"
            << "END IF\n"
            ;

        is_allocated_index++;

        return result;
    }

    static Source emit_allocate_statement_using_array(TL::Symbol private_sym, TL::Symbol shared_symbol)
    {
        Source result;

        TL::Type t = private_sym.get_type();
        if (t.is_any_reference())
            t = t.references_to();

        struct Aux
        {
            static void aux_rec(Source &array_shape, TL::Symbol orig_array, TL::Type t_aux, int& rank)
            {
                Source current_arg;
                if (t_aux.is_fortran_array())
                {
                    aux_rec(array_shape, orig_array, t_aux.array_element(), rank);
                    rank++;

                    Source curent_arg;
                    Nodecl::NodeclBase lower, upper;
                    t_aux.array_get_bounds(lower, upper);

                    current_arg
                        << "LBOUND(" << as_symbol(orig_array) << ", DIM=" << rank << ")"
                        << ":"
                        << "UBOUND(" << as_symbol(orig_array) << ", DIM=" << rank << ")";

                    array_shape.append_with_separator(current_arg, ",");
                }
            }

            static void fill_array_shape(Source &array_shape, TL::Symbol orig_array)
            {
                int n = 0;
                aux_rec(array_shape,
                        orig_array,
                        orig_array.get_type().no_ref(),
                        n);
            }
        };

        Source array_shape;
        Aux::fill_array_shape(array_shape, shared_symbol);

        result
            << "ALLOCATE(" << private_sym.get_name() << "(" << array_shape <<  "));\n"
            ;

        return result;
    }

    TL::Symbol DeviceProvider::new_function_symbol_unpacked(
            TL::Symbol current_function,
            const std::string& function_name,
            CreateOutlineInfo& info,
            // Out
            Nodecl::Utils::SimpleSymbolMap*& symbol_map,
            Source &initial_statements,
            Source &final_statements)
    {
        if (IS_FORTRAN_LANGUAGE && current_function.is_nested_function())
        {
            // Get the enclosing function
            current_function = current_function.get_scope().get_related_symbol();
        }

        bool is_function_task = info._called_task.is_valid();

        Scope sc = current_function.get_scope();
        const decl_context_t* decl_context = sc.get_decl_context();

        const decl_context_t* function_context;
        if (IS_FORTRAN_LANGUAGE)
        {
            function_context = new_program_unit_context(decl_context);
        }
        else
        {
            function_context = new_function_context(decl_context);
            function_context = new_block_context(function_context);
        }

        // Create all the symbols and an appropiate mapping
        TL::ObjectList<TL::Symbol> parameter_symbols,
            private_symbols,
            vla_private_symbols,
            reduction_private_symbols,
            reduction_vector_symbols;

        int lower_bound_index = 1;
        int upper_bound_index = 1;
        int is_allocated_index = 1;
        TL::ObjectList<TL::Symbol> cray_pointee_list;

        TL::ObjectList<OutlineDataItem*> data_items = info._data_items;
        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            TL::Symbol sym = (*it)->get_symbol();
            std::string name = (*it)->get_field_name();

            if (!is_function_task
                    && (*it)->get_is_cxx_this())
                continue;

            switch ((*it)->get_sharing())
            {
                case OutlineDataItem::SHARING_ALLOCA:
                    {
                        // Do nothing
                        break;
                    }
                case OutlineDataItem::SHARING_PRIVATE:
                    {
                        scope_entry_t* private_sym = ::new_symbol(function_context,
                                function_context->current_scope,
                                uniquestr(name.c_str()));

                        private_sym->kind = SK_VARIABLE;
                        private_sym->type_information = (*it)->get_in_outline_type().get_internal_type();
                        symbol_entity_specs_set_is_user_declared(private_sym, 1);
                        private_sym->defined = 1;

                        // Privates may need to be initialized with constant
                        // values
                        private_sym->value =
                            (*it)->get_captured_value().shallow_copy().get_internal_nodecl();

                        if (sym.is_valid())
                        {
                            symbol_map->add_map(sym, private_sym);

                            // Copy attributes that must be preserved
                            symbol_entity_specs_set_is_allocatable(private_sym,
                                    (!sym.is_member() && sym.is_allocatable()));

                            // Cray pointeers are handled a bit special
                            if (sym.is_cray_pointee())
                            {
                                symbol_entity_specs_set_is_cray_pointee(private_sym, 1);

                                // We cannot set the right cray_pointer symbol yet because It's possible that has not been created
                                symbol_entity_specs_set_cray_pointer(private_sym, sym.get_cray_pointer().get_internal_symbol());

                                cray_pointee_list.append(private_sym);
                            }
                        }

                        private_symbols.append(private_sym);

                        if (IS_CXX_LANGUAGE)
                        {
                            // We need the declarations of the private symbols!
                            initial_statements << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), private_sym));
                        }

                        if ((*it)->get_symbol().is_valid()
                                && (*it)->get_symbol().is_allocatable())
                        {
                            initial_statements << emit_allocate_statement((*it)->get_symbol(), is_allocated_index,
                                    lower_bound_index, upper_bound_index);
                        }
                        break;
                    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_SHARED_ALLOCA:
                case OutlineDataItem::SHARING_CAPTURE:
                case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                    {
                        scope_entry_t* private_sym = ::new_symbol(function_context, function_context->current_scope,
                                uniquestr(name.c_str()));

                        private_sym->kind = SK_VARIABLE;
                        private_sym->type_information = (*it)->get_in_outline_type().get_internal_type();
                        symbol_entity_specs_set_is_user_declared(private_sym, 1);
                        private_sym->defined = 1;

                        if (sym.is_valid())
                        {
                            symbol_entity_specs_set_is_optional(private_sym, sym.is_optional());
                            symbol_entity_specs_set_is_target(private_sym, sym.is_target());
                            symbol_entity_specs_set_is_allocatable(private_sym,
                                (!sym.is_member() && sym.is_allocatable())
                                || (*it)->is_copy_of_array_descriptor_allocatable());

                            symbol_map->add_map(sym, private_sym);
                        }



                        symbol_entity_specs_set_is_allocatable(private_sym,
                                symbol_entity_specs_get_is_allocatable(private_sym) ||
                                (((*it)->get_allocation_policy()
                                  & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE)
                                 == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE));

                        parameter_symbols.append(private_sym);

                        scope_entry_t* vla_private_sym = NULL;
                        if (IS_CXX_LANGUAGE
                                && (*it)->get_symbol().get_type().depends_on_nonconstant_values())
                        {
                            // Shape VLA
                            vla_private_sym = ::new_symbol(function_context, function_context->current_scope,
                                    uniquestr(("vla_p_" + name).c_str()));

                            vla_private_sym->kind = SK_VARIABLE;
                            vla_private_sym->type_information = (*it)->get_symbol().get_type().no_ref().get_internal_type();
                            if (is_array_type(vla_private_sym->type_information))
                            {
                                vla_private_sym->type_information = get_pointer_type(array_type_get_element_type(vla_private_sym->type_information));
                            }
                            symbol_entity_specs_set_is_user_declared(vla_private_sym, 1);
                            vla_private_sym->defined = 1;

                            initial_statements << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                                        vla_private_sym));

                            TL::Type updated_vla_type = type_deep_copy(vla_private_sym->type_information,
                                function_context,
                                symbol_map->get_symbol_map());

                            Nodecl::NodeclBase cast;
                            initial_statements << as_statement(
                                    Nodecl::ExpressionStatement::make(
                                        Nodecl::Assignment::make(
                                            TL::Symbol(vla_private_sym).make_nodecl(true),
                                            cast = Nodecl::Conversion::make(
                                                TL::Symbol(private_sym).make_nodecl(true), updated_vla_type),
                                            lvalue_ref(updated_vla_type.get_internal_type()))));
                            cast.set_text("C");

                            symbol_map->add_map(sym, vla_private_sym);
                        }

                        if (IS_CXX_LANGUAGE
                                && ((*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY) == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY)
                        {
                            if (((*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED) == OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED)
                            {
                                ERROR_CONDITION(vla_private_sym == NULL, "This should be a VLA", 0);

                                TL::Type base_type = (*it)->get_symbol().get_type();
                                TL::Type updated_vla_type = type_deep_copy((*it)->get_symbol().get_type().no_ref().get_internal_type(),
                                        function_context,
                                        symbol_map->get_symbol_map());
                                updated_vla_type = updated_vla_type.no_ref().get_pointer_to();

                                while (base_type.is_array())
                                    base_type = base_type.array_element();

                                TL::Type class_type = base_type.get_unqualified_type();
                                ERROR_CONDITION(!class_type.is_named_class(), "This should be a named class type (%s)",
                                        print_declarator(class_type.get_internal_type()));

                                base_type = base_type.get_unqualified_type().get_pointer_to();

                                final_statements
                                    << "{"
                                    <<    as_type(updated_vla_type) << "__orig = (" << as_type(updated_vla_type) << ")"
                                    <<               as_symbol(private_sym) << ";"
                                    <<    as_type(base_type) << "__dest = (" << as_type(base_type) << ")"
                                    <<            as_symbol(vla_private_sym) << ";"
                                    <<    "while (__dest < (" << as_type(base_type) << ")(__orig+1))"
                                    <<    "{"
                                    <<    "(*__dest).~" << class_type.advance_over_typedefs().get_symbol().get_name() << "();"
                                    <<    "__dest++;"
                                    <<    "}"
                                    << "}"
                                    ;
                            }
                            else
                            {
                                TL::Type class_type = (*it)->get_symbol().get_type().no_ref().get_unqualified_type();
                                if (class_type.is_dependent())
                                {
                                    final_statements
                                        << "{"
                                        <<    "typedef " << as_type(class_type) << " DepType;"
                                        <<    as_symbol(private_sym) << ".~DepType();"
                                        << "}"
                                        ;
                                }
                                else if (class_type.is_array())
                                {
                                    TL::Type base_type = (*it)->get_symbol().get_type().no_ref();

                                    while (base_type.is_array())
                                        base_type = base_type.array_element();

                                    base_type = base_type.get_unqualified_type().get_pointer_to();
                                    ERROR_CONDITION(!base_type.points_to().is_named_class(), "This should be a named class type (%s)",
                                            print_declarator(base_type.points_to().get_internal_type()));

                                    final_statements
                                        << "{"
                                        <<     as_type(base_type) << "__dest = (" << as_type(base_type) << ")"
                                        <<           as_symbol(private_sym) << ";"
                                        <<     "while (__dest < (" << as_type(base_type) << ")(&" << as_symbol(private_sym) << "+1))"
                                        <<     "{"
                                        <<     "(*__dest).~"
                                        <<          base_type.points_to().advance_over_typedefs().get_symbol().get_name()
                                        <<      "();"
                                        <<     "__dest++;"
                                        <<     "}"
                                        << "}"
                                        ;
                                }
                                else if (class_type.is_named_class())
                                {
                                    final_statements << as_symbol(private_sym)
                                        << ".~" << class_type.advance_over_typedefs().get_symbol().get_name() << "();";
                                }
                            }
                        }
                        break;
                    }
                case OutlineDataItem::SHARING_REDUCTION:
                    {
                        // Original reduced variable. Passed as we pass shared parameters
                        TL::Type param_type = (*it)->get_in_outline_type();
                        scope_entry_t* shared_reduction_sym = ::new_symbol(function_context, function_context->current_scope,
                                uniquestr((*it)->get_field_name().c_str()));
                        shared_reduction_sym->kind = SK_VARIABLE;
                        shared_reduction_sym->type_information = param_type.get_internal_type();
                        symbol_entity_specs_set_is_user_declared(shared_reduction_sym, 1);
                        shared_reduction_sym->defined = 1;
                        parameter_symbols.append(shared_reduction_sym);

                        symbol_entity_specs_set_is_allocatable(shared_reduction_sym,
                                (sym.is_valid()
                                    && !sym.is_member()
                                    && sym.is_allocatable())
                                || (*it)->is_copy_of_array_descriptor_allocatable());

                        (*it)->reduction_set_shared_symbol_in_outline(shared_reduction_sym);

                        // Private vector of partial reductions. This is a local pointer variable
                        // rdv stands for reduction vector
                        TL::Type private_reduction_vector_type = (*it)->get_private_type();
                        if (IS_C_LANGUAGE
                                || IS_CXX_LANGUAGE)
                        {
                            // T*
                            private_reduction_vector_type = private_reduction_vector_type.get_pointer_to();
                        }
                        else if (IS_FORTRAN_LANGUAGE)
                        {
                            // The type will be a pointer to a descripted array
                            private_reduction_vector_type = private_reduction_vector_type.get_array_to_with_descriptor(
                                    Nodecl::NodeclBase::null(),
                                    Nodecl::NodeclBase::null(),
                                    sc);
                            private_reduction_vector_type = private_reduction_vector_type.get_pointer_to();
                        }
                        else
                        {
                            internal_error("Code unreachable", 0);
                        }

                        scope_entry_t* private_reduction_vector_sym = ::new_symbol(function_context, function_context->current_scope,
                                uniquestr(("rdv_" + name).c_str()));
                        private_reduction_vector_sym->kind = SK_VARIABLE;
                        private_reduction_vector_sym->type_information = private_reduction_vector_type.get_internal_type();
                        symbol_entity_specs_set_is_user_declared(private_reduction_vector_sym, 1);
                        private_reduction_vector_sym->defined = 1;

                        reduction_vector_symbols.append(private_reduction_vector_sym);

                        if (IS_CXX_LANGUAGE)
                        {
                            initial_statements << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                                        private_reduction_vector_sym));
                        }

                        // Local variable (rdp stands for reduction private)
                        scope_entry_t* private_sym = ::new_symbol(function_context, function_context->current_scope,
                                uniquestr(("rdp_" + name).c_str()));
                        private_sym->kind = SK_VARIABLE;
                        private_sym->type_information = (*it)->get_private_type().get_internal_type();
                        symbol_entity_specs_set_is_user_declared(private_sym, 1);
                        private_sym->defined = 1;

                        reduction_private_symbols.append(private_sym);

                        if (IS_CXX_LANGUAGE)
                        {
                            initial_statements << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), private_sym));
                        }

                        if (sym.is_valid())
                        {
                            symbol_entity_specs_set_is_allocatable(private_sym,
                                    (*it)->get_private_type().is_fortran_array()
                                    && (*it)->get_private_type().array_requires_descriptor());

                            if (symbol_entity_specs_get_is_allocatable(private_sym))
                            {
                                initial_statements << emit_allocate_statement_using_array(private_sym, shared_reduction_sym);
                            }

                            symbol_map->add_map(sym, private_sym);
                        }

                        // This variable must be initialized properly
                        OpenMP::Reduction* red = (*it)->get_reduction_info().first;
                        if (!red->get_initializer().is_null())
                        {
                            Nodecl::Utils::SimpleSymbolMap reduction_init_map;
                            reduction_init_map.add_map(red->get_omp_priv(), private_sym);
                            reduction_init_map.add_map(red->get_omp_orig(), shared_reduction_sym);
                            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                            {
                                if (!is_array_type(private_sym->type_information))
                                {
                                    if (!red->get_initializer().is<Nodecl::FunctionCall>())
                                    {
                                        private_sym->value = Nodecl::Utils::deep_copy(red->get_initializer(),
                                                Scope(function_context),
                                                reduction_init_map).get_internal_nodecl();
                                    }
                                    else
                                    {
                                        initial_statements
                                            << as_expression(
                                                    Nodecl::Utils::deep_copy(red->get_initializer(),
                                                        Scope(function_context),
                                                        reduction_init_map).get_internal_nodecl()
                                                    ) << ";";
                                    }
                                }
                                else
                                {
                                    Source type_name;
                                    type_name << as_type(red->get_type());
                                    initial_statements
                                        << "{ "
                                        <<     type_name << "* rdp_init_end = (" << type_name << "*)((&" << as_symbol(private_sym) << ")+1);"
                                        <<     type_name << "* rdp_init_it = (" << type_name << "*)" << as_symbol(private_sym) << ";"
                                        <<     "while (rdp_init_it < rdp_init_end)"
                                        <<     "{"
                                        <<        "*rdp_init_it = "
                                        <<           as_expression(Nodecl::Utils::deep_copy(red->get_initializer(),
                                                        Scope(function_context),
                                                        reduction_init_map).get_internal_nodecl()) << ";"
                                        <<       "rdp_init_it++;"
                                        <<     "}"
                                        << "}"
                                        ;
                                }
                            }
                            else if (IS_FORTRAN_LANGUAGE)
                            {
                                Nodecl::NodeclBase init_expr = Nodecl::Utils::deep_copy(red->get_initializer(),
                                        Scope(function_context),
                                        reduction_init_map);

                                Nodecl::Symbol sym_ref = Nodecl::Symbol::make(private_sym);
                                type_t* lvalue_ref = get_lvalue_reference_type(private_sym->type_information);
                                sym_ref.set_type(lvalue_ref);

                                Nodecl::NodeclBase assignment_statement = Nodecl::ExpressionStatement::make(
                                        Nodecl::Assignment::make(
                                            sym_ref,
                                            init_expr,
                                            lvalue_ref));

                                initial_statements << as_statement(assignment_statement);
                            }
                            else
                            {
                                internal_error("Code unreachable", 0);
                            }
                        }

                        break;
                    }
                case OutlineDataItem::SHARING_UNDEFINED:
                    {
                        internal_error("Undefined data sharing kind for symbol %s",
                                (*it)->get_symbol().get_name().c_str());
                        break;
                    }
                default:
                    {
                        internal_error("Unexpected data sharing kind = %d for symbol %s",
                                (*it)->get_sharing(),
                                (*it)->get_symbol().get_name().c_str());
                    }
            }
        }

        FORTRAN_LANGUAGE()
        {
            // Now, we can update the cray_pointers of any cray_pointee properly
            for (TL::ObjectList<TL::Symbol>::iterator it_cray_pointee = cray_pointee_list.begin();
                    it_cray_pointee != cray_pointee_list.end();
                    it_cray_pointee++)
            {
                TL::Symbol cray_pointee = *it_cray_pointee;
                TL::Symbol updated_cray_pointer =  symbol_map->map(cray_pointee.get_cray_pointer());
                symbol_entity_specs_set_cray_pointer(cray_pointee.get_internal_symbol(), updated_cray_pointer.get_internal_symbol());
            }
        }

        struct GatherUnmappedSavedExpressions
        {
            const decl_context_t* &function_context;
            Nodecl::Utils::SimpleSymbolMap* &symbol_map;

            GatherUnmappedSavedExpressions(const decl_context_t* &fc,
                    Nodecl::Utils::SimpleSymbolMap*& sm)
                : function_context(fc), symbol_map(sm) { }

            void new_private_saved_expression(TL::Symbol sym)
            {
                Nodecl::NodeclBase v = sym.get_value();
                v = Nodecl::Utils::deep_copy(v,
                        Scope(function_context),
                        *symbol_map);

                scope_entry_t* private_sym = ::new_symbol(
                        function_context,
                        function_context->current_scope,
                        uniquestr(sym.get_name().c_str()));

                private_sym->kind = SK_VARIABLE;
                private_sym->type_information = sym.get_type().get_internal_type();
                private_sym->value = v.get_internal_nodecl();
                symbol_entity_specs_set_is_saved_expression(private_sym, 1);

                symbol_map->add_map(sym, private_sym);
            }

            void new_private_saved_expression_if_needed(TL::Symbol sym)
            {
                if (symbol_map->map(sym) == sym)
                {
                    new_private_saved_expression(sym);
                }
            }

            void gather_unmmaped(TL::Type t)
            {
                if (!t.is_valid())
                    return;

                if (t.is_array())
                {
                    gather_unmmaped(t.array_element());

                    if (IS_FORTRAN_LANGUAGE)
                    {
                        Nodecl::NodeclBase lower, upper;

                        t.array_get_bounds(lower, upper);

                        if (!lower.is_null()
                                && lower.is<Nodecl::Symbol>()
                                && lower.get_symbol().is_saved_expression())
                        {
                            new_private_saved_expression_if_needed(lower.get_symbol());
                        }

                        if (!upper.is_null()
                                && upper.is<Nodecl::Symbol>()
                                && upper.get_symbol().is_saved_expression())
                        {
                            new_private_saved_expression_if_needed(upper.get_symbol());
                        }
                    }
                    else if (IS_C_LANGUAGE
                            || IS_CXX_LANGUAGE)
                    {
                        Nodecl::NodeclBase size;
                        size = t.array_get_size();

                        if (size.is<Nodecl::Symbol>()
                                && size.get_symbol().is_saved_expression())
                        {
                            new_private_saved_expression_if_needed(size.get_symbol());
                        }
                    }
                    else
                    {
                        internal_error("Code unreachable", 0);
                    }
                }
                else if (t.is_pointer())
                {
                    gather_unmmaped(t.points_to());
                }
                else if (t.is_any_reference())
                {
                    gather_unmmaped(t.references_to());
                }
            }

            void gather(TL::ObjectList<TL::Symbol>& symbols)
            {
                for (TL::ObjectList<TL::Symbol>::iterator it2 = symbols.begin();
                        it2 != symbols.end();
                        it2++)
                {
                    gather_unmmaped(it2->get_type());
                }
            }
        };

        GatherUnmappedSavedExpressions gather_unmmaped(function_context, symbol_map);
        gather_unmmaped.gather(parameter_symbols);

        struct UpdateTypesVLA
        {
            const decl_context_t* &function_context;
            Nodecl::Utils::SimpleSymbolMap* &symbol_map;

            UpdateTypesVLA(const decl_context_t* &fc,
                    Nodecl::Utils::SimpleSymbolMap*& sm)
                : function_context(fc), symbol_map(sm) { }

            void update(TL::ObjectList<TL::Symbol>& symbols)
            {
                for (TL::ObjectList<TL::Symbol>::iterator it2 = symbols.begin();
                        it2 != symbols.end();
                        it2++)
                {
                    it2->get_internal_symbol()->type_information =
                        type_deep_copy(it2->get_internal_symbol()->type_information,
                                function_context,
                                symbol_map->get_symbol_map());
                }
            }
        };

        // Update types of parameters (this is needed by VLAs)
        UpdateTypesVLA update_vla(function_context, symbol_map);
        update_vla.update(parameter_symbols);

        // Update types of privates (this is needed by VLAs)
        update_vla.update(private_symbols);

        // VLAs in C++
        update_vla.update(vla_private_symbols);

        // Update types of reduction symbols (this is needed by VLAs)
        update_vla.update(reduction_private_symbols);
        update_vla.update(reduction_vector_symbols);

        // Build the function type
        parameter_info_t* p_types = new parameter_info_t[parameter_symbols.size()];
        parameter_info_t* it_ptypes = &(p_types[0]);
        for (ObjectList<TL::Symbol>::iterator it2 = parameter_symbols.begin();
                it2 != parameter_symbols.end();
                it2++, it_ptypes++)
        {
            it_ptypes->is_ellipsis = 0;
            it_ptypes->nonadjusted_type_info = NULL;

            // FIXME - We should do all the remaining lvalue adjustments
            it_ptypes->type_info = get_unqualified_type(it2->get_internal_symbol()->type_information);
            if (is_restrict_qualified_type(it2->get_internal_symbol()->type_information))
            {
                it_ptypes->type_info = get_restrict_qualified_type(it_ptypes->type_info);
            }
        }

        type_t *function_type = get_new_function_type(get_void_type(),
                p_types, parameter_symbols.size(),
                REF_QUALIFIER_NONE);
        delete[] p_types;

        // Now everything is set to register the function
        scope_entry_t* new_function_sym = NULL;
        if (!current_function.get_type().is_template_specialized_type()
                || current_function.get_scope().get_template_parameters()->is_explicit_specialization)
        {
            decl_context_t* new_decl_context = decl_context_clone(decl_context);
            if (current_function.get_scope().get_template_parameters() != NULL
                && current_function.get_scope().get_template_parameters()->is_explicit_specialization)
            {
                new_decl_context->template_parameters = new_decl_context->template_parameters->enclosing;
            }

            new_function_sym = new_symbol(new_decl_context,
                    new_decl_context->current_scope,
                    uniquestr(function_name.c_str()));
            symbol_entity_specs_set_is_user_declared(new_function_sym, 1);
            new_function_sym->kind = SK_FUNCTION;
            new_function_sym->locus = make_locus("", 0, 0);
            new_function_sym->type_information = function_type;
        }
        else
        {
            scope_entry_t* new_template_sym =
                new_symbol(decl_context, decl_context->current_scope, uniquestr(function_name.c_str()));
            new_template_sym->kind = SK_TEMPLATE;
            new_template_sym->locus = make_locus("", 0, 0);

            new_template_sym->type_information = get_new_template_type(
                    decl_context->template_parameters,
                    function_type,
                    uniquestr(function_name.c_str()),
                    decl_context, make_locus("", 0, 0));

            template_type_set_related_symbol(new_template_sym->type_information, new_template_sym);

            if (current_function.is_member())
            {
                symbol_entity_specs_set_is_member(new_template_sym, 1);
                symbol_entity_specs_set_class_type(new_template_sym, current_function.get_class_type().get_internal_type());
            }

            // The new function is the primary template specialization
            new_function_sym = named_type_get_symbol(
                    template_type_get_primary_type(
                        new_template_sym->type_information));
        }

        // Make it static
        symbol_entity_specs_set_is_static(new_function_sym, 1);

        if (IS_CXX_LANGUAGE
                && !is_function_task
                && current_function.is_member()
                && !current_function.is_static())
        {
            symbol_entity_specs_set_is_static(new_function_sym, 0);
        }

        // Finally, we update the parameters of the new function symbol
        for (ObjectList<TL::Symbol>::iterator it2 = parameter_symbols.begin();
                it2 != parameter_symbols.end();
                it2++, it_ptypes++)
        {
            scope_entry_t* param = it2->get_internal_symbol();
            symbol_set_as_parameter_of_function(param, new_function_sym,
                    /* nesting */ 0,
                    /* position */ symbol_entity_specs_get_num_related_symbols(new_function_sym));
            symbol_entity_specs_add_related_symbols(new_function_sym,
                    param);
        }

        // Make it member if the enclosing function is member
        if (current_function.is_member())
        {
            symbol_entity_specs_set_is_member(new_function_sym, 1);
            symbol_entity_specs_set_class_type(new_function_sym, current_function.get_class_type().get_internal_type());

            symbol_entity_specs_set_access(new_function_sym, AS_PUBLIC);

            ::class_type_add_member(symbol_entity_specs_get_class_type(new_function_sym),
                    new_function_sym,
                    new_function_sym->decl_context,
                    /* is_definition */ 0);
        }

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            new_function_sym->type_information = ::get_cv_qualified_type(
                    new_function_sym->type_information,
                    get_cv_qualifier(current_function.get_type().get_internal_type()));
        }

        if (current_function.is_inline())
            symbol_entity_specs_set_is_inline(new_function_sym, 1);

        // symbol_entity_specs_get_is_defined_inside_class_specifier(new_function_sym) =
        //     symbol_entity_specs_get_is_defined_inside_class_specifier(current_function.get_internal_symbol());

        if (IS_FORTRAN_LANGUAGE && current_function.is_in_module())
        {
            scope_entry_t* module_sym = current_function.in_module().get_internal_symbol();
            symbol_entity_specs_set_in_module(new_function_sym, module_sym);
            symbol_entity_specs_add_related_symbols(module_sym,
                    new_function_sym);
            symbol_entity_specs_set_is_module_procedure(new_function_sym, 1);
        }

        function_context->function_scope->related_entry = new_function_sym;
        function_context->block_scope->related_entry = new_function_sym;

        new_function_sym->related_decl_context = function_context;
        return new_function_sym;
    }

    void DeviceProvider::add_forward_function_code_to_extra_c_code(
            const std::string& outline_name,
            TL::ObjectList<OutlineDataItem*> data_items,
            Nodecl::NodeclBase parse_context)
    {
        Source ancillary_source, parameters;

        ancillary_source
            << "extern void " << outline_name << "_forward_" << "(";
        int num_data_items = data_items.size();
        if (num_data_items == 0)
        {
            ancillary_source << "void (*outline_fun)(void)";
        }
        else
        {
            ancillary_source << "void (*outline_fun)(";
            if (num_data_items == 0)
            {
                ancillary_source << "void";
            }
            else
            {
                for (int i = 0; i < num_data_items; i++)
                {
                    if (i > 0)
                    {
                        ancillary_source << ", ";
                    }
                    ancillary_source << "void *p" << i;
                }
            }
            ancillary_source << ")";

            for (int i = 0; i < num_data_items; i++)
            {
                ancillary_source << ", void *p" << i;
            }
        }
        ancillary_source << ")\n{\n"
            // << "    extern int nanos_free(void*);\n"
            << "    extern int nanos_handle_error(int);\n\n"
            << "    outline_fun(";
        for (int i = 0; i < num_data_items; i++)
        {
            if (i > 0)
            {
                ancillary_source << ", ";
            }
            ancillary_source << "p" << i;
        }
        ancillary_source << ");\n";

        // Free all the allocated descriptors
        // bool first = true;
        // int i = 0;
        // for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
        //         it != data_items.end();
        //         it++, i++)
        // {
        //     OutlineDataItem &item (*(*it));

        //     if (item.get_symbol().is_valid()
        //             && item.get_sharing() == OutlineDataItem::SHARING_SHARED)
        //     {
        //         TL::Type t = item.get_symbol().get_type();

        //         if (!item.get_symbol().is_allocatable()
        //                 && t.is_lvalue_reference()
        //                 && t.references_to().is_array()
        //                 && t.references_to().array_requires_descriptor())
        //         {
        //             if (first)
        //             {
        //                 ancillary_source << "   nanos_err_t nanos_err;\n";
        //                 first = false;
        //             }

        //             ancillary_source
        //                 << "    nanos_err = nanos_free(p" << i << ");\n"
        //                 << "    if (nanos_err != NANOS_OK) nanos_handle_error(nanos_err);\n"
        //                 ;
        //         }
        //     }
        // }

        ancillary_source << "}\n\n";

        // Parse in C
        Source::source_language = SourceLanguage::C;

        Nodecl::List n = ancillary_source.parse_global(parse_context).as<Nodecl::List>();

        // Restore original source language (Fortran)
        Source::source_language = SourceLanguage::Current;

        _extra_c_code.append(n);
    }


    // Rewrite inline
    struct RewriteExprOfVlaInOutline : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            const TL::ObjectList<OutlineDataItem*> &_data_items;
            TL::Symbol &_args_symbol;

        public:

        RewriteExprOfVlaInOutline(const TL::ObjectList<OutlineDataItem*> &data_items, TL::Symbol &args_symbol)
            : _data_items(data_items),
            _args_symbol(args_symbol)
        { }

        virtual void visit(const Nodecl::Symbol& node)
        {
            TL::Symbol sym = node.get_symbol();
            for (TL::ObjectList<OutlineDataItem*>::const_iterator it = _data_items.begin();
                    it != _data_items.end();
                    it++)
            {
                if (sym == (*it)->get_symbol())
                {
                    Nodecl::NodeclBase new_class_member_access;
                    // x -> args.x
                    Nodecl::NodeclBase new_args_ref = Nodecl::Symbol::make(_args_symbol);
                    // Should be a reference already
                    new_args_ref.set_type(_args_symbol.get_type());

                    TL::Symbol field_symbol = (*it)->get_field_symbol();
                    Nodecl::NodeclBase field_ref = Nodecl::Symbol::make(field_symbol);
                    field_ref.set_type(field_symbol.get_type());

                    new_class_member_access = Nodecl::ClassMemberAccess::make(
                            new_args_ref,
                            field_ref,
                            /* member-form */ Nodecl::NodeclBase::null(),
                            field_symbol.get_type().no_ref().get_lvalue_reference_to());

                    if (field_symbol.get_type().is_pointer())
                    {
                        // a.mcc_vla must be converted to a *(a.mcc_vla)
                        new_class_member_access = Nodecl::Dereference::make(
                                new_class_member_access,
                                field_symbol.get_type().no_ref().points_to().get_lvalue_reference_to());
                    }

                    node.replace(new_class_member_access);
                    break;
                }
            }
        }
    };

    TL::Type DeviceProvider::rewrite_type_of_vla_in_outline(
            TL::Type t,
            const TL::ObjectList<OutlineDataItem*> &data_items,
            TL::Symbol &arguments_symbol)
    {
        if (t.is_pointer())
        {
            TL::Type p = rewrite_type_of_vla_in_outline(
                    t.points_to(),
                    data_items,
                    arguments_symbol);

            return p.get_pointer_to();
        }
        else if (t.is_lvalue_reference())
        {
            TL::Type item = rewrite_type_of_vla_in_outline(
                    t.references_to(),
                    data_items,
                    arguments_symbol);

            return item.get_lvalue_reference_to();
        }
        else if (t.is_array())
        {
            TL::Type elem = rewrite_type_of_vla_in_outline(
                    t.array_element(),
                    data_items,
                    arguments_symbol);

            Nodecl::NodeclBase new_size = t.array_get_size().shallow_copy();
            RewriteExprOfVlaInOutline rewrite_expr_of_vla(data_items, arguments_symbol);
            rewrite_expr_of_vla.walk(new_size);

            return elem.get_array_to(new_size, new_size.retrieve_context());
        }
        // Do nothing
        else return t;
    }

    static void update_expressions(
            TL::ObjectList<Nodecl::NodeclBase> exprs,
            const TL::Scope& related_scope,
            Nodecl::Utils::SimpleSymbolMap* symbol_map,
            // Out
            TL::ObjectList<Nodecl::NodeclBase>&updated_exprs)
    {
        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = exprs.begin();
                it != exprs.end();
                ++it)
        {
            Nodecl::NodeclBase current_expr = *it;
            updated_exprs.append(
                    Nodecl::Utils::deep_copy(
                        current_expr,
                        related_scope,
                        *symbol_map));
        }
    }

    void DeviceProvider::update_ndrange_and_shmem_expressions(
            const TL::Scope& related_scope,
            const TargetInformation& target_info,
            Nodecl::Utils::SimpleSymbolMap* symbol_map,
            // out
            TL::ObjectList<Nodecl::NodeclBase>& new_ndrange_exprs,
            TL::ObjectList<Nodecl::NodeclBase>& new_shmem_exprs)
    {
        update_expressions(
                target_info.get_ndrange(),
                related_scope,
                symbol_map,
                new_ndrange_exprs);

        update_expressions(
                target_info.get_shmem(),
                related_scope,
                symbol_map,
                new_shmem_exprs);
    }

} }

namespace TL
{
    namespace Nanox {

        namespace {

            std::set<TL::Type> _used_types;

            bool in_the_same_module(TL::Scope sc, TL::Symbol module)
            {
                scope_t* current_scope = sc.get_decl_context()->current_scope;

                while (current_scope != NULL)
                {
                    if (current_scope->related_entry == module.get_internal_symbol())
                        return 1;

                    current_scope = current_scope->contained_in;
                }

                return 0;
            }

            void add_used_types_rec(TL::Type t, TL::Scope sc)
            {
                if (!t.is_valid())
                    return;

                std::pair<std::set<TL::Type>::iterator, bool> p = _used_types.insert(t);
                if (!p.second)
                    return;

                if (t.is_named_class())
                {
                    if (t.get_symbol().is_from_module())
                    {
                        Nodecl::Utils::Fortran::append_module_to_scope(t.get_symbol().from_module(), sc);
                    }
                    else if (t.get_symbol().is_in_module()
                            && !in_the_same_module(sc, t.get_symbol().in_module()))
                    {
                        Nodecl::Utils::Fortran::append_module_to_scope(t.get_symbol().in_module(), sc);
                    }
                    else
                    {
                        TL::ObjectList<TL::Symbol> members = t.get_fields();
                        for (TL::ObjectList<TL::Symbol>::iterator it = members.begin();
                                it != members.end();
                                it++)
                        {
                            add_used_types_rec(it->get_type(), sc);
                        }
                    }
                }
                else if (t.is_lvalue_reference())
                {
                    add_used_types_rec(t.references_to(), sc);
                }
                else if (t.is_pointer())
                {
                    add_used_types_rec(t.points_to(), sc);
                }
                else if (t.is_array())
                {
                    add_used_types_rec(t.array_element(), sc);
                }

                _used_types.erase(t);
            }

        }
    }

    // We rely on qualification to detect undefined references
    void Nanox::add_used_types(const TL::ObjectList<OutlineDataItem*> &data_items, TL::Scope sc)
    {
        for (TL::ObjectList<OutlineDataItem*>::const_iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            add_used_types_rec((*it)->get_in_outline_type(), sc);
        }
    }

    void Nanox::duplicate_internal_subprograms(
            TL::ObjectList<Nodecl::NodeclBase>& internal_function_codes,
            TL::Scope scope_of_unpacked,
            Nodecl::Utils::SimpleSymbolMap* &symbol_map,
            Nodecl::NodeclBase& output_statements
            )
    {
        if (internal_function_codes.empty())
            return;

        ERROR_CONDITION(!output_statements.is<Nodecl::List>(), "Invalid node", 0);

        Nodecl::Utils::SimpleSymbolMap* new_map = new Nodecl::Utils::SimpleSymbolMap(symbol_map);

        for (TL::ObjectList<Nodecl::NodeclBase>::iterator
                it2 = internal_function_codes.begin();
                it2 != internal_function_codes.end();
                it2++)
        {
            ERROR_CONDITION(!it2->is<Nodecl::FunctionCode>(), "Invalid node", 0);

            TL::Symbol orig_sym = it2->get_symbol();

            Nodecl::NodeclBase copied_node = it2->shallow_copy();

            TL::Symbol new_sym = scope_of_unpacked.new_symbol(orig_sym.get_name());
            new_map->add_map(orig_sym, new_sym);

            output_statements.as<Nodecl::List>().append(copied_node);
        }

        symbol_map = new_map;
    }

    void Nanox::duplicate_nested_functions(
            TL::ObjectList<Nodecl::NodeclBase>& internal_function_codes,
            TL::Scope scope_of_unpacked,
            Nodecl::Utils::SimpleSymbolMap* &symbol_map,
            Nodecl::NodeclBase& output_statements
            )
    {
        duplicate_internal_subprograms(internal_function_codes,
                scope_of_unpacked,
                symbol_map,
                output_statements);
    }
}
