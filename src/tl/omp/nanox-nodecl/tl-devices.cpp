/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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
                "0").connect(functor(&DeviceProvider::set_instrumentation, *this));
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
        if (Nanos::Version::interface_is_at_least("master", 5019))
        {
            Source extended_descr, extra_cast, instrument_before_c,
            instrument_after_c, function_name_instr;

            if (task_label.is_null())
            {
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
            }
            else
            {
                extended_descr = task_label.get_text();
            }

            // The description should contains:
            //  - FUNC_DECL: The declaration of the function. The function name shall be qualified
            //  - FILE: The filename
            //  - LINE: The line number
            //  We use '@' as a separator of fields: FUNC_DECL @ FILE @ LINE
            extended_descr << "@" << locus_get_filename(locus) << "@" << locus_get_line(locus);

            // GCC complains if you convert a pointer to an integer of different
            // size. Since we target a unsigned long long, in architectures of 32
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
                << "nanos_err_t err;"
                << "if (nanos_funct_id_init == 0)"
                << "{"
                <<    "err = nanos_instrument_get_key(\"user-funct-location\", &nanos_instr_uf_location_key);"
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
                << "err = nanos_instrument_events(1, &event);"
                ;

            if (is_gpu_device())
            {
                instrument_after_c << "err = nanos_instrument_close_user_fun_event();";
            }
            else
            {
                instrument_after_c
                    << "event.type = NANOS_BURST_END;"
                    << "event.key = nanos_instr_uf_location_key;"
                    << "event.value = (nanos_event_value_t) " << extra_cast << function_name_instr << ";"
                    << "err = nanos_instrument_events(1, &event);"
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

        decl_context_t decl_context = sc.get_decl_context();
        decl_context_t function_context;

        function_context = new_program_unit_context(decl_context);

        TL::ObjectList<TL::Symbol> parameter_symbols, private_symbols;

        // Pointer to the real unpack function
        scope_entry_t* ptr_to_outline = ::new_symbol(function_context, function_context.current_scope,
                "outline_ptr");
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
                    {
                        break;
                    }
                case OutlineDataItem::SHARING_SHARED:
                case OutlineDataItem::SHARING_CAPTURE:
                case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                case OutlineDataItem::SHARING_REDUCTION:
                    {
                        scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope,
                                name.c_str());
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
                        private_sym->defined = private_sym->entity_specs.is_user_declared = 1;

                        private_sym->entity_specs.is_allocatable = 
                            (((*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE) 
                             == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE);

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
        scope_entry_t* new_function_sym = new_symbol(decl_context, decl_context.current_scope, function_name.c_str());
        new_function_sym->entity_specs.is_user_declared = 1;

        new_function_sym->kind = SK_FUNCTION;
        new_function_sym->locus = make_locus("", 0, 0);

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

    TL::Symbol DeviceProvider::new_function_symbol_unpacked(
            TL::Symbol current_function,
            const std::string& function_name,
            CreateOutlineInfo& info,
            // Out
            Nodecl::Utils::SymbolMap*& out_symbol_map,
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
        decl_context_t decl_context = sc.get_decl_context();

        decl_context_t function_context;
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
        Nodecl::Utils::SimpleSymbolMap *symbol_map = new Nodecl::Utils::SimpleSymbolMap();

        TL::ObjectList<TL::Symbol> parameter_symbols, private_symbols, reduction_private_symbols;
        TL::ObjectList<TL::Type> update_vla_types;

        TL::ObjectList<OutlineDataItem*> data_items = info._data_items;
        TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
        if (IS_CXX_LANGUAGE
                && !is_function_task
                && current_function.is_member()
                && !current_function.is_static()
                && it != data_items.end())
        {
            it++;
        }

        int lower_bound_index = 1;
        int upper_bound_index = 1;
        int is_allocated_index = 1;
        TL::ObjectList<TL::Symbol> cray_pointee_list;

        for (; it != data_items.end(); it++)
        {
            TL::Symbol sym = (*it)->get_symbol();
            std::string name = (*it)->get_field_name();

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

                            // Cray pointeers are handled a bit special
                            if (sym.is_cray_pointee())
                            {
                                private_sym->entity_specs.is_cray_pointee = 1;

                                // We cannot set the right cray_pointer symbol yet because It's possible that has not been created
                                private_sym->entity_specs.cray_pointer = sym.get_cray_pointer().get_internal_symbol();

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

                        if (IS_CXX_LANGUAGE
                                && (*it)->get_allocation_policy() == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY)
                        {
                            TL::Type t = (*it)->get_symbol().get_type().no_ref().get_unqualified_type();
                            ERROR_CONDITION(!t.is_named_class(), "This should be a named class type", 0);
                            final_statements << as_symbol(private_sym) << ".~" << t.get_symbol().get_name() << "();";
                        }
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

                        scope_entry_t* private_reduction_vector_sym = ::new_symbol(function_context, function_context.current_scope,
                                ("rdv_" + name).c_str());
                        private_reduction_vector_sym->kind = SK_VARIABLE;
                        private_reduction_vector_sym->type_information = private_reduction_vector_type.get_internal_type();
                        private_reduction_vector_sym->defined = private_reduction_vector_sym->entity_specs.is_user_declared = 1;

                        if (IS_CXX_LANGUAGE)
                        {
                            initial_statements << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                                        private_reduction_vector_sym));
                        }

                        // Local variable (rdp stands for reduction private)
                        scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope,
                                ("rdp_" + name).c_str());
                        private_sym->kind = SK_VARIABLE;
                        private_sym->type_information = (*it)->get_private_type().get_internal_type();
                        private_sym->defined = private_sym->entity_specs.is_user_declared = 1;

                        reduction_private_symbols.append(private_sym);

                        if (IS_CXX_LANGUAGE)
                        {
                            initial_statements << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), private_sym));
                        }


                        if (sym.is_valid())
                        {
                            private_sym->entity_specs.is_allocatable = !sym.is_member() && sym.is_allocatable();

                            if (private_sym->entity_specs.is_allocatable)
                            {
                                initial_statements << emit_allocate_statement(private_sym, is_allocated_index,
                                        lower_bound_index, upper_bound_index);
                            }

                            symbol_map->add_map(sym, private_sym);
                        }

                        // This variable must be initialized properly
                        OpenMP::Reduction* red = (*it)->get_reduction_info();
                        if (!red->get_initializer().is_null())
                        {
                            Nodecl::Utils::SimpleSymbolMap reduction_init_map;
                            reduction_init_map.add_map(red->get_omp_priv(), private_sym);
                            reduction_init_map.add_map(red->get_omp_orig(), shared_reduction_sym);
                            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                            {
                                private_sym->value = Nodecl::Utils::deep_copy(red->get_initializer(),
                                        Scope(function_context),
                                        reduction_init_map).get_internal_nodecl();
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
                default:
                    {
                        internal_error("Unexpected data sharing kind", 0);
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
                cray_pointee.get_internal_symbol()->entity_specs.cray_pointer = updated_cray_pointer.get_internal_symbol();
            }
        }

        // Update types of parameters (this is needed by VLAs)
        for (TL::ObjectList<TL::Symbol>::iterator it2 = parameter_symbols.begin();
                it2 != parameter_symbols.end();
                it2++)
        {
            it2->get_internal_symbol()->type_information =
                type_deep_copy(it2->get_internal_symbol()->type_information,
                        function_context,
                        symbol_map->get_symbol_map());
        }

        // Update types of privates (this is needed by VLAs)
        for (TL::ObjectList<TL::Symbol>::iterator it2 = private_symbols.begin();
                it2 != private_symbols.end();
                it2++)
        {
            it2->get_internal_symbol()->type_information =
                type_deep_copy(it2->get_internal_symbol()->type_information,
                        function_context,
                        symbol_map->get_symbol_map());
        }

        // Update types of reduction privates (this is needed by VLAs)
        for (TL::ObjectList<TL::Symbol>::iterator it2 = reduction_private_symbols.begin();
                it2 != reduction_private_symbols.end();
                it2++)
        {
            it2->get_internal_symbol()->type_information =
                type_deep_copy(it2->get_internal_symbol()->type_information,
                        function_context,
                        symbol_map->get_symbol_map());
        }

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
        }

        type_t *function_type = get_new_function_type(get_void_type(), p_types, parameter_symbols.size());
        delete[] p_types;

        // Now everything is set to register the function
        scope_entry_t* new_function_sym = NULL;
        if (!current_function.get_type().is_template_specialized_type())
        {
            new_function_sym = new_symbol(decl_context, decl_context.current_scope, function_name.c_str());
            new_function_sym->entity_specs.is_user_declared = 1;
            new_function_sym->kind = SK_FUNCTION;
            new_function_sym->locus = make_locus("", 0, 0);
            new_function_sym->type_information = function_type;
        }
        else
        {
            scope_entry_t* new_template_sym =
                new_symbol(decl_context, decl_context.current_scope, function_name.c_str());
            new_template_sym->kind = SK_TEMPLATE;
            new_template_sym->locus = make_locus("", 0, 0);

            new_template_sym->type_information = get_new_template_type(
                    decl_context.template_parameters,
                    function_type,
                    uniquestr(function_name.c_str()),
                    decl_context, make_locus("", 0, 0));

            template_type_set_related_symbol(new_template_sym->type_information, new_template_sym);

            // The new function is the primary template specialization
            new_function_sym = named_type_get_symbol(
                    template_type_get_primary_type(
                        new_template_sym->type_information));
        }

        // Make it static
        new_function_sym->entity_specs.is_static = 1;

        if (IS_CXX_LANGUAGE
                && !is_function_task
                && current_function.is_member()
                && !current_function.is_static())
        {
            new_function_sym->entity_specs.is_static = 0;
        }


        // Finally, we update the parameters of the new function symbol
        for (ObjectList<TL::Symbol>::iterator it2 = parameter_symbols.begin();
                it2 != parameter_symbols.end();
                it2++, it_ptypes++)
        {
            scope_entry_t* param = it2->get_internal_symbol();
            symbol_set_as_parameter_of_function(param, new_function_sym, new_function_sym->entity_specs.num_related_symbols);
            P_LIST_ADD(new_function_sym->entity_specs.related_symbols, new_function_sym->entity_specs.num_related_symbols, param);
        }

        // Make it member if the enclosing function is member
        if (current_function.is_member())
        {
            new_function_sym->entity_specs.is_member = 1;
            new_function_sym->entity_specs.class_type = current_function.get_class_type().get_internal_type();

            new_function_sym->entity_specs.access = AS_PUBLIC;

            ::class_type_add_member(new_function_sym->entity_specs.class_type,
                    new_function_sym);
        }

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            new_function_sym->type_information = ::get_cv_qualified_type(
                    new_function_sym->type_information,
                    get_cv_qualifier(current_function.get_type().get_internal_type()));
        }

        function_context.function_scope->related_entry = new_function_sym;
        function_context.block_scope->related_entry = new_function_sym;

        new_function_sym->related_decl_context = function_context;

        out_symbol_map = symbol_map;
        return new_function_sym;
    }

    TL::Symbol DeviceProvider::new_function_symbol(
            TL::Symbol current_function,
            const std::string& name,
            TL::Type return_type,
            ObjectList<std::string> parameter_names,
            ObjectList<TL::Type> parameter_types)
    {
        if (IS_FORTRAN_LANGUAGE && current_function.is_nested_function())
        {
            // Get the enclosing function
            current_function = current_function.get_scope().get_related_symbol();
        }

        decl_context_t decl_context = current_function.get_scope().get_decl_context();

        ERROR_CONDITION(parameter_names.size() != parameter_types.size(), "Mismatch between names and types", 0);

        decl_context_t function_context;
        if (IS_FORTRAN_LANGUAGE)
        {
            function_context = new_program_unit_context(decl_context);
        }
        else
        {
            function_context = new_function_context(decl_context);
            function_context = new_block_context(function_context);
        }

        // Build the function type
        int num_parameters = 0;
        scope_entry_t** parameter_list = NULL;

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
            param->locus = make_locus("", 0, 0);

            param->defined = 1;

            param->type_information = get_unqualified_type(type_it->get_internal_type());

            P_LIST_ADD(parameter_list, num_parameters, param);

            it_ptypes->is_ellipsis = 0;
            it_ptypes->nonadjusted_type_info = NULL;
            it_ptypes->type_info = get_indirect_type(param);
        }

        type_t *function_type = get_new_function_type(
                return_type.get_internal_type(),
                p_types,
                parameter_types.size());

        delete[] p_types;

        // Now, we can create the new function symbol
        scope_entry_t* new_function_sym = NULL;
        if (!current_function.get_type().is_template_specialized_type())
        {
            new_function_sym = new_symbol(decl_context, decl_context.current_scope, name.c_str());
            new_function_sym->entity_specs.is_user_declared = 1;
            new_function_sym->kind = SK_FUNCTION;
            new_function_sym->locus = make_locus("", 0, 0);
            new_function_sym->type_information = function_type;
        }
        else
        {
            scope_entry_t* new_template_sym = new_symbol(
                    decl_context, decl_context.current_scope, name.c_str());
            new_template_sym->kind = SK_TEMPLATE;
            new_template_sym->locus = make_locus("", 0, 0);

            new_template_sym->type_information = get_new_template_type(
                    decl_context.template_parameters,
                    function_type,
                    uniquestr(name.c_str()),
                    decl_context, make_locus("", 0, 0));

            template_type_set_related_symbol(new_template_sym->type_information, new_template_sym);

            // The new function is the primary template specialization
            new_function_sym = named_type_get_symbol(
                    template_type_get_primary_type(
                        new_template_sym->type_information));
        }

        function_context.function_scope->related_entry = new_function_sym;
        function_context.block_scope->related_entry = new_function_sym;

        new_function_sym->related_decl_context = function_context;

        new_function_sym->entity_specs.related_symbols = parameter_list;
        new_function_sym->entity_specs.num_related_symbols = num_parameters;
        for (int i = 0; i < new_function_sym->entity_specs.num_related_symbols; ++i)
        {
            symbol_set_as_parameter_of_function(
                    new_function_sym->entity_specs.related_symbols[i], new_function_sym, /* parameter position */ i);
        }

        // Make it static
        new_function_sym->entity_specs.is_static = 1;

        // Make it member if the enclosing function is member
        if (current_function.is_member())
        {
            new_function_sym->entity_specs.is_member = 1;
            new_function_sym->entity_specs.class_type = current_function.get_class_type().get_internal_type();

            new_function_sym->entity_specs.access = AS_PUBLIC;

            ::class_type_add_member(new_function_sym->entity_specs.class_type, new_function_sym);
        }
        return new_function_sym;
    }

    void DeviceProvider::build_empty_body_for_function(
            TL::Symbol function_symbol,
            Nodecl::NodeclBase &function_code,
            Nodecl::NodeclBase &empty_stmt
            )
    {
        empty_stmt = Nodecl::EmptyStatement::make(make_locus("", 0, 0));
        Nodecl::List stmt_list = Nodecl::List::make(empty_stmt);

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            Nodecl::CompoundStatement compound_statement =
                Nodecl::CompoundStatement::make(stmt_list,
                        /* destructors */ Nodecl::NodeclBase::null(),
                        make_locus("", 0, 0));
            stmt_list = Nodecl::List::make(compound_statement);
        }

        Nodecl::NodeclBase context = Nodecl::Context::make(
                stmt_list,
                function_symbol.get_related_scope(), make_locus("", 0, 0));

        function_symbol.get_internal_symbol()->defined = 1;

        if (function_symbol.is_dependent_function())
        {
            function_code = Nodecl::TemplateFunctionCode::make(context,
                    // Initializers
                    Nodecl::NodeclBase::null(),
                    // Internal functions
                    Nodecl::NodeclBase::null(),
                    function_symbol,
                    make_locus("", 0, 0));
        }
        else
        {
            function_code = Nodecl::FunctionCode::make(context,
                    // Initializers
                    Nodecl::NodeclBase::null(),
                    // Internal functions
                    Nodecl::NodeclBase::null(),
                    function_symbol,
                    make_locus("", 0, 0));
        }
    }

    // Rewrite inline
    struct RewriteExprOfVla : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            const TL::ObjectList<OutlineDataItem*> &_data_items;
            TL::Symbol &_args_symbol;

        public:

        RewriteExprOfVla(const TL::ObjectList<OutlineDataItem*> &data_items, TL::Symbol &args_symbol)
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

                    Nodecl::NodeclBase field_ref = Nodecl::Symbol::make((*it)->get_field_symbol());
                    field_ref.set_type(field_ref.get_symbol().get_type());

                    new_class_member_access = Nodecl::ClassMemberAccess::make(
                            new_args_ref,
                            field_ref,
                            // The type of this node should be the same
                            node.get_type());

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
            RewriteExprOfVla rewrite_expr_of_vla(data_items, arguments_symbol);
            rewrite_expr_of_vla.walk(new_size);

            return elem.get_array_to(new_size, new_size.retrieve_context());
        }
        // Do nothing
        else return t;
    }



} }
