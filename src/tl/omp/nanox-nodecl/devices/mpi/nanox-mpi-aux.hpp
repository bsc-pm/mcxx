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

#ifndef MPI_AUX_HPP
#define MPI_AUX_HPP


  using namespace TL;
  using namespace TL::Nanox;
  
//  // This is only for Fortran!
//    TL::Symbol DeviceProvider::new_function_symbol_forward(
//            TL::Symbol current_function,
//            const std::string& function_name,
//            CreateOutlineInfo& info)
//    {
//        if (IS_FORTRAN_LANGUAGE && current_function.is_nested_function())
//        {
//            // Get the enclosing function
//            current_function = current_function.get_scope().get_related_symbol();
//        }
//
//        // This is only for Fortran!
//        Scope sc = current_function.get_scope();
//
//        decl_context_t decl_context = sc.get_decl_context();
//        decl_context_t function_context;
//
//        function_context = new_program_unit_context(decl_context);
//
//        TL::ObjectList<TL::Symbol> parameter_symbols, private_symbols;
//
//        // Pointer to the real unpack function
//        scope_entry_t* ptr_to_outline = ::new_symbol(function_context, function_context.current_scope,
//                UNIQUESTR_LITERAL("outline_ptr"));
//        ptr_to_outline->kind = SK_VARIABLE;
//        ptr_to_outline->type_information = fortran_choose_int_type_from_kind(CURRENT_CONFIGURATION->type_environment->sizeof_pointer);
//        parameter_symbols.append(ptr_to_outline);
//
//        TL::ObjectList<OutlineDataItem*> data_items = info._data_items;
//        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
//                it != data_items.end();
//                it++)
//        {
//            TL::Symbol sym = (*it)->get_symbol();
//
//            std::string name;
//            if (sym.is_valid())
//            {
//                name = sym.get_name();
//                if (IS_CXX_LANGUAGE
//                        && name == "this")
//                {
//                    name = "this_";
//                }
//            }
//            else
//            {
//                name = (*it)->get_field_name();
//            }
//
//            switch ((*it)->get_sharing())
//            {
//                case OutlineDataItem::SHARING_PRIVATE:
//                    {
//                        break;
//                    }
//                case OutlineDataItem::SHARING_SHARED:
//                case OutlineDataItem::SHARING_CAPTURE:
//                case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
//                case OutlineDataItem::SHARING_REDUCTION:
//                    {
//                        scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope,
//                                uniquestr(name.c_str()));
//                        private_sym->kind = SK_VARIABLE;
//                        if ((*it)->get_field_type().is_pointer()
//                                && (*it)->get_field_type().points_to().is_void())
//                        {
//                            // Preserve void*
//                            private_sym->type_information = (*it)
//                                ->get_field_type()
//                                .get_internal_type();
//                        }
//                        else
//                        {
//                            private_sym->type_information = (*it)
//                                ->get_field_type()
//                                .get_lvalue_reference_to()
//                                .get_internal_type();
//                        }
//                        private_sym->defined = private_sym->entity_specs.is_user_declared = 1;
//
//                        private_sym->entity_specs.is_allocatable = 
//                            (((*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE) 
//                             == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE);
//
//                        
//                        if ( !((*it)->get_symbol().is_from_module() && (*it)->get_symbol().is_allocatable()) ){
//                           parameter_symbols.append(private_sym);
//                        }
//                        break;
//                    }
//                default:
//                    {
//                        internal_error("Unexpected data sharing kind", 0);
//                    }
//            }
//        }
//
//        // Now everything is set to register the function
//        scope_entry_t* new_function_sym = new_symbol(decl_context, decl_context.current_scope, uniquestr(function_name.c_str()));
//        new_function_sym->entity_specs.is_user_declared = 1;
//
//        new_function_sym->kind = SK_FUNCTION;
//        new_function_sym->locus = make_locus("", 0, 0);
//
//        // Make it static
//        new_function_sym->entity_specs.is_static = 1;
//
//        // Make it member if the enclosing function is member
//        if (current_function.is_member())
//        {
//            new_function_sym->entity_specs.is_member = 1;
//            new_function_sym->entity_specs.class_type = current_function.get_class_type().get_internal_type();
//
//            new_function_sym->entity_specs.access = AS_PUBLIC;
//
//            ::class_type_add_member(new_function_sym->entity_specs.class_type,
//                    new_function_sym);
//        }
//
//        function_context.function_scope->related_entry = new_function_sym;
//        function_context.block_scope->related_entry = new_function_sym;
//
//        new_function_sym->related_decl_context = function_context;
//
//        parameter_info_t* p_types = new parameter_info_t[parameter_symbols.size()];
//
//        parameter_info_t* it_ptypes = &(p_types[0]);
//        for (ObjectList<TL::Symbol>::iterator it = parameter_symbols.begin();
//                it != parameter_symbols.end();
//                it++, it_ptypes++)
//        {
//            scope_entry_t* param = it->get_internal_symbol();
//
//            symbol_set_as_parameter_of_function(param, new_function_sym, new_function_sym->entity_specs.num_related_symbols);
//
//            P_LIST_ADD(new_function_sym->entity_specs.related_symbols,
//                    new_function_sym->entity_specs.num_related_symbols,
//                    param);
//
//            it_ptypes->is_ellipsis = 0;
//            it_ptypes->nonadjusted_type_info = NULL;
//
//            // FIXME - We should do all the remaining lvalue adjustments
//            type_t* param_type = get_unqualified_type(param->type_information);
//            it_ptypes->type_info = param_type;
//        }
//
//        type_t *function_type = get_new_function_type(
//                get_void_type(),
//                p_types,
//                parameter_symbols.size());
//
//        new_function_sym->type_information = function_type;
//
//        delete[] p_types;
//
//        return new_function_sym;
//    }
//
//    
//    
//    TL::Symbol DeviceProvider::new_function_symbol_unpacked(
//            TL::Symbol current_function,
//            const std::string& function_name,
//            CreateOutlineInfo& info,
//            // Out
//            Nodecl::Utils::SimpleSymbolMap*& symbol_map,
//            Source &initial_statements,
//            Source &final_statements)
//    {
//        if (IS_FORTRAN_LANGUAGE && current_function.is_nested_function())
//        {
//            // Get the enclosing function
//            current_function = current_function.get_scope().get_related_symbol();
//        }
//
//        bool is_function_task = info._called_task.is_valid();
//
//        Scope sc = current_function.get_scope();
//        decl_context_t decl_context = sc.get_decl_context();
//
//        decl_context_t function_context;
//        if (IS_FORTRAN_LANGUAGE)
//        {
//            function_context = new_program_unit_context(decl_context);
//        }
//        else
//        {
//            function_context = new_function_context(decl_context);
//            function_context = new_block_context(function_context);
//        }
//
//        // Create all the symbols and an appropiate mapping
//        TL::ObjectList<TL::Symbol> parameter_symbols,
//            private_symbols,
//            reduction_private_symbols,
//            reduction_vector_symbols;
//
//        int lower_bound_index = 1;
//        int upper_bound_index = 1;
//        int is_allocated_index = 1;
//        TL::ObjectList<TL::Symbol> cray_pointee_list;
//
//        TL::ObjectList<OutlineDataItem*> data_items = info._data_items;
//        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
//                it != data_items.end();
//                it++)
//        {
//            TL::Symbol sym = (*it)->get_symbol();
//            std::string name = (*it)->get_field_name();
//
//            if (!is_function_task
//                    && (*it)->get_is_cxx_this())
//                continue;
//
//            bool already_mapped = false;
//            switch ((*it)->get_sharing())
//            {
//                case OutlineDataItem::SHARING_ALLOCA:
//                    {
//                        // Do nothing
//                        break;
//                    }
//                case OutlineDataItem::SHARING_PRIVATE:
//                    {
//                        scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope, uniquestr(name.c_str()));
//                        private_sym->kind = SK_VARIABLE;
//                        private_sym->type_information = (*it)->get_in_outline_type().get_internal_type();
//                        private_sym->defined = private_sym->entity_specs.is_user_declared = 1;
//
//                        if (sym.is_valid())
//                        {
//                            symbol_map->add_map(sym, private_sym);
//
//                            // Copy attributes that must be preserved
//                            private_sym->entity_specs.is_allocatable = !sym.is_member() && sym.is_allocatable();
//
//                            // Cray pointeers are handled a bit special
//                            if (sym.is_cray_pointee())
//                            {
//                                private_sym->entity_specs.is_cray_pointee = 1;
//
//                                // We cannot set the right cray_pointer symbol yet because It's possible that has not been created
//                                private_sym->entity_specs.cray_pointer = sym.get_cray_pointer().get_internal_symbol();
//
//                                cray_pointee_list.append(private_sym);
//                            }
//                        }
//
//                        private_symbols.append(private_sym);
//
//                        if (IS_CXX_LANGUAGE)
//                        {
//                            // We need the declarations of the private symbols!
//                            initial_statements << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), private_sym));
//                        }
//
//                        if ((*it)->get_symbol().is_valid()
//                                && (*it)->get_symbol().is_allocatable())
//                        {
//                            initial_statements << emit_allocate_statement((*it)->get_symbol(), is_allocated_index,
//                                    lower_bound_index, upper_bound_index);
//                        }
//                        break;
//                    }
//                case OutlineDataItem::SHARING_SHARED:
//                case OutlineDataItem::SHARING_SHARED_WITH_CAPTURE:
//                case OutlineDataItem::SHARING_SHARED_ALLOCA:
//                case OutlineDataItem::SHARING_CAPTURE:
//                case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
//                    {
//                        scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope,
//                                uniquestr(name.c_str()));
//
//                        private_sym->kind = SK_VARIABLE;
//                        private_sym->type_information = (*it)->get_in_outline_type().get_internal_type();
//                        private_sym->defined = private_sym->entity_specs.is_user_declared = 1;
//
//
//                        if (sym.is_valid())
//                        {
//                            private_sym->entity_specs.is_optional = sym.is_optional();
//                            private_sym->entity_specs.is_allocatable =
//                                !sym.is_member() && sym.is_allocatable();
//                            if (!already_mapped)
//                            {
//                                symbol_map->add_map(sym, private_sym);
//                            }
//                        }
//
//                        private_sym->entity_specs.is_allocatable =
//                            sym.is_allocatable() ||
//                            (((*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE)
//                             == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE);
//
//                        parameter_symbols.append(private_sym);
//
//                        if (IS_CXX_LANGUAGE
//                                && (*it)->get_allocation_policy() == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY)
//                        {
//                            TL::Type t = (*it)->get_symbol().get_type().no_ref().get_unqualified_type();
//                            ERROR_CONDITION(!t.is_named_class(), "This should be a named class type", 0);
//                            final_statements << as_symbol(private_sym) << ".~" << t.get_symbol().get_name() << "();";
//                        }
//                        break;
//                    }
//                case OutlineDataItem::SHARING_REDUCTION:
//                    {
//                        // Original reduced variable. Passed as we pass shared parameters
//                        TL::Type param_type = (*it)->get_in_outline_type();
//                        scope_entry_t* shared_reduction_sym = ::new_symbol(function_context, function_context.current_scope,
//                                uniquestr((*it)->get_field_name().c_str()));
//                        shared_reduction_sym->kind = SK_VARIABLE;
//                        shared_reduction_sym->type_information = param_type.get_internal_type();
//                        shared_reduction_sym->defined = shared_reduction_sym->entity_specs.is_user_declared = 1;
//                        parameter_symbols.append(shared_reduction_sym);
//
//                        shared_reduction_sym->entity_specs.is_allocatable = sym.is_valid()
//                            && !sym.is_member()
//                            && sym.is_allocatable();
//
//                        (*it)->reduction_set_shared_symbol_in_outline(shared_reduction_sym);
//
//                        // Private vector of partial reductions. This is a local pointer variable
//                        // rdv stands for reduction vector
//                        TL::Type private_reduction_vector_type = (*it)->get_private_type();
//                        if (IS_C_LANGUAGE
//                                || IS_CXX_LANGUAGE)
//                        {
//                            // T*
//                            private_reduction_vector_type = private_reduction_vector_type.get_pointer_to();
//                        }
//                        else if (IS_FORTRAN_LANGUAGE)
//                        {
//                            // The type will be a pointer to a descripted array
//                            private_reduction_vector_type = private_reduction_vector_type.get_array_to_with_descriptor(
//                                    Nodecl::NodeclBase::null(),
//                                    Nodecl::NodeclBase::null(),
//                                    sc);
//                            private_reduction_vector_type = private_reduction_vector_type.get_pointer_to();
//                        }
//                        else
//                        {
//                            internal_error("Code unreachable", 0);
//                        }
//
//                        scope_entry_t* private_reduction_vector_sym = ::new_symbol(function_context, function_context.current_scope,
//                                uniquestr(("rdv_" + name).c_str()));
//                        private_reduction_vector_sym->kind = SK_VARIABLE;
//                        private_reduction_vector_sym->type_information = private_reduction_vector_type.get_internal_type();
//                        private_reduction_vector_sym->defined
//                            = private_reduction_vector_sym->entity_specs.is_user_declared = 1;
//
//                        reduction_vector_symbols.append(private_reduction_vector_sym);
//
//                        if (IS_CXX_LANGUAGE)
//                        {
//                            initial_statements << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
//                                        private_reduction_vector_sym));
//                        }
//
//                        // Local variable (rdp stands for reduction private)
//                        scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope,
//                                uniquestr(("rdp_" + name).c_str()));
//                        private_sym->kind = SK_VARIABLE;
//                        private_sym->type_information = (*it)->get_private_type().get_internal_type();
//                        private_sym->defined = private_sym->entity_specs.is_user_declared = 1;
//
//                        reduction_private_symbols.append(private_sym);
//
//                        if (IS_CXX_LANGUAGE)
//                        {
//                            initial_statements << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), private_sym));
//                        }
//
//
//                        if (sym.is_valid())
//                        {
//                            private_sym->entity_specs.is_allocatable = !sym.is_member() && sym.is_allocatable();
//
//                            if (private_sym->entity_specs.is_allocatable)
//                            {
//                                initial_statements << emit_allocate_statement(private_sym, is_allocated_index,
//                                        lower_bound_index, upper_bound_index);
//                            }
//
//                            symbol_map->add_map(sym, private_sym);
//                        }
//
//                        // This variable must be initialized properly
//                        OpenMP::Reduction* red = (*it)->get_reduction_info().first;
//                        if (!red->get_initializer().is_null())
//                        {
//                            Nodecl::Utils::SimpleSymbolMap reduction_init_map;
//                            reduction_init_map.add_map(red->get_omp_priv(), private_sym);
//                            reduction_init_map.add_map(red->get_omp_orig(), shared_reduction_sym);
//                            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
//                            {
//                                if (!is_array_type(private_sym->type_information))
//                                {
//                                    private_sym->value = Nodecl::Utils::deep_copy(red->get_initializer(),
//                                            Scope(function_context),
//                                            reduction_init_map).get_internal_nodecl();
//                                }
//                                else
//                                {
//                                    Source type_name;
//                                    type_name << as_type(red->get_type());
//                                    initial_statements
//                                        << "{ "
//                                        <<     type_name << "* rdp_init_end = (" << type_name << "*)((&" << as_symbol(private_sym) << ")+1);"
//                                        <<     type_name << "* rdp_init_it = (" << type_name << "*)" << as_symbol(private_sym) << ";"
//                                        <<     "while (rdp_init_it < rdp_init_end)"
//                                        <<     "{"
//                                        <<        "*rdp_init_it = "
//                                        <<           as_expression(Nodecl::Utils::deep_copy(red->get_initializer(),
//                                                        Scope(function_context),
//                                                        reduction_init_map).get_internal_nodecl()) << ";"
//                                        <<       "rdp_init_it++;"
//                                        <<     "}"
//                                        << "}"
//                                        ;
//                                }
//                            }
//                            else if (IS_FORTRAN_LANGUAGE)
//                            {
//                                Nodecl::NodeclBase init_expr = Nodecl::Utils::deep_copy(red->get_initializer(),
//                                        Scope(function_context),
//                                        reduction_init_map);
//
//                                Nodecl::Symbol sym_ref = Nodecl::Symbol::make(private_sym);
//                                type_t* lvalue_ref = get_lvalue_reference_type(private_sym->type_information);
//                                sym_ref.set_type(lvalue_ref);
//
//                                Nodecl::NodeclBase assignment_statement = Nodecl::ExpressionStatement::make(
//                                        Nodecl::Assignment::make(
//                                            sym_ref,
//                                            init_expr,
//                                            lvalue_ref));
//
//                                initial_statements << as_statement(assignment_statement);
//                            }
//                            else
//                            {
//                                internal_error("Code unreachable", 0);
//                            }
//                        }
//
//                        break;
//                    }
//                default:
//                    {
//                        internal_error("Unexpected data sharing kind", 0);
//                    }
//            }
//        }
//
//        FORTRAN_LANGUAGE()
//        {
//            // Now, we can update the cray_pointers of any cray_pointee properly
//            for (TL::ObjectList<TL::Symbol>::iterator it_cray_pointee = cray_pointee_list.begin();
//                    it_cray_pointee != cray_pointee_list.end();
//                    it_cray_pointee++)
//            {
//                TL::Symbol cray_pointee = *it_cray_pointee;
//                TL::Symbol updated_cray_pointer =  symbol_map->map(cray_pointee.get_cray_pointer());
//                cray_pointee.get_internal_symbol()->entity_specs.cray_pointer = updated_cray_pointer.get_internal_symbol();
//            }
//        }
//
//        struct UpdateTypesVLA
//        {
//            decl_context_t &function_context;
//            Nodecl::Utils::SimpleSymbolMap* &symbol_map;
//
//            UpdateTypesVLA(decl_context_t &fc,
//                    Nodecl::Utils::SimpleSymbolMap*& sm)
//                : function_context(fc), symbol_map(sm) { }
//
//            void update(TL::ObjectList<TL::Symbol>& symbols)
//            {
//                for (TL::ObjectList<TL::Symbol>::iterator it2 = symbols.begin();
//                        it2 != symbols.end();
//                        it2++)
//                {
//                    it2->get_internal_symbol()->type_information =
//                        type_deep_copy(it2->get_internal_symbol()->type_information,
//                                function_context,
//                                symbol_map->get_symbol_map());
//                }
//            }
//        };
//
//        // Update types of parameters (this is needed by VLAs)
//        UpdateTypesVLA update_vla(function_context, symbol_map);
//        update_vla.update(parameter_symbols);
//
//        // Update types of privates (this is needed by VLAs)
//        update_vla.update(private_symbols);
//
//        // Update types of reduction symbols (this is needed by VLAs)
//        update_vla.update(reduction_private_symbols);
//        update_vla.update(reduction_vector_symbols);
//
//        // Build the function type
//        parameter_info_t* p_types = new parameter_info_t[parameter_symbols.size()];
//        parameter_info_t* it_ptypes = &(p_types[0]);
//        for (ObjectList<TL::Symbol>::iterator it2 = parameter_symbols.begin();
//                it2 != parameter_symbols.end();
//                it2++, it_ptypes++)
//        {
//            it_ptypes->is_ellipsis = 0;
//            it_ptypes->nonadjusted_type_info = NULL;
//
//            // FIXME - We should do all the remaining lvalue adjustments
//            it_ptypes->type_info = get_unqualified_type(it2->get_internal_symbol()->type_information);
//        }
//
//        type_t *function_type = get_new_function_type(get_void_type(), p_types, parameter_symbols.size());
//        delete[] p_types;
//
//        // Now everything is set to register the function
//        scope_entry_t* new_function_sym = NULL;
//        if (!current_function.get_type().is_template_specialized_type())
//        {
//            new_function_sym = new_symbol(decl_context, decl_context.current_scope, uniquestr(function_name.c_str()));
//            new_function_sym->entity_specs.is_user_declared = 1;
//            new_function_sym->kind = SK_FUNCTION;
//            new_function_sym->locus = make_locus("", 0, 0);
//            new_function_sym->type_information = function_type;
//        }
//        else
//        {
//            scope_entry_t* new_template_sym =
//                new_symbol(decl_context, decl_context.current_scope, uniquestr(function_name.c_str()));
//            new_template_sym->kind = SK_TEMPLATE;
//            new_template_sym->locus = make_locus("", 0, 0);
//
//            new_template_sym->type_information = get_new_template_type(
//                    decl_context.template_parameters,
//                    function_type,
//                    uniquestr(function_name.c_str()),
//                    decl_context, make_locus("", 0, 0));
//
//            template_type_set_related_symbol(new_template_sym->type_information, new_template_sym);
//
//            // The new function is the primary template specialization
//            new_function_sym = named_type_get_symbol(
//                    template_type_get_primary_type(
//                        new_template_sym->type_information));
//        }
//
//        // Make it static
//        new_function_sym->entity_specs.is_static = 1;
//
//        if (IS_CXX_LANGUAGE
//                && !is_function_task
//                && current_function.is_member()
//                && !current_function.is_static())
//        {
//            new_function_sym->entity_specs.is_static = 0;
//        }
//
//        // Finally, we update the parameters of the new function symbol
//        for (ObjectList<TL::Symbol>::iterator it2 = parameter_symbols.begin();
//                it2 != parameter_symbols.end();
//                it2++, it_ptypes++)
//        {
//            scope_entry_t* param = it2->get_internal_symbol();
//            symbol_set_as_parameter_of_function(param, new_function_sym, new_function_sym->entity_specs.num_related_symbols);
//            P_LIST_ADD(new_function_sym->entity_specs.related_symbols, new_function_sym->entity_specs.num_related_symbols, param);
//        }
//
//        // Make it member if the enclosing function is member
//        if (current_function.is_member())
//        {
//            new_function_sym->entity_specs.is_member = 1;
//            new_function_sym->entity_specs.class_type = current_function.get_class_type().get_internal_type();
//
//            new_function_sym->entity_specs.access = AS_PUBLIC;
//
//            ::class_type_add_member(new_function_sym->entity_specs.class_type,
//                    new_function_sym);
//        }
//
//        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
//        {
//            new_function_sym->type_information = ::get_cv_qualified_type(
//                    new_function_sym->type_information,
//                    get_cv_qualifier(current_function.get_type().get_internal_type()));
//        }
//
//        if (current_function.is_inline())
//            new_function_sym->entity_specs.is_inline = 1;
//
//        // new_function_sym->entity_specs.is_defined_inside_class_specifier =
//        //     current_function.get_internal_symbol()->entity_specs.is_defined_inside_class_specifier;
//
//        if (IS_FORTRAN_LANGUAGE && current_function.is_in_module())
//        {
//            scope_entry_t* module_sym = current_function.in_module().get_internal_symbol();
//            new_function_sym->entity_specs.in_module = module_sym;
//            P_LIST_ADD(
//                    module_sym->entity_specs.related_symbols,
//                    module_sym->entity_specs.num_related_symbols,
//                    new_function_sym);
//            new_function_sym->entity_specs.is_module_procedure = 1;
//        }
//
//        function_context.function_scope->related_entry = new_function_sym;
//        function_context.block_scope->related_entry = new_function_sym;
//
//        new_function_sym->related_decl_context = function_context;
//        return new_function_sym;
//    }


#endif
