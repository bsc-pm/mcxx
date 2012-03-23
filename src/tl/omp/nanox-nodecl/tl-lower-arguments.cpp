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

#include "tl-lowering-visitor.hpp"
#include "tl-outline-info.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"

namespace TL { namespace Nanox {

    bool LoweringVisitor::c_type_needs_vla_handling(TL::Type t)
    {
        if (t.is_array())
        {
            return t.array_is_vla()
                || c_type_needs_vla_handling(t.array_element());
        }
        else if (t.is_pointer())
        {
            return c_type_needs_vla_handling(t.points_to());
        }
        //  FIXME - Local classes in C can be "VLA"
        else
        {
            return 0;
        }
    }

    bool LoweringVisitor::c_requires_vla_handling(OutlineDataItem& outline_data_item)
    {
        TL::Type t = outline_data_item.get_symbol().get_type();

        return c_type_needs_vla_handling(t);
    }

    TL::Type LoweringVisitor::c_handle_vla_type_rec(
            OutlineDataItem& outline_data_item,
            TL::Type type, 
            TL::Scope class_scope, 
            TL::Symbol new_class_symbol,
            TL::Type new_class_type,
            TL::ObjectList<TL::Symbol>& new_symbols,
            const std::string& filename, 
            int line)
    {
        if (type.is_array())
        {
            TL::Type synthesized_type = c_handle_vla_type_rec(
                    outline_data_item,
                    type.array_element(), 
                    class_scope, 
                    new_class_symbol, 
                    new_class_type,
                    new_symbols,
                    filename, line);

            Nodecl::NodeclBase array_size = type.array_get_size();

            if (!array_size.is_null()
                    && !array_size.is_constant())
            {
                ERROR_CONDITION(!array_size.is<Nodecl::SavedExpr>(), "This must be a saved expression", 0);

                TL::Symbol saved_symbol = array_size.as<Nodecl::SavedExpr>().get_symbol();
                new_symbols.append(saved_symbol);

                TL::Symbol vla_field = class_scope.new_symbol(saved_symbol.get_name());
                vla_field.get_internal_symbol()->kind = SK_VARIABLE;
                vla_field.get_internal_symbol()->type_information = saved_symbol.get_type().get_internal_type();

                vla_field.get_internal_symbol()->entity_specs.class_type = ::get_user_defined_type(new_class_symbol.get_internal_symbol());

                vla_field.get_internal_symbol()->entity_specs.access = AS_PUBLIC;

                vla_field.get_internal_symbol()->file = uniquestr(filename.c_str());
                vla_field.get_internal_symbol()->line = line;

                class_type_add_member(new_class_type.get_internal_type(), vla_field.get_internal_symbol());

                Nodecl::NodeclBase vla_sym = Nodecl::Symbol::make(saved_symbol, filename, line);
                vla_sym.set_type(vla_field.get_type());

                return synthesized_type.get_array_to(vla_sym, vla_field.get_scope());
            }
            else
            {
                // We do not care very much about this scope
                TL::Scope sc(CURRENT_COMPILED_FILE->global_decl_context);
                return synthesized_type.get_array_to(array_size, sc);
            }
        }
        else if (type.is_pointer())
        {
            TL::Type synthesized_type = c_handle_vla_type_rec(
                    outline_data_item,
                    type.points_to(), 
                    class_scope, new_class_symbol, 
                    new_class_type,
                    new_symbols,
                    filename, line);
            return synthesized_type.get_pointer_to();
        }
        else
        {
            return type;
        }
    }

    void LoweringVisitor::c_handle_vla_type(
            OutlineDataItem& outline_data_item,
            TL::Scope class_scope, 
            TL::Symbol new_class_symbol,
            TL::Type new_class_type,
            TL::ObjectList<TL::Symbol>& new_symbols,
            const std::string& filename, 
            int line)
    {
        TL::Type synthesized_type = c_handle_vla_type_rec(
                outline_data_item,
                outline_data_item.get_symbol().get_type(),
                class_scope,
                new_class_symbol, 
                new_class_type,
                new_symbols,
                filename, line);

        if (synthesized_type.is_array())
        {
            synthesized_type = synthesized_type.array_element().get_pointer_to();
        }

        outline_data_item.set_in_outline_type(synthesized_type);
        outline_data_item.set_field_type(Type::get_void_type().get_pointer_to());

        outline_data_item.set_item_kind(OutlineDataItem::ITEM_KIND_DATA_ADDRESS);

        if (outline_data_item.get_symbol().get_type().is_array()
                && outline_data_item.get_sharing() == OutlineDataItem::SHARING_CAPTURE)
        {
            outline_data_item.set_allocation_policy(
                    OutlineDataItem::AllocationPolicyFlags(
                        (outline_data_item.get_allocation_policy() | OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED)));
        }
    }

    void LoweringVisitor::fortran_handle_vla_type(
            OutlineDataItem& outline_data_item,
            TL::Type field_type,
            TL::Symbol field_symbol,
            TL::Scope class_scope, 
            TL::Symbol new_class_symbol,
            TL::Type new_class_type,
            TL::ObjectList<TL::Symbol>& new_symbols,
            const std::string& filename, 
            int line)
    {
        if (field_type.is_any_reference())
            field_type = field_type.references_to();

        bool is_pointer = false;

        if (field_type.is_pointer())
        {
            is_pointer = true;
            field_type = field_type.points_to();
        }

        TL::Type t = field_type;
        if (t.array_is_vla())
        {
            while (t.is_array())
            {
                Nodecl::NodeclBase lower, upper;
                t.array_get_bounds(lower, upper);

                if (!lower.is_null()
                        && lower.is<Nodecl::SavedExpr>())
                {
                    TL::Symbol saved_symbol = lower.as<Nodecl::SavedExpr>().get_symbol();
                    new_symbols.append(saved_symbol);

                    TL::Symbol vla_field = class_scope.new_symbol(saved_symbol.get_name());
                    vla_field.get_internal_symbol()->kind = SK_VARIABLE;
                    vla_field.get_internal_symbol()->type_information = saved_symbol.get_type().get_internal_type();

                    vla_field.get_internal_symbol()->entity_specs.class_type = ::get_user_defined_type(new_class_symbol.get_internal_symbol());

                    vla_field.get_internal_symbol()->entity_specs.access = AS_PUBLIC;

                    vla_field.get_internal_symbol()->file = uniquestr(filename.c_str());
                    vla_field.get_internal_symbol()->line = line;

                    class_type_add_member(new_class_type.get_internal_type(), vla_field.get_internal_symbol());
                }

                if (!upper.is_null()
                        && upper.is<Nodecl::SavedExpr>())
                {
                    TL::Symbol saved_symbol = upper.as<Nodecl::SavedExpr>().get_symbol();
                    new_symbols.append(saved_symbol);

                    TL::Symbol vla_field = class_scope.new_symbol(saved_symbol.get_name());
                    vla_field.get_internal_symbol()->kind = SK_VARIABLE;
                    vla_field.get_internal_symbol()->type_information = saved_symbol.get_type().get_internal_type();

                    vla_field.get_internal_symbol()->entity_specs.class_type = ::get_user_defined_type(new_class_symbol.get_internal_symbol());

                    vla_field.get_internal_symbol()->entity_specs.access = AS_PUBLIC;

                    vla_field.get_internal_symbol()->file = uniquestr(filename.c_str());
                    vla_field.get_internal_symbol()->line = line;

                    class_type_add_member(new_class_type.get_internal_type(), vla_field.get_internal_symbol());
                }

                t = t.array_element();
            }
        }

        // Rebuild the array type as an unbounded array type
        int k = 0;
        while (field_type.is_array())
        {
            field_type = field_type.array_element();
            k++;
        }

        while (k > 0)
        {
            field_type = field_type.get_array_to_with_descriptor(
                    Nodecl::NodeclBase::null(), 
                    Nodecl::NodeclBase::null(),
                    // This scope does not matter very much
                    class_scope);
            k--;
        }

        if (is_pointer)
        {
            field_type = field_type.get_pointer_to();
        }
        else
        {
            // Make the field an ALLOCATABLE
            field_symbol.get_internal_symbol()->entity_specs.is_allocatable = 1;
        }

        outline_data_item.set_allocation_policy(OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE);
        outline_data_item.set_field_type(field_type);
    }

    TL::Symbol LoweringVisitor::declare_argument_structure(OutlineInfo& outline_info, Nodecl::NodeclBase construct)
    {
        // Come up with a unique name
        Counter& counter = CounterManager::get_counter("nanos++-struct");
        std::string structure_name;

        std::stringstream ss;
        ss << "nanos_args_" << (int)counter << "_t";
        counter++;

        if (IS_C_LANGUAGE)
        {
            // We need an extra 'struct '
            structure_name = "struct " + ss.str();
        }
        else
        {
            structure_name = ss.str();
        }

        TL::ObjectList<OutlineDataItem> &data_items = outline_info.get_data_items();

        // FIXME - Wrap lots of things
        TL::Scope sc(CURRENT_COMPILED_FILE->global_decl_context);
        TL::Symbol new_class_symbol = sc.new_symbol(structure_name);
        new_class_symbol.get_internal_symbol()->kind = SK_CLASS;
        new_class_symbol.get_internal_symbol()->entity_specs.is_user_declared = 1;

        type_t* new_class_type = get_new_class_type(sc.get_decl_context(), TT_STRUCT);
        decl_context_t class_context = new_class_context(sc.get_decl_context(), new_class_symbol.get_internal_symbol());
        TL::Scope class_scope(class_context);

        class_type_set_inner_context(new_class_type, class_context);

        new_class_symbol.get_internal_symbol()->type_information = new_class_type;

        TL::ObjectList<TL::Symbol> extra_symbols;

        for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            TL::Symbol field = class_scope.new_symbol(it->get_field_name());
            field.get_internal_symbol()->kind = SK_VARIABLE;
            field.get_internal_symbol()->entity_specs.is_user_declared = 1;

            TL::Type field_type = it->get_field_type();

            field.get_internal_symbol()->entity_specs.is_member = 1;
            field.get_internal_symbol()->entity_specs.class_type = ::get_user_defined_type(new_class_symbol.get_internal_symbol());
            field.get_internal_symbol()->entity_specs.access = AS_PUBLIC;

            field.get_internal_symbol()->file = uniquestr(construct.get_filename().c_str());
            field.get_internal_symbol()->line = construct.get_line();

            // Language specific parts
            if (IS_FORTRAN_LANGUAGE)
            {
                Type field_type_no_ptr = field_type;

                if (field_type_no_ptr.is_pointer())
                    field_type_no_ptr = field_type_no_ptr.points_to();

                // Fix the type for Fortran arrays
                if (field_type_no_ptr.is_array()
                        && (field_type_no_ptr.array_requires_descriptor()
                            || field_type_no_ptr.array_is_vla()))
                {
                    fortran_handle_vla_type(*it, 
                            field_type,
                            field,
                            class_scope,
                            new_class_symbol, 
                            new_class_type,
                            extra_symbols,
                            construct.get_filename(),
                            construct.get_line());

                    // Fix the type for VLAs
                    field_type = it->get_field_type();
                }
            }
            else if (IS_C_LANGUAGE
                    || IS_CXX_LANGUAGE)
            {
                if (c_requires_vla_handling(*it))
                {
                    c_handle_vla_type(*it, 
                            class_scope,
                            new_class_symbol, 
                            new_class_type,
                            extra_symbols,
                            construct.get_filename(),
                            construct.get_line());

                    // Fix the type for VLAs
                    field_type = it->get_field_type();
                }
            }

            field.get_internal_symbol()->type_information = field_type.get_internal_type();
            class_type_add_member(new_class_type, field.get_internal_symbol());
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = extra_symbols.begin();
                it != extra_symbols.end();
                it++)
        {
            OutlineDataItem &data_item = outline_info.get_entity_for_symbol(*it);
            data_item.set_item_kind(OutlineDataItem::ITEM_KIND_DATA_DIMENSION);
            data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE);
            data_item.set_field_type(it->get_type());
        }

        set_is_complete_type(new_class_type, 1);

        nodecl_t nodecl_output = nodecl_null();
        finish_class_type(new_class_type, 
                ::get_user_defined_type(new_class_symbol.get_internal_symbol()),
                sc.get_decl_context(), 
                construct.get_filename().c_str(),
                construct.get_line(),
                &nodecl_output);
        set_is_complete_type(new_class_type, /* is_complete */ 1);
        set_is_complete_type(get_actual_class_type(new_class_type), /* is_complete */ 1);

        if (!nodecl_is_null(nodecl_output))
        {
            std::cerr << "FIXME: finished class issues nonempty nodecl" << std::endl; 
        }

        return new_class_symbol;
    }
} }
