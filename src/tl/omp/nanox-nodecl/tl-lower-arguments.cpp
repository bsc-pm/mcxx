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

#include "fortran03-typeutils.h"

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

    void LoweringVisitor::handle_vla_saved_expr(Nodecl::NodeclBase saved_expr, OutlineInfo& outline_info)
    {
        OutlineDataItem& outline_data_item = outline_info.get_entity_for_symbol(saved_expr.get_symbol());
        outline_data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE);
        outline_data_item.set_field_type(saved_expr.get_type().get_unqualified_type());
    }

    void LoweringVisitor::handle_vla_type_rec(TL::Type t, OutlineInfo& outline_info,
            OutlineDataItem& outline_data_item)
    {
        if (t.is_array())
        {
            if (IS_C_LANGUAGE
                    || IS_CXX_LANGUAGE)
            {
                Nodecl::NodeclBase size = t.array_get_size();

                if (size.is<Nodecl::Symbol>()
                        && size.get_symbol().is_saved_expression())
                {
                    handle_vla_saved_expr(size, outline_info);
                }
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                Nodecl::NodeclBase lower, upper;
                t.array_get_bounds(lower, upper);

                if (lower.is<Nodecl::Symbol>()
                        && lower.get_symbol().is_saved_expression())
                {
                    // VLA case for lower bound
                    handle_vla_saved_expr(lower, outline_info);
                }
                else if (lower.is_null()
                        && outline_data_item.get_symbol().is_valid()
                        && outline_data_item.get_symbol().is_allocatable())
                {
                    if (outline_data_item.get_sharing() == OutlineDataItem::SHARING_PRIVATE)
                    {
                        // Note that the lower bound is always kept in some way
                        Counter& counter = CounterManager::get_counter("array-lower-boundaries");
                        std::string structure_name;

                        std::stringstream ss;
                        ss << "mcc_lower_bound_" << (int)counter;
                        counter++;

                        OutlineDataItem& outline_item = outline_info.append_field(ss.str(), fortran_get_default_integer_type());
                        outline_item.set_sharing(OutlineDataItem::SHARING_CAPTURE);
                    }
                }

                if (upper.is<Nodecl::Symbol>()
                        && upper.get_symbol().is_saved_expression())
                {
                    // VLA case for upper bound
                    handle_vla_saved_expr(upper, outline_info);
                }
                // If it is a private ALLOCATABLE, we keep the upper bound too
                else if (upper.is_null()
                        && outline_data_item.get_symbol().is_valid()
                        && outline_data_item.get_symbol().is_allocatable())
                {
                    if (outline_data_item.get_sharing() == OutlineDataItem::SHARING_PRIVATE)
                    {
                        Counter& counter = CounterManager::get_counter("array-upper-boundaries");
                        std::string structure_name;

                        std::stringstream ss;
                        ss << "mcc_upper_bound_" << (int)counter;
                        counter++;

                        OutlineDataItem& outline_item = outline_info.append_field(ss.str(), fortran_get_default_integer_type());
                        outline_item.set_sharing(OutlineDataItem::SHARING_CAPTURE);
                    }
                }
            }
            handle_vla_type_rec(t.array_element(), outline_info, outline_data_item);
        }
        else if (t.is_pointer())
        {
            handle_vla_type_rec(t.points_to(), outline_info, outline_data_item);
        }
        else if (t.is_any_reference())
        {
            handle_vla_type_rec(t.references_to(), outline_info, outline_data_item);
        }
    }

    void LoweringVisitor::handle_vla_entity(OutlineDataItem& data_item, OutlineInfo& outline_info)
    {
        if (!data_item.get_symbol().is_valid())
            return;

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            TL::Type t = data_item.get_symbol().get_type();

            if (c_type_needs_vla_handling(t))
            {
                handle_vla_type_rec(t, outline_info, data_item);

                data_item.set_field_type(Type::get_void_type().get_pointer_to());
                data_item.set_item_kind(OutlineDataItem::ITEM_KIND_DATA_ADDRESS);

                if (data_item.get_sharing() == OutlineDataItem::SHARING_CAPTURE)
                {
                    data_item.set_allocation_policy(OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED);
                }
            }
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            TL::Type t = data_item.get_symbol().get_type();

            bool is_lvalue_ref = false;
            if (is_lvalue_ref = t.is_lvalue_reference())
                t = t.references_to();

            if (t.is_array())
            {
                handle_vla_type_rec(t, outline_info, data_item);

                struct BuildArray
                {
                    static Type get_assumed_array(TL::Type t)
                    {
                        if (t.is_array())
                        {
                            Nodecl::NodeclBase lower, upper;
                            t.array_get_bounds(lower, upper);

                            TL::Type element_type = get_assumed_array(t.array_element());

                            if (!lower.is_null()
                                    && !upper.is_null()
                                    && lower.is_constant()
                                    && upper.is_constant()
                                    && (!element_type.is_array()
                                        || !element_type.array_requires_descriptor()))
                            {
                                return element_type.get_array_to(
                                        lower,
                                        upper,
                                        CURRENT_COMPILED_FILE->global_decl_context);
                            }
                            else
                            {
                                return element_type.get_array_to_with_descriptor(
                                        lower,
                                        Nodecl::NodeclBase::null(),
                                        CURRENT_COMPILED_FILE->global_decl_context);
                            }
                        }
                        else
                        {
                            return t;
                        }
                    }
                };

                switch (data_item.get_sharing())
                {
                    case OutlineDataItem::SHARING_PRIVATE:
                        {
                            // We can use the original array type, even if VLA
                            data_item.set_in_outline_type(t);
                            // Since it does not appear in the structure, void is enough
                            data_item.set_field_type(Type::get_void_type());
                            break;
                        }
                    case OutlineDataItem::SHARING_CAPTURE:
                        {
                            TL::Type assumed_array_descriptor = BuildArray::get_assumed_array(t);
                            int rank = ::fortran_get_rank_of_type(t.get_internal_type());
                            TL::Type deferred_array_descriptor = TL::Type(
                                    ::fortran_get_n_ranked_type_with_descriptor(
                                        ::fortran_get_rank0_type(t.get_internal_type()), rank, CURRENT_COMPILED_FILE->global_decl_context)
                                    );

                            data_item.set_in_outline_type(assumed_array_descriptor.get_lvalue_reference_to());
                            data_item.set_field_type(deferred_array_descriptor);

                            // We want the field be ALLOCATABLE
                            data_item.set_allocation_policy(OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE);
                            break;
                        }
                    case OutlineDataItem::SHARING_SHARED:
                        {
                            int rank = ::fortran_get_rank_of_type(t.get_internal_type());
                            TL::Type deferred_array_descriptor = TL::Type(
                                    ::fortran_get_n_ranked_type_with_descriptor(
                                        ::fortran_get_rank0_type(t.get_internal_type()), rank, CURRENT_COMPILED_FILE->global_decl_context)
                                    );

                            // We do not modify the outline type because we want to preserve the original type of the item

                            data_item.set_field_type(deferred_array_descriptor.get_pointer_to());
                            break;
                        }
                    default:
                        {
                            break;
                        }
                }
            }
        }
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

        TL::ObjectList<OutlineDataItem*> data_items = outline_info.get_data_items();
        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            // VLA types
            handle_vla_entity(*(*it), outline_info);
        }

        // Update again
        data_items = outline_info.get_data_items();

        // FIXME - Wrap lots of things
        TL::Scope sc(construct.retrieve_context()
                .get_decl_context()
                .namespace_scope->related_entry // SK_NAMESPACE
                ->decl_context);

        TL::Symbol related_symbol = construct.retrieve_context().get_related_symbol();
        if (related_symbol.is_member())
        {
            // Class scope
            sc = ::class_type_get_inner_context(related_symbol.get_class_type().get_internal_type());
        }
        else if (related_symbol.is_in_module())
        {
            // Scope of the module
            sc = related_symbol.in_module().get_related_scope();
        }

        TL::Symbol new_class_symbol = sc.new_symbol(structure_name);
        new_class_symbol.get_internal_symbol()->kind = SK_CLASS;
        new_class_symbol.get_internal_symbol()->entity_specs.is_user_declared = 1;

        type_t* new_class_type = get_new_class_type(sc.get_decl_context(), TT_STRUCT);
        decl_context_t class_context = new_class_context(sc.get_decl_context(), new_class_symbol.get_internal_symbol());
        TL::Scope class_scope(class_context);

        class_type_set_inner_context(new_class_type, class_context);

        new_class_symbol.get_internal_symbol()->type_information = new_class_type;

        for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            // Privates are ignored here
            if ((*it)->get_sharing() == OutlineDataItem::SHARING_PRIVATE)
                continue;

            TL::Symbol field = class_scope.new_symbol((*it)->get_field_name());
            field.get_internal_symbol()->kind = SK_VARIABLE;
            field.get_internal_symbol()->entity_specs.is_user_declared = 1;

            TL::Type field_type = (*it)->get_field_type();

            if (IS_CXX_LANGUAGE || IS_C_LANGUAGE)
            {
                if (field_type.is_const())
                {
                    field_type = field_type.get_unqualified_type();
                }
            }
            field.get_internal_symbol()->type_information = field_type.get_internal_type();

            field.get_internal_symbol()->entity_specs.is_member = 1;
            field.get_internal_symbol()->entity_specs.class_type = ::get_user_defined_type(new_class_symbol.get_internal_symbol());
            field.get_internal_symbol()->entity_specs.access = AS_PUBLIC;

            field.get_internal_symbol()->file = uniquestr(construct.get_filename().c_str());
            field.get_internal_symbol()->line = construct.get_line();

            // Language specific parts
            if (IS_FORTRAN_LANGUAGE)
            {
                if (((*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE)
                        == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE)
                {
                    field.get_internal_symbol()->entity_specs.is_allocatable = 1;
                }
            }

            class_type_add_member(new_class_type, field.get_internal_symbol());
        }

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

        if (related_symbol.is_member())
        {
            new_class_symbol.get_internal_symbol()->entity_specs.is_member = 1;
            new_class_symbol.get_internal_symbol()->entity_specs.class_type 
                = related_symbol.get_class_type().get_internal_type();
            new_class_symbol.get_internal_symbol()->entity_specs.access = AS_PUBLIC;

            new_class_symbol.get_internal_symbol()->entity_specs.is_defined_inside_class_specifier = 
                related_symbol.get_internal_symbol()->entity_specs.is_defined_inside_class_specifier;

            ::class_type_add_member_before(
                    related_symbol.get_class_type().get_internal_type(), 
                    related_symbol.get_internal_symbol(),
                    new_class_symbol.get_internal_symbol());
        }
        else if (related_symbol.is_in_module())
        {
            // Add the newly created argument as a structure
            TL::Symbol module = construct.retrieve_context().get_related_symbol().in_module();

            new_class_symbol.get_internal_symbol()->entity_specs.in_module = module.get_internal_symbol();

            P_LIST_ADD(
                    module.get_internal_symbol()->entity_specs.related_symbols,
                    module.get_internal_symbol()->entity_specs.num_related_symbols,
                    new_class_symbol.get_internal_symbol());
        }

        CXX_LANGUAGE()
        {
            Nodecl::NodeclBase nodecl_decl = Nodecl::CxxDef::make(
                    /* optative context */ nodecl_null(),
                    new_class_symbol,
                    construct.get_filename(),
                    construct.get_line());
            Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, nodecl_decl);
        }

        return new_class_symbol;
    }
} }
