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


#include "tl-outline-info.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-datareference.hpp"
#include "tl-counters.hpp"
#include "tl-predicateutils.hpp"
#include "codegen-phase.hpp"
#include "cxx-diagnostic.h"
#include "cxx-exprtype.h"
#include "fortran03-typeutils.h"
#include "tl-target-information.hpp"
#include "tl-counters.hpp"
#include "fortran03-typeutils.h"
#include "fortran03-typeenviron.h"
#include "tl-nanox-ptr.hpp"

namespace TL { namespace Nanox {

    std::string OutlineInfo::get_field_name(std::string name)
    {
        // Adjust names first
        if (IS_CXX_LANGUAGE
                && name == "this")
            name = "this_";

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            // Builtin identifiers that became reserved names in later versions
            // of gcc
            if (name == "__PRETTY_FUNCTION__" // g++
                    || name == "__FUNCTION__" // gcc
                    || name == "__MERCURIUM_PRETTY_FUNCTION__") // Mercurium
                name = strtolower(name.c_str());
            else if (name == "__func__") // C99
                name = "__function__";
        }

        int times_name_appears = 0;
        for (ObjectList<OutlineDataItem*>::iterator it = _data_env_items.begin();
                it != _data_env_items.end();
                it++)
        {
            if ((*it)->get_symbol().is_valid()
                    && (*it)->get_symbol().get_name() == name)
            {
                times_name_appears++;
            }
        }

        std::stringstream ss;
        ss << name;
        if (times_name_appears > 0)
            ss << "_" << times_name_appears;

        return ss.str();
    }

    OutlineDataItem& OutlineInfo::get_entity_for_symbol(TL::Symbol sym)
    {
        bool dummy;
        return get_entity_for_symbol(sym, dummy);
    }

    OutlineDataItem& OutlineInfo::get_entity_for_symbol(TL::Symbol sym, bool &is_new)
    {
        is_new = false;
        for (ObjectList<OutlineDataItem*>::iterator it = _data_env_items.begin();
                it != _data_env_items.end();
                it++)
        {
            if ((*it)->get_symbol() == sym)
                return *(*it);
        }

        std::string field_name = get_field_name(sym.get_name());
        OutlineDataItem* env_item = new OutlineDataItem(sym, field_name);
        is_new = true;

        _data_env_items.append(env_item);
        return (*_data_env_items.back());
    }

    void OutlineInfoRegisterEntities::add_shared_opaque(Symbol sym)
    {
        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym, is_new);

        outline_info.set_sharing(OutlineDataItem::SHARING_SHARED);

        Type t = sym.get_type();
        if (t.is_any_reference())
        {
            t = t.references_to();
        }

        outline_info.set_field_type(TL::Type::get_void_type().get_pointer_to());

        if (is_new)
        {
            TL::Type in_outline_type = t.get_lvalue_reference_to();
            in_outline_type = add_extra_dimensions(sym, in_outline_type, &outline_info);

            outline_info.set_in_outline_type(in_outline_type);

            _outline_info.move_at_end(outline_info);
        }
    }

    void OutlineInfoRegisterEntities::add_shared_opaque_and_captured_array_descriptor(Symbol sym)
    {
        ERROR_CONDITION(!IS_FORTRAN_LANGUAGE, "This function is only for Fortran", 0);

        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym, is_new);

        outline_info.set_sharing(OutlineDataItem::SHARING_SHARED);

        Type t = sym.get_type();
        if (t.is_any_reference())
        {
            t = t.references_to();
        }

        TL::Type void_pointer = TL::Type::get_void_type().get_pointer_to();
        outline_info.set_field_type(void_pointer);

        // Finalize this entity
        if (is_new)
        {
            TL::Type in_outline_type = t.get_lvalue_reference_to();
            in_outline_type = add_extra_dimensions(sym, in_outline_type, &outline_info);

            outline_info.set_in_outline_type(in_outline_type);

            _outline_info.move_at_end(outline_info);

            // Capture the descriptor
            TL::Type array_type = sym.get_type().no_ref();
            if (array_type.is_pointer())
            {
                array_type = array_type.points_to();
            }

            ERROR_CONDITION(!array_type.is_fortran_array(), "Invalid type", 0);

            TL::Type suitable_integer = fortran_choose_int_type_from_kind(void_pointer.get_size());

            size_t size_of_array_descriptor = fortran_size_of_array_descriptor(
                    fortran_get_rank0_type(array_type.get_internal_type()),
                    fortran_get_rank_of_type(array_type.get_internal_type()));

            ERROR_CONDITION((size_of_array_descriptor % suitable_integer.get_size()) != 0,
                    "The size of the descriptor is not a multiple of the integer type", 0);

            int num_items = size_of_array_descriptor / suitable_integer.get_size();

            Counter& counter = CounterManager::get_counter("array-descriptor-copies");
            std::stringstream ss;
            ss << outline_info.get_field_name() << "_descriptor_" << (int)counter;
            counter++;

            TL::Symbol captured_array_descriptor = _sc.new_symbol(ss.str());
            captured_array_descriptor.get_internal_symbol()->kind = SK_VARIABLE;
            captured_array_descriptor.get_internal_symbol()->type_information =
                get_array_type_bounds(suitable_integer.get_internal_type(),
                        const_value_to_nodecl(const_value_get_signed_int(1)),
                        const_value_to_nodecl(const_value_get_signed_int(num_items)),
                        _sc.get_decl_context());

            TL::Symbol ptr_of_sym = get_function_ptr_of(sym, _sc);

            Source src;
            src 
                << "{"
                << "nanos_err_t err;"
                << "err = nanos_memcpy(" << captured_array_descriptor.get_name() << ","
                <<     as_expression(ptr_of_sym.make_nodecl(/* set_ref_type */ true))
                <<            "(" << as_expression(sym.make_nodecl(/* set_ref_type */ true)) << "),"
                <<     size_of_array_descriptor
                << ");"
                << "if (err != NANOS_OK) nanos_handle_error(err);"
                << "}"
                ;

            Source::source_language = SourceLanguage::C;
            Nodecl::NodeclBase prepare_capture = src.parse_statement(_sc);
            Source::source_language = SourceLanguage::Fortran;

            this->add_capture(captured_array_descriptor);
            OutlineDataItem &captured_array_descriptor_info = _outline_info.get_entity_for_symbol(captured_array_descriptor);
            captured_array_descriptor_info.set_prepare_capture_code(prepare_capture);
            captured_array_descriptor_info.set_in_outline_type(outline_info.get_in_outline_type());
            captured_array_descriptor_info.set_is_copy_of_array_descriptor_allocatable(sym.is_allocatable());

            outline_info.set_copy_of_array_descriptor(&captured_array_descriptor_info);
        }
    }

    void OutlineInfoRegisterEntities::add_shared(Symbol sym)
    {
        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym, is_new);

        outline_info.set_sharing(OutlineDataItem::SHARING_SHARED);

        Type t = sym.get_type();
        if (t.is_any_reference())
        {
            t = t.references_to();
        }

        if (IS_CXX_LANGUAGE
                && sym.get_name() == "this")
        {
            outline_info.set_field_type(t.get_unqualified_type_but_keep_restrict());
            outline_info.set_in_outline_type(t.get_unqualified_type_but_keep_restrict());
        }
        else
        {
            outline_info.set_field_type(t.get_pointer_to());

            if (is_new)
            {
                TL::Type in_outline_type = t.get_lvalue_reference_to();
                in_outline_type = add_extra_dimensions(sym, in_outline_type, &outline_info);

                outline_info.set_in_outline_type(in_outline_type);

                _outline_info.move_at_end(outline_info);
            }
        }
    }

    void OutlineInfoRegisterEntities::add_shared_with_capture(Symbol sym)
    {
        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym, is_new);

        outline_info.set_sharing(OutlineDataItem::SHARING_SHARED_WITH_CAPTURE);

        Type t = sym.get_type();
        if (t.is_any_reference())
        {
            t = t.references_to();
        }

        outline_info.set_field_type(t);

        if (is_new)
        {
            TL::Type in_outline_type = t;
            in_outline_type = add_extra_dimensions(sym, in_outline_type, &outline_info);

            outline_info.set_in_outline_type(in_outline_type);

            _outline_info.move_at_end(outline_info);
        }


        Nodecl::Symbol symbol_nodecl = Nodecl::Symbol::make(sym);
        symbol_nodecl.set_type(sym.get_type());

        TL::DataReference data_ref(
                Nodecl::Dereference::make(
                    symbol_nodecl,
                    sym.get_type().points_to(),
                    symbol_nodecl.get_locus()));

        outline_info.get_dependences().append(OutlineDataItem::DependencyItem(data_ref, OutlineDataItem::DEP_IN));
    }


    void OutlineInfoRegisterEntities::add_shared_alloca(Symbol sym)
    {
        ERROR_CONDITION(!IS_C_LANGUAGE && !IS_CXX_LANGUAGE, "This function is only for C/C++", 0);

        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym, is_new);

        outline_info.set_sharing(OutlineDataItem::SHARING_SHARED_ALLOCA);

        if (is_new)
        {
            Type t = sym.get_type();
            if (t.is_any_reference())
            {
                t = t.references_to();
            }

            if (t.is_array())
            {
                t = t.array_element().get_pointer_to();
            }

            outline_info.set_field_type(t.get_unqualified_type_but_keep_restrict());

            TL::Type in_outline_type = t.get_unqualified_type_but_keep_restrict();
            in_outline_type = add_extra_dimensions(sym, in_outline_type, &outline_info);

            outline_info.set_in_outline_type(in_outline_type);

            _outline_info.move_at_end(outline_info);
        }
    }

    // Only used in task expressions to store the return results
    void OutlineInfoRegisterEntities::add_alloca(Symbol sym, TL::DataReference& data_ref)
    {
        ERROR_CONDITION(!IS_C_LANGUAGE && !IS_CXX_LANGUAGE, "This function is only for C/C++", 0);

        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym, is_new);

        outline_info.set_sharing(OutlineDataItem::SHARING_ALLOCA);

        if (is_new)
        {
            Type t = data_ref.get_type();
            if (t.is_any_reference())
            {
                t = t.references_to();
            }

            if (t.is_array())
            {
                t = t.array_element().get_pointer_to();
            }

            outline_info.set_field_type(t.get_unqualified_type_but_keep_restrict());

            TL::Type in_outline_type = t.get_unqualified_type_but_keep_restrict();
            in_outline_type = add_extra_dimensions(sym, in_outline_type, &outline_info);

            outline_info.set_in_outline_type(in_outline_type);

            _outline_info.move_at_end(outline_info);
        }
    }

    void OutlineInfoRegisterEntities::add_capture_address(Symbol sym, TL::DataReference& data_ref)
    {
        ERROR_CONDITION(!IS_C_LANGUAGE && !IS_CXX_LANGUAGE, "This function is only for C/C++", 0);

        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym, is_new);

        outline_info.set_sharing(OutlineDataItem::SHARING_CAPTURE_ADDRESS);

        outline_info.set_base_address_expression(data_ref.get_address_of_symbol());

        if (is_new)
        {
            Type t = sym.get_type();
            if (t.is_any_reference())
            {
                t = t.references_to();
            }

            if (t.is_array())
            {
                t = t.array_element().get_pointer_to();
            }

            outline_info.set_field_type(t.get_unqualified_type_but_keep_restrict());

            TL::Type in_outline_type = t.get_unqualified_type_but_keep_restrict();
            in_outline_type = add_extra_dimensions(sym, in_outline_type, &outline_info);

            outline_info.set_in_outline_type(in_outline_type);

            _outline_info.move_at_end(outline_info);
        }
    }

    void OutlineInfoRegisterEntities::add_private(Symbol sym)
    {
        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym, is_new);
        outline_info.set_sharing(OutlineDataItem::SHARING_PRIVATE);

        TL::Type t = sym.get_type();
        if (t.is_any_reference())
            t = t.references_to();

        if (is_new)
        {
            TL::Type in_outline_type = add_extra_dimensions(sym, t, &outline_info);

            outline_info.set_in_outline_type(in_outline_type);

            _outline_info.move_at_end(outline_info);
        }

        outline_info.set_private_type(t);
    }

    void OutlineInfoRegisterEntities::add_shared_with_private_storage(Symbol sym, bool captured)
    {
        if (captured)
        {
            this->add_capture(sym);
        }
        else
        {
            this->add_private(sym);
        }

        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym);
        outline_info.set_is_lastprivate(true);

        TL::Type t = sym.get_type();
        if (t.is_any_reference())
            t = t.references_to();

        outline_info.set_private_type(t);

        // Add a new symbol just to hold the address
        TL::Symbol new_addr_symbol = _sc.new_symbol(sym.get_name() + "_addr");
        new_addr_symbol.get_internal_symbol()->kind = SK_VARIABLE;
        new_addr_symbol.get_internal_symbol()->type_information = t.get_pointer_to().get_internal_type();

        Nodecl::Symbol sym_ref = Nodecl::Symbol::make(sym, make_locus("", 0, 0));
        sym_ref.set_type(t.get_lvalue_reference_to());

        this->add_capture_with_value(
                new_addr_symbol,
                Nodecl::Reference::make(sym_ref, t.get_pointer_to(), make_locus("", 0, 0)));
    }

    TL::Type OutlineInfoRegisterEntities::add_extra_dimensions(TL::Symbol sym, TL::Type t)
    {
        return add_extra_dimensions(sym, t, NULL);
    }

    TL::Type OutlineInfoRegisterEntities::add_extra_dimensions(TL::Symbol sym, TL::Type t,
            OutlineDataItem* outline_data_item)
    {
        bool make_allocatable = false;
        Nodecl::NodeclBase conditional_bound;
        TL::Type res = add_extra_dimensions_rec(sym, t, outline_data_item, make_allocatable, conditional_bound);
        if (t.is_any_reference())
            t = t.references_to();

        if (t.is_array()
                && outline_data_item != NULL
                && outline_data_item->get_sharing() == OutlineDataItem::SHARING_CAPTURE)
        {
            if (!IS_FORTRAN_LANGUAGE // Fortran never overallocates in the struct
                    && t.array_is_vla())
            {
                outline_data_item->set_allocation_policy(
                        outline_data_item->get_allocation_policy() |
                        OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED);
                outline_data_item->set_field_type(
                        TL::Type::get_void_type().get_pointer_to());

                CXX_LANGUAGE()
                {
                    // C++ does not quite support VLAs
                    res = TL::Type::get_void_type().get_pointer_to();
                }
            }
        }
        else
        {
            if (t.depends_on_nonconstant_values())
            {
                outline_data_item->set_field_type(
                        TL::Type::get_void_type().get_pointer_to());

                CXX_LANGUAGE()
                {
                    // C++ does not quite support VLAs
                    res = TL::Type::get_void_type().get_pointer_to();
                }
            }
        }
        return res;
    }

    TL::Type OutlineInfoRegisterEntities::add_extra_dimensions_rec(TL::Symbol sym, TL::Type t,
            OutlineDataItem* outline_data_item,
            bool &make_allocatable,
            Nodecl::NodeclBase &conditional_bound)
    {
        if (t.is_lvalue_reference())
        {
            TL::Type res = add_extra_dimensions_rec(sym, t.references_to(), outline_data_item, make_allocatable, conditional_bound);
            return res.get_lvalue_reference_to();
        }
        else if (t.is_pointer())
        {
            TL::Type res = add_extra_dimensions_rec(sym, t.points_to(), outline_data_item, make_allocatable, conditional_bound);
            return res.get_pointer_to().get_as_qualified_as(t);
        }
        else if (t.is_function())
        {
            // Not handled
            return t;
        }
        else if (t.is_class())
        {
            // FIXME - Classes may have "VLA" components
            return t;
        }
        else if ((IS_C_LANGUAGE
                    || IS_CXX_LANGUAGE)
                && t.is_array())
        {
            Nodecl::NodeclBase array_size = t.array_get_size();

            if (array_size.is<Nodecl::Symbol>()
                    && array_size.get_symbol().is_saved_expression())
            {
                this->add_capture(array_size.get_symbol());
            }

            t = add_extra_dimensions_rec(sym, t.array_element(), outline_data_item, make_allocatable, conditional_bound);
            return t.get_array_to(array_size, _sc);
        }
        else if (IS_FORTRAN_LANGUAGE
                && t.is_fortran_array())
        {
            Nodecl::NodeclBase lower, upper;

            t.array_get_bounds(lower, upper);

            Nodecl::NodeclBase result_lower, result_upper;

            // If the symbol is a shared allocatable we want the original type
            if (sym.is_allocatable()
                    && outline_data_item != NULL
                    && (outline_data_item->get_sharing() == OutlineDataItem::SHARING_SHARED))
                return t;

            if (sym.is_allocatable()
                    && outline_data_item != NULL
                    && ((outline_data_item->get_sharing() == OutlineDataItem::SHARING_PRIVATE)
                        || (outline_data_item->get_sharing() == OutlineDataItem::SHARING_REDUCTION))
                    && conditional_bound.is_null())
            {
                Counter& counter = CounterManager::get_counter("array-allocatable");
                std::stringstream ss;
                ss << "mcc_is_allocated_" << (int)counter++;

                TL::Symbol is_allocated_sym = _sc.new_symbol(ss.str());
                is_allocated_sym.get_internal_symbol()->kind = SK_VARIABLE;
                is_allocated_sym.get_internal_symbol()->type_information = fortran_get_default_logical_type();

                Nodecl::NodeclBase symbol_ref = Nodecl::Symbol::make(sym, make_locus("", 0, 0));
                TL::Type sym_type = sym.get_type();

                if (!sym_type.is_any_reference())
                    sym_type = sym_type.get_lvalue_reference_to();
                symbol_ref.set_type(sym_type);

                Source allocated_src;
                allocated_src << "ALLOCATED(" << as_expression(symbol_ref) << ")";

                Nodecl::NodeclBase allocated_tree = allocated_src.parse_expression(_sc);

                this->add_capture_with_value(is_allocated_sym, allocated_tree);

                conditional_bound = allocated_tree;
            }

            if (sym.is_optional()
                    && conditional_bound.is_null())
            {
                Source present_src;
                present_src << "PRESENT(" << sym.get_name() << ")";

                Nodecl::NodeclBase allocated_tree = present_src.parse_expression(_sc);
                conditional_bound = allocated_tree;
            }

            if (lower.is_null())
            {
                if (t.array_requires_descriptor()
                        && sym.is_parameter()
                        && !sym.is_allocatable()
                        && !(sym.get_type().is_pointer()
                            || (sym.get_type().is_any_reference()
                                && sym.get_type().references_to().is_pointer())))
                {
                    // This is an assumed shape, the lower is actually one
                    result_lower = const_value_to_nodecl(const_value_get_one(4, 1));
                }
                else if (sym.get_type().is_pointer()
                        || (sym.get_type().is_any_reference()
                            && sym.get_type().references_to().is_pointer())
                        || sym.is_allocatable())
                {

                    Counter& counter = CounterManager::get_counter("array-lower-boundaries");
                    std::stringstream ss;
                    ss << "mcc_lower_bound_" << (int)counter++;

                    // This is a deferred shape, create a symbol
                    TL::Symbol bound_sym = _sc.new_symbol(ss.str());
                    bound_sym.get_internal_symbol()->kind = SK_VARIABLE;
                    bound_sym.get_internal_symbol()->type_information = get_signed_int_type();

                    int dim = fortran_get_rank_of_type(t.get_internal_type());

                    Nodecl::NodeclBase symbol_ref = Nodecl::Symbol::make(sym, make_locus("", 0, 0));
                    TL::Type sym_type = sym.get_type();
                    if (!sym_type.no_ref().is_pointer())
                    {
                        if (!sym_type.is_any_reference())
                            sym_type = sym_type.get_lvalue_reference_to();
                        symbol_ref.set_type(sym_type);
                    }
                    else
                    {
                        symbol_ref.set_type(sym_type);
                        symbol_ref = Nodecl::Dereference::make(
                                symbol_ref,
                                sym_type.no_ref().points_to().get_lvalue_reference_to());
                    }

                    Source lbound_src;
                    lbound_src << "LBOUND(" << as_expression(symbol_ref) << ", DIM=" << dim << ")";
                    
                    Nodecl::NodeclBase lbound_tree = lbound_src.parse_expression(_sc);

                    this->add_capture_with_value(bound_sym, lbound_tree, conditional_bound);

                    result_lower = Nodecl::Symbol::make(bound_sym, make_locus("", 0, 0));
                    result_lower.set_type(bound_sym.get_type().get_lvalue_reference_to());

                    make_allocatable = !sym_type.no_ref().is_pointer();
                }
            }
            else if (lower.is<Nodecl::Symbol>()
                    && lower.get_symbol().is_saved_expression())
            {
                this->add_capture(lower.get_symbol());
                result_lower = lower;

                make_allocatable = true;
            }
            else
            {
                ERROR_CONDITION(!lower.is_constant(), "This should be constant", 0);
                // This should be constant
                result_lower = lower;
            }

            if (upper.is_null())
            {
                if (t.array_requires_descriptor())
                {
                    // This is an assumed shape or deferred shape
                    Counter& counter = CounterManager::get_counter("array-upper-boundaries");
                    std::stringstream ss;
                    ss << "mcc_upper_bound_" << (int)counter++;

                    // This is a deferred shape, create a symbol
                    TL::Symbol bound_sym = _sc.new_symbol(ss.str());
                    bound_sym.get_internal_symbol()->kind = SK_VARIABLE;
                    bound_sym.get_internal_symbol()->type_information = get_signed_int_type();

                    int dim = fortran_get_rank_of_type(t.get_internal_type());

                    Nodecl::NodeclBase symbol_ref = Nodecl::Symbol::make(sym, make_locus("", 0, 0));
                    TL::Type sym_type = sym.get_type();
                    if (!sym_type.no_ref().is_pointer())
                    {
                        if (!sym_type.is_any_reference())
                            sym_type = sym_type.get_lvalue_reference_to();
                        symbol_ref.set_type(sym_type);
                    }
                    else
                    {
                        symbol_ref.set_type(sym_type);
                        symbol_ref = Nodecl::Dereference::make(
                                symbol_ref,
                                sym_type.no_ref().points_to().get_lvalue_reference_to());
                    }

                    Source ubound_src;
                    ubound_src << "UBOUND(" << as_expression(symbol_ref) << ", DIM=" << dim << ")";

                    Nodecl::NodeclBase ubound_tree = ubound_src.parse_expression(_sc);

                    this->add_capture_with_value(bound_sym, ubound_tree, conditional_bound);

                    result_upper = Nodecl::Symbol::make(bound_sym, make_locus("", 0, 0));
                    result_upper.set_type(bound_sym.get_type().get_lvalue_reference_to());

                    make_allocatable = !sym_type.no_ref().is_pointer();
                }
                else
                {
                    // This is an assumed size array, result_upper must remain null
                    if (outline_data_item != NULL)
                    {
                        // FIXME - We should check this earlier. In OpenMP::Core
                        if (outline_data_item->get_sharing() == OutlineDataItem::SHARING_CAPTURE)
                        {
                            error_printf("%s:%d: error: symbol '%s' cannot be FIRSTPRIVATE since it is an assumed size array\n",
                                    sym.get_filename().c_str(),
                                    sym.get_line(),
                                    sym.get_name().c_str());
                        }
                    }
                }
            }
            else if (upper.is<Nodecl::Symbol>()
                    && upper.get_symbol().is_saved_expression())
            {
                this->add_capture(upper.get_symbol());
                result_upper = upper;

                make_allocatable = true;
            }
            else
            {
                ERROR_CONDITION(!upper.is_constant(), "This should be constant", 0);
                // This should be constant
                result_upper = upper;
            }

            TL::Type res = add_extra_dimensions_rec(sym, t.array_element(), outline_data_item, make_allocatable, conditional_bound);

            if (upper.is_null()
                    && !t.array_requires_descriptor())
            {
                // This array is an assumed size, do not use a descriptor for it
                make_allocatable = false;
            }

            if (make_allocatable)
            {
                res = res.get_array_to_with_descriptor(result_lower, result_upper, _sc);
            }
            else
            {
                res = res.get_array_to(result_lower, result_upper, _sc);
            }

            if (make_allocatable
                    && outline_data_item != NULL)
            {
                if (outline_data_item->get_sharing() == OutlineDataItem::SHARING_CAPTURE)
                {
                    outline_data_item->set_allocation_policy(
                            outline_data_item->get_allocation_policy() |
                            OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE);
                    outline_data_item->set_field_type(
                            ::fortran_get_n_ranked_type_with_descriptor(
                                ::fortran_get_rank0_type(t.get_internal_type()),
                                ::fortran_get_rank_of_type(t.get_internal_type()), _sc.get_decl_context()));
                }
            }

            return res;
        }
        return t;
    }

    void OutlineInfoRegisterEntities::add_dependence(Nodecl::NodeclBase node, OutlineDataItem::DependencyDirectionality directionality)
    {
        TL::DataReference data_ref(node);
        if (data_ref.is_valid())
        {
            TL::Symbol sym = data_ref.get_base_symbol();

            if (directionality == OutlineDataItem::DEP_IN_ALLOCA)
            {
                add_shared_alloca(sym);
            }
            else
            {
                if (IS_FORTRAN_LANGUAGE)
                {
                    // For ALLOCATABLE and shared pointers, copy the descriptor
                    if ((sym.is_allocatable()
                            && sym.get_type().no_ref().is_fortran_array())
                            || (sym.get_type().no_ref().is_pointer()
                                && sym.get_type().no_ref().points_to().is_fortran_array()))
                    {
                        add_shared_opaque_and_captured_array_descriptor(sym);
                    }
                    else
                    {
                        add_shared_opaque(sym);
                    }
                }
                else if (data_ref.is<Nodecl::Symbol>())
                {
                    add_shared(sym);
                }
                else
                {
                    add_capture_address(sym, data_ref);
                }
            }

            OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym);
            outline_info.get_dependences().append(OutlineDataItem::DependencyItem(data_ref, directionality));
        }
        else
        {
            internal_error("%s: data reference '%s' must be valid at this point!\n",
                    node.get_locus_str().c_str(),
                    Codegen::get_current().codegen_to_str(node, node.retrieve_context()).c_str()
                    );
        }
    }

    void OutlineInfoRegisterEntities::add_dependences(Nodecl::List list, OutlineDataItem::DependencyDirectionality directionality)
    {
        for (Nodecl::List::iterator it = list.begin();
                it != list.end();
                it++)
        {
            add_dependence(*it, directionality);
        }
    }

    void OutlineInfoRegisterEntities::add_copies(Nodecl::List list, OutlineDataItem::CopyDirectionality copy_directionality)
    {
        for (Nodecl::List::iterator it = list.begin();
                it != list.end();
                it++)
        {
            TL::DataReference data_ref(*it);
            if (data_ref.is_valid())
            {
                TL::Symbol sym = data_ref.get_base_symbol();

                OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym);
                outline_info.get_copies().append(OutlineDataItem::CopyItem(data_ref, copy_directionality));
            }
            else
            {
                internal_error("%s: data reference '%s' must be valid at this point!\n",
                        it->get_locus_str().c_str(),
                        Codegen::get_current().codegen_to_str(*it, it->retrieve_context()).c_str()
                        );
            }
        }
    }

    void OutlineInfoRegisterEntities::add_capture(Symbol sym)
    {
        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym, is_new);
        outline_info.set_sharing(OutlineDataItem::SHARING_CAPTURE);

        Type t = sym.get_type();
        if (t.is_any_reference())
        {
            t = t.references_to();
        }

        outline_info.set_field_type(t.get_unqualified_type_but_keep_restrict());

        if (is_new)
        {
            TL::Type in_outline_type = t.get_lvalue_reference_to();
            in_outline_type = add_extra_dimensions(sym, in_outline_type, &outline_info);

            if ((outline_info.get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE)
                    == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE)
            {
                // ALLOCATABLEs must be handled with care
                outline_info.set_in_outline_type(outline_info.get_field_type().get_lvalue_reference_to());
            }
            else
            {
                outline_info.set_in_outline_type(in_outline_type);
            }

            if (IS_CXX_LANGUAGE
                    && !t.is_pod())
            {
                outline_info.set_allocation_policy(
                        outline_info.get_allocation_policy() |
                        OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY);
            }

            _outline_info.move_at_end(outline_info);
        }
    }


    void OutlineInfoRegisterEntities::add_capture_with_value(Symbol sym, Nodecl::NodeclBase expr)
    {
        add_capture_with_value(sym, expr, Nodecl::NodeclBase::null());
    }

    void OutlineInfoRegisterEntities::add_capture_with_value(Symbol sym, Nodecl::NodeclBase expr, Nodecl::NodeclBase condition)
    {
        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym, is_new);

        outline_info.set_sharing(OutlineDataItem::SHARING_CAPTURE);

        Type t = sym.get_type();
        if (t.is_any_reference())
        {
            t = t.references_to();
        }
        outline_info.set_field_type(t);

        if (is_new)
        {
            TL::Type in_outline_type = t.get_lvalue_reference_to();
            in_outline_type = add_extra_dimensions(sym, in_outline_type, &outline_info);

            outline_info.set_in_outline_type(in_outline_type);

            _outline_info.move_at_end(outline_info);

            if (IS_CXX_LANGUAGE
                    && !t.is_pod())
            {
                outline_info.set_allocation_policy(
                        outline_info.get_allocation_policy() |
                        OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY);
            }
        }

        outline_info.set_captured_value(expr);
        outline_info.set_conditional_capture_value(condition);
    }

    void OutlineInfoRegisterEntities::add_reduction(TL::Symbol symbol,
            TL::Type reduction_type,
            OpenMP::Reduction* reduction,
            OutlineDataItem::Sharing kind)
    {
        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(symbol, is_new);
        outline_info.set_sharing(kind);
        outline_info.set_reduction_info(reduction, reduction_type);

        TL::Type t = symbol.get_type();
        if (t.is_any_reference())
        {
            t = t.references_to();
        }

        if (IS_FORTRAN_LANGUAGE)
        {
            outline_info.set_field_type(TL::Type::get_void_type().get_pointer_to());
        }
        else
        {
            outline_info.set_field_type(t.get_pointer_to());
        }

        if (is_new)
        {
            TL::Type in_outline_type = t.get_lvalue_reference_to();
            in_outline_type = add_extra_dimensions(symbol, in_outline_type, &outline_info);

            outline_info.set_in_outline_type(in_outline_type);

            _outline_info.move_at_end(outline_info);
        }

        outline_info.set_private_type(reduction_type);
    }

    void OutlineInfoRegisterEntities::add_copy_of_outline_data_item(const OutlineDataItem& data_item)
    {
        _outline_info.add_copy_of_outline_data_item(data_item);
    }

    class OutlineInfoSetupVisitor : public Nodecl::ExhaustiveVisitor<void>, OutlineInfoRegisterEntities
    {
        private:
            OutlineInfo& _outline_info;
        public:
            OutlineInfoSetupVisitor(OutlineInfo& outline_info, TL::Scope sc)
                : OutlineInfoRegisterEntities(outline_info, sc),
                _outline_info(outline_info)
            {
            }

            void visit(const Nodecl::OpenMP::Auto& shared)
            {
                Nodecl::List l = shared.get_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    error_printf("%s: error: entity '%s' with unresolved 'auto' data sharing\n",
                            it->get_locus_str().c_str(),
                            sym.get_name().c_str());
                }
                if (!l.empty())
                {
                    running_error("%s: error: unresolved auto data sharings\n", shared.get_locus_str().c_str());
                }
            }

            void visit(const Nodecl::OpenMP::Shared& shared)
            {
                Nodecl::List l = shared.get_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();

                    if (IS_FORTRAN_LANGUAGE)
                    {
                        add_shared_opaque(sym);
                    }
                    else
                    {
                        add_shared(sym);
                    }
                }
            }

            void visit(const Nodecl::OpenMP::Alloca& alloca)
            {
                Nodecl::List l = alloca.get_alloca_expressions().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::DataReference data_ref(*it);

                    ERROR_CONDITION(!data_ref.is_valid(), "%s: data reference '%s' must be valid at this point!\n",
                            it->get_locus_str().c_str(),
                            Codegen::get_current().codegen_to_str(*it, it->retrieve_context()).c_str());

                    TL::Symbol sym = data_ref.get_base_symbol();

                    add_alloca(sym, data_ref);
                }
            }


            void visit(const Nodecl::OpenMP::DepIn& dep_in)
            {
                add_dependences(dep_in.get_in_deps().as<Nodecl::List>(), OutlineDataItem::DEP_IN);
            }

            void visit(const Nodecl::OpenMP::DepInValue& dep_in_value)
            {
                add_dependences(dep_in_value.get_in_deps().as<Nodecl::List>(), OutlineDataItem::DEP_IN_VALUE);
            }

            void visit(const Nodecl::OpenMP::DepInAlloca& dep_in_alloca)
            {
                add_dependences(dep_in_alloca.get_in_deps().as<Nodecl::List>(), OutlineDataItem::DEP_IN_ALLOCA);
            }

            void visit(const Nodecl::OpenMP::DepInPrivate& dep_in_private)
            {
                add_dependences(dep_in_private.get_in_deps().as<Nodecl::List>(), OutlineDataItem::DEP_IN_PRIVATE);
            }

            void visit(const Nodecl::OpenMP::DepOut& dep_out)
            {
                add_dependences(dep_out.get_out_deps().as<Nodecl::List>(), OutlineDataItem::DEP_OUT);
            }

            void visit(const Nodecl::OpenMP::DepInout& dep_inout)
            {
                add_dependences(dep_inout.get_inout_deps().as<Nodecl::List>(), OutlineDataItem::DEP_INOUT);
            }

            void visit(const Nodecl::OpenMP::Concurrent& concurrent)
            {
                add_dependences(concurrent.get_inout_deps().as<Nodecl::List>(), OutlineDataItem::DEP_CONCURRENT);
            }

            void visit(const Nodecl::OpenMP::Commutative& commutative)
            {
                add_dependences(commutative.get_inout_deps().as<Nodecl::List>(), OutlineDataItem::DEP_COMMUTATIVE);
            }

            void visit(const Nodecl::OpenMP::CopyIn& copy_in)
            {
                add_copies(copy_in.get_input_copies().as<Nodecl::List>(), OutlineDataItem::COPY_IN);
            }

            void visit(const Nodecl::OpenMP::CopyOut& copy_out)
            {
                add_copies(copy_out.get_output_copies().as<Nodecl::List>(), OutlineDataItem::COPY_OUT);
            }

            void visit(const Nodecl::OpenMP::CopyInout& copy_inout)
            {
                add_copies(copy_inout.get_inout_copies().as<Nodecl::List>(), OutlineDataItem::COPY_INOUT);
            }

            void visit(const Nodecl::OpenMP::Implements& implements)
            {
                _outline_info.add_implementation(
                        implements.get_function_name().as<Nodecl::Symbol>().get_symbol(),
                        implements.get_device().as<Nodecl::Text>().get_text());
            }

            void visit(const Nodecl::OpenMP::NDRange& ndrange)
            {
                _outline_info.append_to_ndrange(ndrange.get_function_name().as<Nodecl::Symbol>().get_symbol(),
                        ndrange.get_ndrange_expressions().as<Nodecl::List>().to_object_list());
            }

            void visit(const Nodecl::OpenMP::ShMem& shmem)
            {
                _outline_info.append_to_shmem(shmem.get_function_name().as<Nodecl::Symbol>().get_symbol(),
                        shmem.get_shmem_expressions().as<Nodecl::List>().to_object_list());
            }

            void visit(const Nodecl::OpenMP::Onto& onto)
            {
                _outline_info.append_to_onto(onto.get_function_name().as<Nodecl::Symbol>().get_symbol(),
                        onto.get_onto_expressions().as<Nodecl::List>().to_object_list());
            }

            void visit(const Nodecl::OpenMP::File& file)
            {
                _outline_info.set_file(file.get_function_name().as<Nodecl::Symbol>().get_symbol(), file.get_filename().get_text());
            }

            void visit(const Nodecl::OpenMP::Name& name)
            {
                _outline_info.set_name(name.get_function_name().as<Nodecl::Symbol>().get_symbol(), name.get_name().get_text());
            }

            void visit(const Nodecl::OpenMP::Firstprivate& shared)
            {
                Nodecl::List l = shared.get_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    add_capture(sym);
                }
            }

            void visit(const Nodecl::OpenMP::Lastprivate& shared)
            {
                Nodecl::List l = shared.get_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    add_shared_with_private_storage(sym, /* captured */ false);
                }
            }

            void visit(const Nodecl::OpenMP::FirstLastprivate& shared)
            {
                Nodecl::List l = shared.get_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    add_shared_with_private_storage(sym, /* captured */ true);
                }
            }

            void visit(const Nodecl::OpenMP::Private& private_)
            {
                Nodecl::List l = private_.get_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    add_private(sym);
                }
            }

            void visit(const Nodecl::OpenMP::Reduction& reduction)
            {
                Nodecl::List reductions = reduction.get_reductions().as<Nodecl::List>();
                for (Nodecl::List::iterator it = reductions.begin();
                        it != reductions.end();
                        it++)
                {
                    Nodecl::OpenMP::ReductionItem red_item = it->as<Nodecl::OpenMP::ReductionItem>();

                    TL::Symbol reduction_sym = red_item.get_reductor().get_symbol();
                    TL::Symbol symbol = red_item.get_reduced_symbol().get_symbol();
                    TL::Type reduction_type = red_item.get_reduction_type().get_type();

                    OpenMP::Reduction* red = OpenMP::Reduction::get_reduction_info_from_symbol(reduction_sym);
                    ERROR_CONDITION(red == NULL, "Invalid value for red_item", 0);
                    add_reduction(symbol, reduction_type, red, OutlineDataItem::SHARING_REDUCTION);
                }
            }

            void visit(const Nodecl::OpenMP::ConcurrentReduction& reduction)
            {
                Nodecl::List reductions = reduction.get_reductions().as<Nodecl::List>();
                for (Nodecl::List::iterator it = reductions.begin();
                        it != reductions.end();
                        it++)
                {
                    Nodecl::OpenMP::ReductionItem red_item = it->as<Nodecl::OpenMP::ReductionItem>();

                    TL::Symbol reduction_sym = red_item.get_reductor().get_symbol();
                    TL::Symbol symbol = red_item.get_reduced_symbol().get_symbol();
                    TL::Type reduction_type = red_item.get_reduction_type().get_type();

                    OpenMP::Reduction* red = OpenMP::Reduction::get_reduction_info_from_symbol(reduction_sym);
                    ERROR_CONDITION(red == NULL, "Invalid value for red_item", 0);
                    add_reduction(symbol, reduction_type, red, OutlineDataItem::SHARING_CONCURRENT_REDUCTION);

                    OutlineDataItem &outline_data_item = _outline_info.get_entity_for_symbol(symbol);

                    TL::DataReference data_ref(red_item.get_reduced_symbol());
                    outline_data_item.get_dependences().append(OutlineDataItem::DependencyItem(data_ref, OutlineDataItem::DEP_CONCURRENT));
                }
            }

            // void visit(const Nodecl::OpenMP::ReductionItem& reduction)
            // {
            //     TL::Symbol reduction_sym = reduction.get_reductor().get_symbol();
            //     TL::Symbol symbol = reduction.get_reduced_symbol().get_symbol();
            //     TL::Type reduction_type = reduction.get_reduction_type().get_type();

            //     OpenMP::Reduction* red = OpenMP::Reduction::get_reduction_info_from_symbol(reduction_sym);
            //     ERROR_CONDITION(red == NULL, "Invalid value for reduction", 0);
            //     add_reduction(symbol, reduction_type, red);
            // }

            void visit(const Nodecl::OpenMP::Target& target)
            {
                Nodecl::List devices = target.get_devices().as<Nodecl::List>();

                for (Nodecl::List::iterator it = devices.begin();
                        it != devices.end();
                        it++)
                {
                    std::string current_device = it->as<Nodecl::Text>().get_text();
                    _outline_info.add_device_name(current_device, _outline_info.get_funct_symbol());
                }
                walk(target.get_items());
            }
    };

    OutlineInfo::OutlineInfo() : _data_env_items() { }

    OutlineInfo::~OutlineInfo()
    {
        for (ObjectList<OutlineDataItem*>::iterator it = _data_env_items.begin();
                it != _data_env_items.end();
                it++)
        {
            delete *it;
        }
    }

    OutlineInfo::OutlineInfo(Nodecl::NodeclBase environment, TL::Symbol funct_symbol, RefPtr<OpenMP::FunctionTaskSet> function_task_set)
        : _data_env_items(), _function_task_set(function_task_set)
    {
        TL::Scope sc(CURRENT_COMPILED_FILE->global_decl_context);
        if (!environment.is_null())
        {
            sc = environment.retrieve_context();
        }

        if (funct_symbol.is_valid())
        {
            //Add one targetInfo, main task
            TargetInformation ti;
            ti.set_outline_name(get_outline_name(funct_symbol));
            _implementation_table.insert(std::make_pair(funct_symbol, ti));

            _funct_symbol = funct_symbol;
        }

        OutlineInfoSetupVisitor setup_visitor(*this, sc);
        setup_visitor.walk(environment);

        reset_array_counters();
    }

    void OutlineInfo::reset_array_counters()
    {
        Counter& counter_allocatable = CounterManager::get_counter("array-allocatable");
        counter_allocatable = 0;
        Counter& counter_upper_boundaries = CounterManager::get_counter("array-upper-boundaries");
        counter_upper_boundaries = 0;
        Counter& counter_lower_boundaries = CounterManager::get_counter("array-lower-boundaries");
        counter_lower_boundaries = 0;
    }

    ObjectList<OutlineDataItem*> OutlineInfo::get_data_items()
    {
        return _data_env_items;
    }

    TL::Symbol OutlineInfo::get_funct_symbol() const
    {
        return _funct_symbol;
    }

    OutlineDataItem& OutlineInfo::prepend_field(TL::Symbol sym)
    {
        std::string field_name = get_field_name(sym.get_name());
        OutlineDataItem* env_item = new OutlineDataItem(sym, field_name);

        _data_env_items.std::vector<OutlineDataItem*>::insert(_data_env_items.begin(), env_item);
        return *(_data_env_items.front());
    }

    OutlineDataItem& OutlineInfo::append_field(TL::Symbol sym)
    {
        std::string field_name = get_field_name(sym.get_name());
        OutlineDataItem* env_item = new OutlineDataItem(sym, field_name);

        _data_env_items.append(env_item);
        return *(_data_env_items.back());
    }

    void OutlineInfo::move_at_end(OutlineDataItem& item)
    {
        TL::ObjectList<OutlineDataItem*> new_list;
        for (TL::ObjectList<OutlineDataItem*>::iterator it = _data_env_items.begin();
                it != _data_env_items.end();
                it++)
        {
            if (*it != &item)
            {
                new_list.append(*it);
            }
        }
        new_list.append(&item);

        std::swap(_data_env_items, new_list);
    }

    void OutlineInfo::add_device_name(std::string device_name,TL::Symbol function_symbol)
    {
       ERROR_CONDITION(_implementation_table.count(function_symbol) == 0,
               "Function symbol '%s' not found in outline info implementation table",
               function_symbol.get_name().c_str());

       _implementation_table[function_symbol].add_device_name(device_name);
    }

    ObjectList<std::string> OutlineInfo::get_device_names(TL::Symbol function_symbol)
    {
       ERROR_CONDITION(_implementation_table.count(function_symbol) == 0,
               "Function symbol '%s' not found in outline info implementation table",
               function_symbol.get_name().c_str());

       return _implementation_table[function_symbol].get_device_names();
    }

    void OutlineInfo::set_file(TL::Symbol function_symbol,std::string file)
    {
        ERROR_CONDITION(_implementation_table.count(function_symbol) == 0,
                "Function symbol '%s' not found in outline info implementation table",
                function_symbol.get_name().c_str());

        _implementation_table[function_symbol].set_file(file);
    }

    std::string OutlineInfo::get_file(TL::Symbol function_symbol)
    {
        ERROR_CONDITION(_implementation_table.count(function_symbol) == 0,
                "Function symbol '%s' not found in outline info implementation table",
                function_symbol.get_name().c_str());

        return _implementation_table[function_symbol].get_file();
    }

    void OutlineInfo::set_name(TL::Symbol function_symbol,std::string name)
    {
        ERROR_CONDITION(_implementation_table.count(function_symbol) == 0,
                "Function symbol '%s' not found in outline info implementation table",
                function_symbol.get_name().c_str());

        _implementation_table[function_symbol].set_name(name);
    }

    std::string OutlineInfo::get_name(TL::Symbol function_symbol)
    {
        ERROR_CONDITION(_implementation_table.count(function_symbol) == 0,
                "Function symbol '%s' not found in outline info implementation table",
                function_symbol.get_name().c_str());

        return _implementation_table[function_symbol].get_name();
    }

    void OutlineInfo::append_to_ndrange(TL::Symbol function_symbol, const ObjectList<Nodecl::NodeclBase>& ndrange_exprs)
    {
       ERROR_CONDITION(_implementation_table.count(function_symbol) == 0,
               "Function symbol '%s' not found in outline info implementation table",
               function_symbol.get_name().c_str());

       _implementation_table[function_symbol].append_to_ndrange(ndrange_exprs);
    }

    void OutlineInfo::append_to_shmem(TL::Symbol function_symbol, const ObjectList<Nodecl::NodeclBase>& shmem_exprs)
    {
       ERROR_CONDITION(_implementation_table.count(function_symbol) == 0,
               "Function symbol '%s' not found in outline info implementation table",
               function_symbol.get_name().c_str());

       _implementation_table[function_symbol].append_to_shmem(shmem_exprs);
    }

    void OutlineInfo::append_to_onto(TL::Symbol function_symbol, const ObjectList<Nodecl::NodeclBase>& onto_exprs)
    {
        ERROR_CONDITION(_implementation_table.count(function_symbol) == 0,
                "Function symbol '%s' not found in outline info implementation table",
                function_symbol.get_name().c_str());

        _implementation_table[function_symbol].append_to_onto(onto_exprs);
    }

    Nodecl::Utils::SimpleSymbolMap OutlineInfo::get_param_arg_map(TL::Symbol function_symbol)
    {
        ERROR_CONDITION(_implementation_table.count(function_symbol) == 0,
                "Function symbol '%s' not found in outline info implementation table",
                function_symbol.get_name().c_str());

        return _implementation_table[function_symbol].get_param_arg_map();
    }

    void OutlineInfo::set_param_arg_map(Nodecl::Utils::SimpleSymbolMap param_arg_map,TL::Symbol function_symbol)
    {
        ERROR_CONDITION(_implementation_table.count(function_symbol) == 0,
                "Function symbol '%s' not found in outline info implementation table",
                function_symbol.get_name().c_str());

        _implementation_table[function_symbol].set_param_arg_map(param_arg_map);
    }

    void OutlineInfo::add_implementation(TL::Symbol function_symbol, std::string device_name)
    {
        //if no impl present, we add it, otherwise just add a device
        if(_implementation_table.count(function_symbol) == 0)
        {
            TargetInformation ti;
            ti.add_device_name(device_name);
            ti.set_outline_name(get_outline_name(function_symbol));
            _implementation_table.insert(std::make_pair(function_symbol, ti));

            if (_function_task_set.valid())
            {
                set_file(function_symbol, _function_task_set->get_function_task(function_symbol).get_target_info().get_file());
                append_to_ndrange(function_symbol, _function_task_set->get_function_task(function_symbol).get_target_info().get_ndrange());
                append_to_shmem(function_symbol, _function_task_set->get_function_task(function_symbol).get_target_info().get_shmem());
                append_to_onto(function_symbol, _function_task_set->get_function_task(function_symbol).get_target_info().get_onto());
            }
        }
        else
        {
            add_device_name(device_name,function_symbol);
        }
    }

    std::string OutlineInfo::get_outline_name(TL::Symbol function_symbol)
    {
        // This string will be the returned outline name
        std::string outline_name;
        std::string orig_function_name = function_symbol.get_name();

        std::string prefix = "", suffix = "";

        Counter& task_counter = CounterManager::get_counter("nanos++-outline");
        if (IS_FORTRAN_LANGUAGE)
        {
            std::stringstream ss;
            ss << "ol_";

            if (function_symbol.is_nested_function())
            {
                TL::Symbol enclosing_function_symbol = function_symbol.get_scope().get_related_symbol();
                ss << enclosing_function_symbol.get_name() << "_";

                if (enclosing_function_symbol.is_in_module())
                {
                    ss << enclosing_function_symbol.in_module().get_name() << "_";
                }
            }
            else if (function_symbol.is_in_module())
            {
                ss << function_symbol.in_module().get_name() << "_";
            }

            ss << (int)task_counter;

            unsigned int hash_int = simple_hash_str(ss.str().c_str());

            // Use this base to maximally compact the hash
            static char base_syms[] = "0123456789abcdefghijklmnopqrstuvwxyz_";
            const unsigned int nbase =
                sizeof(base_syms) / sizeof(base_syms[0]) - 1; // Do not count \0

            if (hash_int == 0)
            {
                prefix = "0";
            }
            else
            {
                while (hash_int > 0)
                {
                    prefix += base_syms[(hash_int % nbase)];
                    hash_int /= nbase;
                }
            }

            // suffix is left empty here as we included the task_counter in our hash
            outline_name = prefix + function_symbol.get_name();
        }
        else if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            // C and C++ do not define limits on the identifiers
            prefix = "ol_";

            std::stringstream ss;
            ss << "_" << (int)task_counter;
            suffix = ss.str();

            if (IS_C_LANGUAGE)
            {
                outline_name = prefix + function_symbol.get_name() + suffix;
            }
            else
            {
                // If the symbol name contains any space, we should remove them!

                // First case: the function symbol is a constructor
                // We need to unmangle the symbol name
                std::string unmangle_name = unmangle_symbol_name(function_symbol.get_internal_symbol());

                // Second case: the function symbol is an operator
                // We cannot add the characters of the operator to the function name
                if (unmangle_name.find("operator ") != std::string::npos)
                    unmangle_name = "operator";

                outline_name = prefix + unmangle_name + suffix;
            }
        }

        task_counter++;

        return outline_name;
    }

    OutlineInfo::implementation_table_t& OutlineInfo::get_implementation_table()
    {
        return _implementation_table;
    }

    void OutlineInfo::add_copy_of_outline_data_item(const OutlineDataItem& ol)
    {
        _data_env_items.append(new OutlineDataItem(ol));
    }

    namespace
    {
        bool is_not_private(OutlineDataItem* it)
        {
            return it->get_sharing() != OutlineDataItem::SHARING_PRIVATE;
        }
    }

    ObjectList<OutlineDataItem*> OutlineInfo::get_fields() const
    {
        return _data_env_items.filter(predicate(is_not_private));
    }

    bool OutlineInfo::only_has_smp_or_mpi_implementations() const
    {
        for (implementation_table_t::const_iterator it = _implementation_table.begin();
                it != _implementation_table.end();
                ++it)
        {
            TargetInformation target_info = it->second;
            ObjectList<std::string> devices = target_info.get_device_names();
            for (ObjectList<std::string>::const_iterator it2 = devices.begin();
                    it2 != devices.end();
                    ++it2)
            {
                if (*it2 != "smp" && *it2 != "mpi")
                    return false;
            }
        }
        return true;
    }
} }
