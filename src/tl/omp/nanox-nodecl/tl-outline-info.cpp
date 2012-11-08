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


#include "tl-outline-info.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-datareference.hpp"
#include "tl-counters.hpp"
#include "codegen-phase.hpp"
#include "cxx-diagnostic.h"
#include "cxx-exprtype.h"
#include "fortran03-typeutils.h"

namespace TL { namespace Nanox {

    std::string OutlineInfo::get_field_name(std::string name)
    {
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

        if (IS_CXX_LANGUAGE
                && name == "this")
            name = "this_";

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
            outline_info.set_field_type(t.get_unqualified_type());
            outline_info.set_in_outline_type(t.get_unqualified_type());
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

    void OutlineInfoRegisterEntities::add_capture_address(Symbol sym, TL::DataReference& data_ref)
    {
        ERROR_CONDITION(!IS_C_LANGUAGE && !IS_CXX_LANGUAGE, "This function is only for C/C++", 0);

        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym, is_new);

        outline_info.set_sharing(OutlineDataItem::SHARING_CAPTURE_ADDRESS);

        outline_info.set_base_address_expression(data_ref.get_base_address());

        Type t = sym.get_type();
        if (t.is_any_reference())
        {
            t = t.references_to();
        }

        if (t.is_array())
        {
            t = t.array_element().get_pointer_to();
        }

        outline_info.set_field_type(t.get_unqualified_type());

        if (is_new)
        {
            TL::Type in_outline_type = t.get_unqualified_type();
            in_outline_type = add_extra_dimensions(sym, in_outline_type, &outline_info);

            outline_info.set_in_outline_type(in_outline_type);

            _outline_info.move_at_end(outline_info);
        }
    }

    void OutlineInfoRegisterEntities::add_private(Symbol sym)
    {
        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym, is_new);

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
        outline_info.set_sharing(OutlineDataItem::SHARING_PRIVATE);

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

        Nodecl::Symbol sym_ref = Nodecl::Symbol::make(sym, "", 0);
        sym_ref.set_type(t.get_lvalue_reference_to());

        this->add_capture_with_value(
                new_addr_symbol,
                Nodecl::Reference::make(sym_ref, t.get_pointer_to(), "", 0));
    }

    TL::Type OutlineInfoRegisterEntities::add_extra_dimensions(TL::Symbol sym, TL::Type t)
    {
        return add_extra_dimensions(sym, t, NULL);
    }

    TL::Type OutlineInfoRegisterEntities::add_extra_dimensions(TL::Symbol sym, TL::Type t,
            OutlineDataItem* outline_data_item)
    {
        if (t.is_array())
        {
            if (IS_C_LANGUAGE
                    || IS_CXX_LANGUAGE)
            {
                Nodecl::NodeclBase array_size = t.array_get_size();

                if (array_size.is<Nodecl::Symbol>()
                        && array_size.get_symbol().is_saved_expression())
                {
                    this->add_capture(array_size.get_symbol());

                    if (outline_data_item != NULL)
                    {
                        if (outline_data_item->get_sharing() == OutlineDataItem::SHARING_CAPTURE)
                        {
                            outline_data_item->set_allocation_policy(OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED);
                        }
                        else
                        {
                            outline_data_item->set_field_type(TL::Type::get_void_type().get_pointer_to());
                        }
                    }
                }

                t = add_extra_dimensions(sym, t.array_element(), outline_data_item);

                return t.get_array_to(array_size, _sc);
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                Nodecl::NodeclBase lower, upper;

                t.array_get_bounds(lower, upper);

                Nodecl::NodeclBase result_lower, result_upper;

                bool make_allocatable = false;

                // If the symbol is a shared allocatable we want the original type
                if (sym.is_allocatable()
                        && outline_data_item != NULL
                        && (outline_data_item->get_sharing() == OutlineDataItem::SHARING_SHARED))
                    return t;

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

                        Nodecl::NodeclBase symbol_ref = Nodecl::Symbol::make(sym, "", 0);
                        TL::Type sym_type = sym.get_type();
                        if (!sym_type.is_any_reference()) sym_type = sym_type.get_lvalue_reference_to();
                        symbol_ref.set_type(sym_type);

                        Source lbound_src;
                        lbound_src << "LBOUND(" << as_expression(symbol_ref) << ", DIM=" << dim << ")";

                        Nodecl::NodeclBase lbound_tree = lbound_src.parse_expression(_sc);

                        this->add_capture_with_value(bound_sym, lbound_tree);

                        result_lower = Nodecl::Symbol::make(bound_sym, "", 0);
                        result_lower.set_type(bound_sym.get_type().get_lvalue_reference_to());

                        make_allocatable = true;
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

                        Nodecl::NodeclBase symbol_ref = Nodecl::Symbol::make(sym, "", 0);
                        TL::Type sym_type = sym.get_type();
                        if (!sym_type.is_any_reference()) sym_type = sym_type.get_lvalue_reference_to();
                        symbol_ref.set_type(sym_type);

                        Source ubound_src;
                        ubound_src << "UBOUND(" << as_expression(symbol_ref) << ", DIM=" << dim << ")";

                        Nodecl::NodeclBase ubound_tree = ubound_src.parse_expression(_sc);

                        this->add_capture_with_value(bound_sym, ubound_tree);

                        result_upper = Nodecl::Symbol::make(bound_sym, "", 0);
                        result_upper.set_type(bound_sym.get_type().get_lvalue_reference_to());

                        make_allocatable = true;
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

                if (make_allocatable
                        && outline_data_item != NULL)
                {
                    if (outline_data_item->get_sharing() == OutlineDataItem::SHARING_CAPTURE)
                    {
                        outline_data_item->set_allocation_policy(OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE);
                        outline_data_item->set_field_type(
                                ::fortran_get_n_ranked_type_with_descriptor(
                                    ::fortran_get_rank0_type(t.get_internal_type()),
                                    ::fortran_get_rank_of_type(t.get_internal_type()), _sc.get_decl_context()));
                    }
                }

                TL::Type res = add_extra_dimensions(sym, t.array_element(), outline_data_item);
                return res.get_array_to(result_lower, result_upper, _sc);
            }
        }
        else if (t.is_pointer())
        {
            TL::Type res = add_extra_dimensions(sym, t.points_to(), outline_data_item);
            return res.get_pointer_to();
        }
        else if (t.is_lvalue_reference())
        {
            TL::Type res = add_extra_dimensions(sym, t.references_to(), outline_data_item);
            return res.get_lvalue_reference_to();
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
        return t;
    }

    void OutlineInfoRegisterEntities::add_dependence(Nodecl::NodeclBase node, OutlineDataItem::Directionality directionality)
    {
        TL::DataReference data_ref(node);
        if (data_ref.is_valid())
        {
            TL::Symbol sym = data_ref.get_base_symbol();
            if (IS_FORTRAN_LANGUAGE)
            {
                add_shared_opaque(sym);
            }
            else if (data_ref.is<Nodecl::Symbol>())
            {
                add_shared(sym);
            }
            else
            {
                add_capture_address(sym, data_ref);
            }

            OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym);
            outline_info.set_directionality(
                    OutlineDataItem::Directionality(directionality | outline_info.get_directionality())
                    );
            outline_info.get_dependences().append(data_ref);
        }
        else
        {
            internal_error("%s: data reference '%s' must be valid at this point!\n", 
                    node.get_locus().c_str(),
                    Codegen::get_current().codegen_to_str(node, node.retrieve_context()).c_str()
                    );
        }
    }

    void OutlineInfoRegisterEntities::add_dependences(Nodecl::List list, OutlineDataItem::Directionality directionality)
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
                // -- FIXME ---
                // If we are in an inline task, dependences are
                // truly shared...
                add_shared(sym);

                OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym);
                outline_info.set_copy_directionality(
                        OutlineDataItem::CopyDirectionality(copy_directionality | outline_info.get_copy_directionality())
                        );

                outline_info.get_copies().append(data_ref);
            }
            else
            {
                internal_error("%s: data reference '%s' must be valid at this point!\n", 
                        it->get_locus().c_str(),
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

        outline_info.set_field_type(t);

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
                outline_info.set_in_outline_type(t.get_lvalue_reference_to());
            }

            _outline_info.move_at_end(outline_info);
        }
    }

    void OutlineInfoRegisterEntities::add_capture_with_value(Symbol sym, Nodecl::NodeclBase expr)
    {
        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym, is_new);

        outline_info.set_sharing(OutlineDataItem::SHARING_CAPTURE);

        Type t = sym.get_type();
        if (t.is_any_reference())
        {
            t = t.references_to();
        }

        if (is_new)
        {
            TL::Type in_outline_type = t.get_lvalue_reference_to();
            in_outline_type = add_extra_dimensions(sym, in_outline_type, &outline_info);

            outline_info.set_in_outline_type(in_outline_type);

            _outline_info.move_at_end(outline_info);
        }

        outline_info.set_field_type(t);
        outline_info.set_captured_value(expr);
    }

    void OutlineInfoRegisterEntities::add_reduction(TL::Symbol symbol, OpenMP::UDRInfoItem& udr_info)
    {
        bool is_new = false;
        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(symbol, is_new);
        outline_info.set_sharing(OutlineDataItem::SHARING_REDUCTION);
        outline_info.set_reduction_info(&udr_info);

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

        outline_info.set_private_type(t);
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
                Nodecl::List l = shared.get_auto_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    error_printf("%s: error: entity '%s' with unresolved 'auto' data sharing\n",
                            it->get_locus().c_str(),
                            sym.get_name().c_str());
                }
                if (!l.empty())
                {
                    running_error("%s: error: unresolved auto data sharings\n", shared.get_locus().c_str());
                }
            }

            void visit(const Nodecl::OpenMP::Shared& shared)
            {
                Nodecl::List l = shared.get_shared_symbols().as<Nodecl::List>();
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


            void visit(const Nodecl::OpenMP::DepIn& dep_in)
            {
                add_dependences(dep_in.get_in_deps().as<Nodecl::List>(), OutlineDataItem::DIRECTIONALITY_IN);
            }

            void visit(const Nodecl::OpenMP::DepOut& dep_out)
            {
                add_dependences(dep_out.get_out_deps().as<Nodecl::List>(), OutlineDataItem::DIRECTIONALITY_OUT);
            }

            void visit(const Nodecl::OpenMP::DepInout& dep_inout)
            {
                add_dependences(dep_inout.get_inout_deps().as<Nodecl::List>(), OutlineDataItem::DIRECTIONALITY_INOUT);
            }

            void visit(const Nodecl::OpenMP::Concurrent& concurrent)
            {
                add_dependences(concurrent.get_inout_deps().as<Nodecl::List>(), OutlineDataItem::DIRECTIONALITY_CONCURRENT);
            }

            void visit(const Nodecl::OpenMP::Commutative& commutative)
            {
                add_dependences(commutative.get_inout_deps().as<Nodecl::List>(), OutlineDataItem::DIRECTIONALITY_COMMUTATIVE);
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
                        implements.get_device().as<Nodecl::Text>().get_text(),
                        implements.get_function_name().as<Nodecl::Symbol>().get_symbol());
            }

            void visit(const Nodecl::OpenMP::Firstprivate& shared)
            {
                Nodecl::List l = shared.get_firstprivate_symbols().as<Nodecl::List>();
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
                Nodecl::List l = shared.get_lastprivate_symbols().as<Nodecl::List>();
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
                Nodecl::List l = shared.get_firstlastprivate_symbols().as<Nodecl::List>();
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
                Nodecl::List l = private_.get_private_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    add_private(sym);

                    if (sym.is_allocatable())
                    {
                        error_printf("%s: error: setting a PRIVATE data-sharing to ALLOCATABLE arrays are not supported\n",
                                private_.get_locus().c_str());
                    }
                }
            }

            void visit(const Nodecl::OpenMP::ReductionItem& reduction)
            {
                TL::Symbol udr_reductor = reduction.get_reductor().get_symbol();
                TL::Symbol symbol = reduction.get_reduced_symbol().get_symbol();

                OpenMP::UDRInfoItem &udr_info = OpenMP::UDRInfoItem::get_udr_info_item_from_symbol_holder(udr_reductor);

                add_reduction(symbol, udr_info);
            }

            void visit(const Nodecl::OpenMP::Target& target)
            {
                Nodecl::List devices = target.get_devices().as<Nodecl::List>();

                for (Nodecl::List::iterator it = devices.begin();
                        it != devices.end();
                        it++)
                {
                    std::string current_device = it->as<Nodecl::Text>().get_text();
                    _outline_info.add_device_name(current_device);
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

    OutlineInfo::OutlineInfo(Nodecl::NodeclBase environment, bool is_function_task)
        : _data_env_items()
    {
        TL::Scope sc(CURRENT_COMPILED_FILE->global_decl_context);
        if (!environment.is_null())
        {
            sc = environment.retrieve_context();
        }
        OutlineInfoSetupVisitor setup_visitor(*this, sc);
        setup_visitor.walk(environment);
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

    void OutlineInfo::add_device_name(std::string device_name)
    {
        _device_names.append(device_name);
    }

    ObjectList<std::string> OutlineInfo::get_device_names()
    {
        return _device_names;
    }

    void OutlineInfo::add_implementation(std::string device_name, TL::Symbol function_symbol)
    {
        _implementation_table.insert(make_pair(device_name, function_symbol));
    }

    OutlineInfo::implementation_table_t OutlineInfo::get_implementation_table()
    {
        return _implementation_table;
    }
} }
