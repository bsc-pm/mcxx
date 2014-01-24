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

#include "tl-datareference.hpp"
#include "tl-nodecl-visitor.hpp"

#include "cxx-cexpr.h"
#include "fortran03-typeutils.h"

namespace TL
{
    struct DataReferenceVisitor : public Nodecl::NodeclVisitor<void>
    {
        public:
            DataReferenceVisitor(DataReference& data_ref)
                : _data_ref(data_ref)
            {
            }

        private:
            DataReference& _data_ref;

            virtual void unhandled_node(const Nodecl::NodeclBase & tree) 
            { 
                _data_ref._is_valid = false;
                _data_ref._error_log = 
                    tree.get_locus_str() + ": error: expression '" + tree.prettyprint() + "' not allowed in data-reference\n";
            }

            // Symbol
            virtual void visit(const Nodecl::Symbol& n)
            {
                _data_ref._base_symbol = n.get_symbol();

                TL::Type t;

                if (n.get_type().is_valid())
                {
                    t = n.get_type();
                }
                else
                {
                    t = n.get_symbol().get_type();
                }

                if (t.is_any_reference())
                    t = t.references_to();

                _data_ref._data_type = t;
                _data_ref._base_address = Nodecl::Reference::make(
                        n.shallow_copy(),
                        t.get_pointer_to(),
                        n.get_locus());

                if (IS_FORTRAN_LANGUAGE
                        && _data_ref == n)
                {
                    TL::Symbol sym = n.get_symbol();
                    if (sym.is_parameter()
                            && sym.get_type().no_ref().is_array()
                            && !sym.get_type().no_ref().array_requires_descriptor()
                            && sym.get_type().no_ref().array_get_size().is_null())
                    {
                        // This is a 'A' where A is an assumed size array.
                        // We cannot accept this case
                        _data_ref._is_valid = false;
                    }
                }
            }

            virtual void visit(const Nodecl::Dereference& derref)
            {
                Nodecl::NodeclBase operand = derref.get_rhs();
                while (operand.is<Nodecl::ParenthesizedExpression>())
                    operand = operand.as<Nodecl::ParenthesizedExpression>().get_nest();

                if (operand.is<Nodecl::Reference>())
                {
                    // *&a is like a
                    walk(operand.as<Nodecl::Reference>().get_rhs());
                    return;
                }

                walk(derref.get_rhs());

                if (!_data_ref.is_valid())
                    return;

                TL::Type t = derref.get_type();
                if (t.is_any_reference())
                    t = t.references_to();

                _data_ref._data_type = t;

                if (IS_FORTRAN_LANGUAGE
                        && derref.get_rhs().get_type().no_ref().is_pointer()
                        && derref.get_rhs().get_type().no_ref().points_to().is_fortran_array())
                {
                    _data_ref._base_address = Nodecl::Reference::make(
                            derref.shallow_copy(), t.get_pointer_to(), derref.get_locus());
                }
                else
                {
                    _data_ref._base_address = derref.get_rhs().shallow_copy();
                }

            }

            virtual void visit(const Nodecl::Reference& ref)
            {
                // In general we do not allow &x but there are some cases that may arise
                // during internal transformations
                //
                Nodecl::NodeclBase rhs = ref.get_rhs();
                if (rhs.is<Nodecl::Dereference>())
                {
                    // &*a is like a
                    walk(ref.get_rhs().as<Nodecl::Dereference>().get_rhs());
                }
                else if (rhs.is<Nodecl::ArraySubscript>())
                {
                    // &(a[e])
                    // &(a[l:u])
                    // &(a[l;S])
                    walk(rhs.as<Nodecl::ArraySubscript>().get_subscripted());
                }
                else
                {
                    unhandled_node(ref);
                }
            }

            TL::Type extend_array_type_to_regions(const Nodecl::ArraySubscript& array)
            {
                TL::Type subscripted_type = array.get_subscripted().get_type();

                TL::ObjectList<Nodecl::NodeclBase> lower_bounds;
                TL::ObjectList<Nodecl::NodeclBase> upper_bounds;

                if (subscripted_type.is_any_reference())
                    subscripted_type = subscripted_type.references_to();

                ERROR_CONDITION(!subscripted_type.is_pointer() && !subscripted_type.is_array(), "Invalid type!", 0);

                Nodecl::List subscripts = array.get_subscripts().as<Nodecl::List>();

                TL::Type t = subscripted_type;

                for (Nodecl::List::iterator it = subscripts.begin();
                        it != subscripts.end();
                        it++)
                {
                    Nodecl::NodeclBase index = *it;

                    if (t.is_pointer())
                    {
                        // We do not really know the size, so normalize the region from 0
                        lower_bounds.push_back(const_value_to_nodecl(const_value_get_zero(4, 1)));
                        if (index.is<Nodecl::Range>())
                        {
                            // compute the new upper bound normalized to 0
                            upper_bounds.push_back(
                                    Nodecl::Minus::make(
                                        Nodecl::ParenthesizedExpression::make(
                                            index.as<Nodecl::Range>().get_upper().shallow_copy(),
                                            get_signed_int_type()),
                                        Nodecl::ParenthesizedExpression::make(
                                            index.as<Nodecl::Range>().get_lower().shallow_copy(),
                                            get_signed_int_type()),
                                        get_signed_int_type()));
                        }
                        else
                        {
                            // A single element of this region
                            upper_bounds.push_back(const_value_to_nodecl(const_value_get_zero(4, 1)));
                        }

                        t = t.points_to();
                    }
                    else if (t.is_array())
                    {
                        Nodecl::NodeclBase lb, ub;

                        t.array_get_bounds(lb, ub);

                        lower_bounds.push_back(lb.shallow_copy());
                        upper_bounds.push_back(ub.shallow_copy());

                        t = t.array_element();
                    }
                    else
                    {
                        internal_error("Mismatch between types and indexes", 0);
                    }
                }

                TL::Type rebuilt_type = t;

                ERROR_CONDITION(lower_bounds.size() != subscripts.size()
                        || subscripts.size() != upper_bounds.size(),
                        "Mismatch between dimensions and subscripts", 0);

                for (int i = lower_bounds.size() - 1; i >= 0; i--)
                {
                    Nodecl::NodeclBase item = subscripts[i];

                    if (item.is<Nodecl::Range>())
                    {
                        rebuilt_type =
                            get_array_type_bounds_with_regions(rebuilt_type.get_internal_type(),
                                    lower_bounds[i].get_internal_nodecl(),
                                    upper_bounds[i].get_internal_nodecl(),
                                    CURRENT_COMPILED_FILE->global_decl_context,
                                    item.shallow_copy().get_internal_nodecl(),
                                    CURRENT_COMPILED_FILE->global_decl_context);
                    }
                    else
                    {
                        Nodecl::NodeclBase singleton_region =
                            Nodecl::Range::make(
                                    item.shallow_copy(),
                                    item.shallow_copy(),
                                    /* stride */ const_value_to_nodecl(const_value_get_signed_int(1)),
                                    item.get_type(),
                                    item.get_locus());

                        rebuilt_type =
                            get_array_type_bounds_with_regions(rebuilt_type.get_internal_type(),
                                    lower_bounds[i].get_internal_nodecl(),
                                    upper_bounds[i].get_internal_nodecl(),
                                    CURRENT_COMPILED_FILE->global_decl_context,
                                    singleton_region.get_internal_nodecl(),
                                    CURRENT_COMPILED_FILE->global_decl_context);
                    }
                }

                return rebuilt_type;
            }

            virtual void visit(const Nodecl::Conversion& c)
            {
               walk(c.get_nest());
            }

            virtual void visit(const Nodecl::ParenthesizedExpression& p)
            {
                walk(p.get_nest());
            }

            virtual void visit(const Nodecl::ArraySubscript& array)
            {
                walk(array.get_subscripted());

                if (!_data_ref._is_valid)
                    return;

                // Accesses like a[1][2] look like scalars but we want them to behave
                // as if they were a[1:1][2:2]
                TL::Type t = array.get_type();
                if (t.is_any_reference())
                    t = t.references_to();

                Nodecl::List subscripts = array.get_subscripts().as<Nodecl::List>();
                Nodecl::List low_subscripts_list;

                _data_ref._data_type = extend_array_type_to_regions(array);

                Nodecl::NodeclBase subscripted = array.get_subscripted();
                while (subscripted.is<Nodecl::Conversion>())
                {
                    subscripted = subscripted.as<Nodecl::Conversion>().get_nest();
                }

                if (subscripted.is<Nodecl::Symbol>())
                {
                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    {
                        // float x[10][20]       x[1][2]      (we are in [1][2])
                        // float *p              p[3]         (we are in [3])
                        //
                        // Use x (or p) directly
                        _data_ref._base_address = subscripted;
                    }
                }
                else if (subscripted.is<Nodecl::ClassMemberAccess>())
                {
                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    {
                        // struct A { float x[10][20]; float *p; } a;
                        //                      a.x[1][2]     (we are in [1][2])
                        //                      a.p[3]        (we are in [3])
                        // Use a.x or (a.p) directly
                        _data_ref._base_address = subscripted;
                    }
                }
                else if (subscripted.is<Nodecl::Shaping>())
                {
                    // ([10][20]x)[30][40]          (we are in [30][40])
                    // Use the address of the shaping expression itself
                    // Do nothing
                }
                else if (subscripted.is<Nodecl::ArraySubscript>())
                {
                    // float (*p)[20][30]    (p[1])[2][3]  (we are in [2][3])
                    // Do nothing
                }
                else if (subscripted.is<Nodecl::Dereference>())
                {
                    // float (*p)[20][30]    (*p)[2][3]    (we are in [2][3])
                    // This is like p[0][2][3] so the base address is p.
                    // Do nothing
                }
                else
                {
                    // Anything else cannot be accepted here
                    _data_ref._is_valid = false;
                    return;
                }
            }

            virtual void visit(const Nodecl::ClassMemberAccess& member)
            {
                walk(member.get_lhs());

                if (!_data_ref._is_valid)
                    return;

                TL::Type t = member.get_type();
                if (t.is_any_reference())
                    t = t.references_to();

                _data_ref._data_type = t;

                if (member.get_member().get_kind() == NODECL_CLASS_MEMBER_ACCESS)
                {
                    _data_ref._base_address =
                        Nodecl::Reference::make(
                                Nodecl::ClassMemberAccess::make(
                                    _data_ref._base_address.as<Nodecl::Reference>().get_rhs(),
                                    member.get_member().shallow_copy(),
                                    /* member-form */ Nodecl::NodeclBase::null(),
                                    t,
                                    member.get_locus()
                                    ),
                                t.get_pointer_to(),
                                member.get_locus());
                }
                else if (IS_CXX_LANGUAGE
                        && member.get_member().get_kind() == NODECL_SYMBOL
                        && _data_ref._base_address.get_kind() == NODECL_SYMBOL
                        && _data_ref._base_address.get_symbol().get_name() == "this")
                {
                    _data_ref._base_address =
                        Nodecl::Reference::make(
                                Nodecl::ClassMemberAccess::make(
                                    member.get_lhs().shallow_copy(),
                                    member.get_member().shallow_copy(),
                                    member.get_member_form().shallow_copy(),
                                    t,
                                    member.get_locus()
                                    ),
                                t.get_pointer_to(),
                                member.get_locus());
                }
                else
                {
                    // a.x
                    // If the base address of the 'a' expression contains a Nodecl::Reference, remove it
                    Nodecl::NodeclBase base_address = _data_ref._base_address;
                    if (base_address.is<Nodecl::Reference>())
                        base_address = base_address.as<Nodecl::Reference>().get_rhs();

                    _data_ref._base_address =
                        Nodecl::Reference::make(
                                Nodecl::ClassMemberAccess::make(
                                    base_address,
                                    member.get_member().shallow_copy(),
                                    member.get_member_form().shallow_copy(),
                                    t,
                                    member.get_locus()
                                    ),
                                t.get_pointer_to(),
                                member.get_locus());
                }
            }

            virtual void visit(const Nodecl::Shaping& shaping_expr)
            {
                walk(shaping_expr.get_postfix());

                if (!_data_ref._is_valid)
                    return;

                TL::Type t = shaping_expr.get_type();
                if (t.is_any_reference())
                    t = t.references_to();

                _data_ref._data_type = t;

                _data_ref._base_address = shaping_expr.get_postfix().shallow_copy();
            }
    };

    DataReference::DataReference(Nodecl::NodeclBase expr)
        : Nodecl::NodeclBase(expr),
        _is_valid(true),
        _base_symbol(NULL),
        _data_type(NULL),
        _error_log("")
    {
        if (expr.is_null()
                || expr.is<Nodecl::ErrExpr>()
                || !expr.get_type().is_valid()
                || expr.get_type().is_error_type())
        {
            _is_valid = false;
            return;
        }

        DataReferenceVisitor data_ref_visitor(*this);
        data_ref_visitor.walk(expr);
    }

    //! States whether this expression is a data reference
    /*!
      Not all expressions are data references, as defined by this class,
      use this function to check it
      */
    bool DataReference::is_valid() const
    {
        return _is_valid;
    }

    //! Returns the warning log
    /*!
      This is the same message as is_valid(std::string&) stores in its first parameter
      */
    std::string DataReference::get_error_log() const
    {
        return _error_log;
    }

    //! Gets the base symbol
    /*!
      The base symbol is the entity to which we know we are expressing
      its object or a subobject

      Note for instance that a.b and a.c have the same base symbol, while
      the subobject being named is different.
      */
    Symbol DataReference::get_base_symbol() const
    {
        return _base_symbol;
    }

    //! Returns a type representing the data covered by the data reference
    /*!
      This function returns a type which represents the data covered
      by the data reference.

      \note The type returned may not be fully valid if it contains arrays
      as this function uses Type::get_array_to(const std::string&)
      */
    Type DataReference::get_data_type() const
    {
        return _data_type;
    }

    Nodecl::NodeclBase DataReference::get_base_address() const
    {
        return _base_address.shallow_copy();
    }

    namespace
    {
        Nodecl::NodeclBase get_index_expression_rec(
                TL::ObjectList<Nodecl::NodeclBase>::iterator current_index,

                TL::ObjectList<Nodecl::NodeclBase>::iterator end_index,
                TL::ObjectList<Nodecl::NodeclBase>::iterator current_size,
                TL::ObjectList<Nodecl::NodeclBase>::iterator current_lower
                )
        {
            if ((current_index + 1) == end_index)
            {
                return Nodecl::Minus::make(
                            current_index->shallow_copy(),
                            current_lower->shallow_copy(),
                            current_index->get_type(),
                            current_index->get_locus());
            }
            else
            {
                Nodecl::NodeclBase next_indexing = get_index_expression_rec(
                        current_index + 1, end_index,
                        current_size + 1, current_lower + 1);

                // Horner algorithm
                Nodecl::NodeclBase result;
                result = Nodecl::Add::make(
                        Nodecl::Minus::make(
                            current_index->shallow_copy(),
                            current_lower->shallow_copy(),
                            current_index->get_type(),
                            current_index->get_locus()),
                        Nodecl::Mul::make(
                            current_size->shallow_copy(),
                            Nodecl::ParenthesizedExpression::make(
                                next_indexing,
                                next_indexing.get_type(),
                                next_indexing.get_locus()),
                            current_size->get_type(),
                            current_size->get_locus()),
                        current_index->get_type(),
                        current_index->get_locus());

                return result;
            }
        }

        Nodecl::NodeclBase get_index_expression(Nodecl::List subscripts, TL::Type subscripted_type)
        {
            ObjectList<Nodecl::NodeclBase> reversed_indexes;
            ObjectList<Nodecl::NodeclBase> reversed_sizes;
            ObjectList<Nodecl::NodeclBase> reversed_lower_bounds;

            for (Nodecl::List::iterator it = subscripts.begin();
                    it != subscripts.end();
                    it++)
            {
                if (it->is<Nodecl::Range>())
                {
                    reversed_indexes.prepend(it->as<Nodecl::Range>().get_lower());
                }
                else
                {
                    reversed_indexes.prepend(*it);
                }
            }

            TL::Type it_type = subscripted_type;
            while (it_type.is_array())
            {
                Nodecl::NodeclBase size = it_type.array_get_size();
                reversed_sizes.prepend(size);
                Nodecl::NodeclBase lower, upper;
                it_type.array_get_bounds(lower, upper);

                reversed_lower_bounds.prepend(lower);

                it_type = it_type.array_element();

            }

            ERROR_CONDITION(reversed_indexes.size() != reversed_sizes.size(), "Mismatch between indexes and dimensions", 0);

            Nodecl::NodeclBase index_expression = get_index_expression_rec(
                    reversed_indexes.begin(),
                    reversed_indexes.end(),

                    reversed_sizes.begin(),
                    reversed_lower_bounds.begin());

            TL::Type index_type = CURRENT_CONFIGURATION->type_environment->type_of_ptrdiff_t();

            Nodecl::NodeclBase result =
                Nodecl::Mul::make(
                        const_value_to_nodecl(const_value_get_signed_int(it_type.get_size())),
                        Nodecl::ParenthesizedExpression::make(
                            index_expression,
                            index_expression.get_type(),
                            index_expression.get_locus()),
                        index_type,
                        index_expression.get_locus()
                        );

            return result;
        }
    }

    Nodecl::NodeclBase DataReference::get_base_address_as_integer() const
    {
        Nodecl::NodeclBase base_address = _base_address;

        if (!base_address.is<Nodecl::Reference>())
            internal_error("Base address is not an address actually", 0);

        base_address = base_address.as<Nodecl::Reference>().get_rhs();

        if (base_address.is<Nodecl::Symbol>())
        {
            return base_address.shallow_copy();
        }
        else if (base_address.is<Nodecl::ArraySubscript>())
        {
            Nodecl::ArraySubscript arr_subscript = base_address.as<Nodecl::ArraySubscript>();

            Nodecl::NodeclBase subscripted = arr_subscript.get_subscripted();
            Nodecl::NodeclBase subscripts = arr_subscript.get_subscripts();

            TL::Type subscripted_type = subscripted.get_type();
            if (subscripted_type.is_any_reference())
                subscripted_type = subscripted_type.references_to();

            if (subscripted_type.is_pointer())
                internal_error("Not yet implemented", 0);

            Nodecl::NodeclBase index_expression = get_index_expression(subscripts.as<Nodecl::List>(), subscripted_type);

            Nodecl::NodeclBase result = Nodecl::Add::make(
                    subscripted.shallow_copy(),
                    Nodecl::ParenthesizedExpression::make(
                        index_expression,
                        index_expression.get_type(),
                        index_expression.get_locus()),
                    index_expression.get_type(),
                    index_expression.get_locus()
                    );

            return result;
        }
        else
        {
            internal_error("Not yet implemented %s", ast_print_node_type(base_address.get_kind()));
        }
    }

    Nodecl::NodeclBase DataReference::get_address_of_symbol_helper(Nodecl::NodeclBase expr, bool reference) const
    {
        if (expr.is<Nodecl::Symbol>())
        {
            TL::Symbol sym = expr.as<Nodecl::Symbol>().get_symbol();
            if (!reference
                    || (sym.get_type().is_array()
                        || (sym.get_type().is_any_reference()
                            && sym.get_type().references_to().is_array())))
            {
                return expr.shallow_copy();
            }
            else
            {
                TL::Type t = expr.get_type();
                if (t.is_any_reference())
                    t = t.references_to();

                return Nodecl::Reference::make(
                        expr.shallow_copy(),
                        t.get_pointer_to(),
                        expr.get_locus());
            }
        }
        else if (expr.is<Nodecl::ArraySubscript>())
        {
            Nodecl::NodeclBase subscripted = expr.as<Nodecl::ArraySubscript>().get_subscripted();

            return get_address_of_symbol_helper(subscripted, /* reference */ false);
        }
        else if (expr.is<Nodecl::Reference>())
        {
            return get_address_of_symbol_helper(expr.as<Nodecl::Reference>().get_rhs(), /* reference */ false);
        }
        else if (expr.is<Nodecl::Shaping>())
        {
            return get_address_of_symbol_helper(expr.as<Nodecl::Shaping>().get_postfix(), /* reference */ false);
        }
        else if (expr.is<Nodecl::Dereference>())
        {
            if (IS_FORTRAN_LANGUAGE
                    && expr.as<Nodecl::Reference>().get_rhs().get_type().no_ref().is_pointer()
                    && expr.as<Nodecl::Reference>().get_rhs().get_type().no_ref().points_to().is_fortran_array())
            {
                return Nodecl::Reference::make(
                        expr.shallow_copy(),
                        expr.get_type().no_ref().get_pointer_to(),
                        expr.get_locus());
            }
            else
            {
                return get_address_of_symbol_helper(expr.as<Nodecl::Dereference>().get_rhs(), /* reference */ false);
            }
        }
        else if (expr.is<Nodecl::ParenthesizedExpression>())
        {
            return get_address_of_symbol_helper(expr.as<Nodecl::ParenthesizedExpression>().get_nest(), reference);
        }
        else if (expr.is<Nodecl::ClassMemberAccess>())
        {
            return get_address_of_symbol_helper(expr.as<Nodecl::ClassMemberAccess>().get_lhs(), /* reference */ true);
        }
        else if (expr.is<Nodecl::Conversion>())
        {
            return get_address_of_symbol_helper(expr.as<Nodecl::Conversion>().get_nest(), reference);
        }
        else
        {
            internal_error("Unhandled case '%s'\n", ast_print_node_type(expr.get_kind()));
        }
    }

    Nodecl::NodeclBase DataReference::get_address_of_symbol() const
    {
        if (!_is_valid)
            return Nodecl::NodeclBase::null();

        return get_address_of_symbol_helper(*this, /* reference */ true);
    }

    Nodecl::NodeclBase DataReference::compute_sizeof_of_type(TL::Type relevant_type, bool ignore_regions) const
    {
        if (relevant_type.is_any_reference())
            relevant_type = relevant_type.references_to();

        if (relevant_type.is_array())
        {
            Nodecl::NodeclBase lower_bound, upper_bound;
            if (!relevant_type.array_is_region() || ignore_regions)
            {
                relevant_type.array_get_bounds(lower_bound, upper_bound);
            }
            else
            {
                relevant_type.array_get_region_bounds(lower_bound, upper_bound);
            }

            Nodecl::NodeclBase array_size;

            if (lower_bound.is_constant()
                    && upper_bound.is_constant())
            {
                if (!relevant_type.array_is_region())
                {
                    array_size = relevant_type.array_get_size();
                }
                else
                {
                    array_size = relevant_type.array_get_region_size();
                }
            }
            else
            {
                // (Upper) - (Lower) + 1
                array_size = Nodecl::Add::make(
                        Nodecl::Minus::make(
                            Nodecl::ParenthesizedExpression::make(
                                upper_bound,
                                TL::Type::get_int_type(),
                                upper_bound.get_locus()),
                            Nodecl::ParenthesizedExpression::make(
                                lower_bound,
                                TL::Type::get_int_type(),
                                lower_bound.get_locus()),
                            TL::Type::get_int_type(),
                            upper_bound.get_locus()),
                        const_value_to_nodecl(const_value_get_signed_int(1)),
                        TL::Type::get_int_type(),
                        lower_bound.get_locus());
            }

            Nodecl::NodeclBase element_size = compute_sizeof_of_type(relevant_type.array_element(), ignore_regions);

            return Nodecl::Mul::make(
                    Nodecl::ParenthesizedExpression::make(
                        element_size,
                        TL::Type::get_int_type(),
                        element_size.get_locus()),
                    Nodecl::ParenthesizedExpression::make(
                        array_size,
                        TL::Type::get_int_type(),
                        array_size.get_locus()),
                    TL::Type::get_int_type(),
                    element_size.get_locus());

            return array_size;
        }
        else
        {
            return const_value_to_nodecl(
                    // FIXME - This should be size_t
                    const_value_get_signed_int(
                        relevant_type.get_size()));
        }
    }

    Nodecl::NodeclBase DataReference::compute_offsetof(Nodecl::NodeclBase expr,
            Nodecl::NodeclBase reference_expr,
            TL::Scope scope) const
    {
        if (expr.is<Nodecl::ArraySubscript>())
        {
            // E[X]
            // E[X:Y]
            // E[X;Y]
            //
            // Note that E[X:Y] and E[X;Y] will have array (with region) type
            // while E may have array (with or without region) or pointer type
            Nodecl::ArraySubscript array_subscript = expr.as<Nodecl::ArraySubscript>();
            TL::Type t = _data_type;

            Nodecl::List subscripts = array_subscript.get_subscripts().as<Nodecl::List>();
            Nodecl::List::iterator it = subscripts.begin(), first = it;

            TL::ObjectList<Nodecl::NodeclBase> indexes;
            TL::ObjectList<Nodecl::NodeclBase> sizes;

            while (it != subscripts.end())
            {
                Nodecl::NodeclBase lower_bound;
                if (t.is_array())
                {
                    Nodecl::NodeclBase upper_bound;
                    t.array_get_bounds(lower_bound, upper_bound);
                }
                else if (t.is_pointer())
                {
                    lower_bound = const_value_to_nodecl(const_value_get_zero(4, 1));
                }
                else
                {
                    internal_error("Unexpected type %s", print_declarator(t.get_internal_type()));
                }

                Nodecl::NodeclBase lower = *it;
                if (it->is<Nodecl::Range>())
                {
                    lower = it->as<Nodecl::Range>().get_lower();
                }

                if (lower_bound.is_null() && IS_FORTRAN_LANGUAGE)
                {
                    /*if (t.array_requires_descriptor()
                      && _base_symbol.is_parameter())
                      {
                    // This is an assumed shape of 1
                    lower_bound = const_value_to_nodecl(const_value_get_one(4, 1));
                    }
                    else */ if (reference_expr.is_null())
                    {
                        return Nodecl::NodeclBase::null();
                    }
                    else
                    {
                        DataReference data_ref(reference_expr);
                        if (data_ref.is_valid())
                        {
                            Source lbound_src;
                            lbound_src << "LBOUND(" << data_ref.get_base_symbol().get_name() << ", DIM = " << 
                                ::fortran_get_rank_of_type(t.get_internal_type()) << ")";

                            lower_bound = lbound_src.parse_expression(scope);
                        }
                        else
                        {
                            return Nodecl::NodeclBase::null();
                        }
                    }
                }

                // This means that it is OK to specify A(:)
                if (lower.is_null())
                    lower = lower_bound;

                Nodecl::NodeclBase current_index =
                    Nodecl::ParenthesizedExpression::make(
                            Nodecl::Minus::make(
                                Nodecl::ParenthesizedExpression::make(
                                    lower.shallow_copy(),
                                    TL::Type::get_int_type(),
                                    expr.get_locus()),
                                Nodecl::ParenthesizedExpression::make(
                                    lower_bound.shallow_copy(),
                                    TL::Type::get_int_type(),
                                    lower_bound.get_locus()),
                                TL::Type::get_int_type(),
                                expr.get_locus()),
                            TL::Type::get_int_type(),
                            expr.get_locus());

                indexes.append(current_index);
                if (t.is_array())
                {
                    Nodecl::NodeclBase current_size = t.array_get_size();
                    if (current_size.is_null())
                    {
                        if (IS_FORTRAN_LANGUAGE && !reference_expr.is_null())
                        {
                            DataReference data_ref(reference_expr);
                            if (!data_ref.is_valid())
                                return Nodecl::NodeclBase::null();

                            Source lbound_src;
                            lbound_src << "SIZE(" << data_ref.get_base_symbol().get_name() << ", DIM = " << 
                                ::fortran_get_rank_of_type(t.get_internal_type()) << ")";

                            current_size = lbound_src.parse_expression(scope);
                        }
                        else
                        {
                            if ((it + 1) != subscripts.end())
                            {
                                return Nodecl::NodeclBase::null();
                            }
                            current_size = Nodecl::NodeclBase::null();
                        }
                    }
                    sizes.append(current_size.shallow_copy());
                }
                else
                {
                    // This way it will have the same length as indexes
                    sizes.append(const_value_to_nodecl(const_value_get_one(4, 1)));
                }


                if (t.is_array())
                {
                    t = t.array_element();
                }
                else if (t.is_pointer())
                {
                    t = t.points_to();
                }

                it++;
            }

            ERROR_CONDITION(indexes.size() != sizes.size(), "Mismatch between indexes and sizes", 0);

            TL::ObjectList<Nodecl::NodeclBase>::iterator it_indexes = indexes.begin();
            TL::ObjectList<Nodecl::NodeclBase>::iterator it_sizes = sizes.begin();

            Nodecl::NodeclBase result;

            // Horner algorithm
            while (it_indexes != indexes.end())
            {
                // First one is special
                if (it_indexes == indexes.begin())
                {
                    result = *it_indexes;
                }
                else
                {
                    result = Nodecl::Add::make(
                            Nodecl::ParenthesizedExpression::make(
                                Nodecl::Mul::make(
                                    Nodecl::ParenthesizedExpression::make(*it_sizes, it_sizes->get_type()),
                                    Nodecl::ParenthesizedExpression::make(result, result.get_type()),
                                    get_ptrdiff_t_type()), get_ptrdiff_t_type()),
                            Nodecl::ParenthesizedExpression::make(*it_indexes, it_indexes->get_type()),
                            get_ptrdiff_t_type());
                }

                it_indexes++;
                it_sizes++;
            }

            result = Nodecl::Mul::make(
                    compute_sizeof_of_type(t, /* ignore regions */ true),
                    Nodecl::ParenthesizedExpression::make(result, result.get_type()),
                    get_ptrdiff_t_type(),
                    result.get_locus());

            Nodecl::NodeclBase subscripted = array_subscript.get_subscripted();
            while (subscripted.is<Nodecl::Conversion>())
            {
                subscripted = subscripted.as<Nodecl::Conversion>().get_nest();
            }

            // a.b[e]
            // (p[e1])[e] -> This only happens when indexing a pointer p
            if (subscripted.is<Nodecl::ClassMemberAccess>()
                    || subscripted.is<Nodecl::ArraySubscript>())
            {
                DataReference subscripted_expr_ref = DataReference(subscripted);
                Nodecl::NodeclBase result_subscripted = compute_offsetof(subscripted, subscripted_expr_ref, scope);

                result = Nodecl::Add::make(
                        Nodecl::ParenthesizedExpression::make(result_subscripted, result_subscripted.get_type()),
                        Nodecl::ParenthesizedExpression::make(result, result.get_type()),
                        get_ptrdiff_t_type(),
                        expr.get_locus());
            }
            // (*p)[e]
            else if (subscripted.is<Nodecl::Dereference>())
            {
                DataReference subscripted_expr_ref = DataReference(subscripted.as<Nodecl::Dereference>().get_rhs());
                Nodecl::NodeclBase result_subscripted = compute_offsetof(subscripted.as<Nodecl::Dereference>().get_rhs(), subscripted_expr_ref, scope);

                result = Nodecl::Add::make(
                        Nodecl::ParenthesizedExpression::make(result_subscripted, result_subscripted.get_type()),
                        Nodecl::ParenthesizedExpression::make(result, result.get_type()),
                        get_ptrdiff_t_type(),
                        expr.get_locus());
            }
            // ([N]p)[X:Y]
            else if (subscripted.is<Nodecl::Shaping>())
            {
                Nodecl::Shaping shaping = subscripted.as<Nodecl::Shaping>();
                DataReference shaped_data_ref = shaping.get_postfix();

                if (!shaped_data_ref.is_valid())
                    return Nodecl::NodeclBase::null();

                Nodecl::NodeclBase shaped_offset = compute_offsetof(
                        shaping.get_postfix(),
                        shaped_data_ref,
                        scope);

                result = Nodecl::Add::make(
                        result,
                        shaped_offset,
                        get_ptrdiff_t_type(),
                        expr.get_locus());
            }
            // a[e]
            else if (subscripted.is<Nodecl::Symbol>())
            {
                // Do nothing
            }
            else
            {
                internal_error("Unexpected node %s",
                        ast_print_node_type(subscripted.get_kind()));
            }

            return result;
        }
        else if (expr.is<Nodecl::ClassMemberAccess>())
        {
            // a.b
            Nodecl::NodeclBase result = Nodecl::Minus::make(
                    Nodecl::Cast::make(
                        Nodecl::Reference::make(
                            expr,
                            expr.get_type().get_pointer_to(),
                            expr.get_locus()),
                        get_ptrdiff_t_type(),
                        "C", expr.get_locus()),
                    Nodecl::Cast::make(
                        Nodecl::Reference::make(
                            expr.as<Nodecl::ClassMemberAccess>().get_lhs(),
                            expr.as<Nodecl::ClassMemberAccess>().get_lhs().get_type().get_pointer_to(),
                            expr.get_locus()),
                        get_ptrdiff_t_type(),
                        "C", expr.get_locus()),
                    get_ptrdiff_t_type(),
                    expr.get_locus());

            return result;
        }
        else if (expr.is<Nodecl::Shaping>())
        {
            // A shaping itself does not imply any offset
        }

        return const_value_to_nodecl(const_value_get_integer(0, type_get_size(get_ptrdiff_t_type()), 1));
    }

    Nodecl::NodeclBase DataReference::get_offsetof() const
    {
        Nodecl::NodeclBase offset = this->compute_offsetof(*this, Nodecl::NodeclBase::null(), CURRENT_COMPILED_FILE->global_decl_context);
        return offset;
    }

    Nodecl::NodeclBase DataReference::get_offsetof(Nodecl::NodeclBase reference, TL::Scope sc) const
    {
        Nodecl::NodeclBase offset = this->compute_offsetof(*this, reference, sc);
        return offset;
    }

    Nodecl::NodeclBase DataReference::get_sizeof() const
    {
        return compute_sizeof_of_type(_data_type);
    }

    DataReference::~DataReference()
    {
    }

    void DataReference::module_write(ModuleWriter& mw)
    {
        mw.write((Nodecl::NodeclBase&)*this);
        mw.write(_is_valid);

        mw.write(_base_symbol);
        mw.write(_data_type);

        mw.write(_error_log);

        mw.write(_base_address);
    }

    void DataReference::module_read(ModuleReader& mr)
    {
        mr.read((Nodecl::NodeclBase&)*this);
        mr.read(_is_valid);

        mr.read(_base_symbol);
        mr.read(_data_type);

        mr.read(_error_log);

        mr.read(_base_address);
    }
}
