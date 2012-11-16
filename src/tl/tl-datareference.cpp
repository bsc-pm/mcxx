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
                    tree.get_locus() + ": error: expression '" + tree.prettyprint() + "' not allowed in data-reference\n";
            }

            // Symbol
            virtual void visit(const Nodecl::Symbol& sym)
            {
                _data_ref._base_symbol = sym.get_symbol();

                TL::Type t;

                if (sym.get_type().is_valid())
                {
                    t = sym.get_type();
                }
                else
                {
                    t = sym.get_symbol().get_type();
                }

                if (t.is_any_reference())
                    t = t.references_to();

                _data_ref._data_type = t;
                _data_ref._base_address = Nodecl::Reference::make(
                        sym.shallow_copy(),
                        t.get_pointer_to(),
                        sym.get_filename(),
                        sym.get_line());
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
                _data_ref._base_address = derref.get_rhs().shallow_copy();
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

                if (t.is_pointer())
                {
                    t = t.points_to();
                    ERROR_CONDITION(subscripts.size() != 1, "Invalid number of subscript items (%d) for a pointer subscript",
                            subscripts.size());

                    Nodecl::NodeclBase first = subscripts[0];

                    if (first.is<Nodecl::Range>())
                    {
                        lower_bounds.push_back(first.as<Nodecl::Range>().get_lower().shallow_copy());
                        upper_bounds.push_back(first.as<Nodecl::Range>().get_upper().shallow_copy());
                    }
                    else
                    {
                        lower_bounds.push_back(first.shallow_copy());
                        upper_bounds.push_back(first.shallow_copy());
                    }
                }
                else
                {
                    while (t.is_array())
                    {
                        Nodecl::NodeclBase lb, ub;

                        t.array_get_bounds(lb, ub);

                        lower_bounds.push_back(lb.shallow_copy());
                        upper_bounds.push_back(ub.shallow_copy());

                        t = t.array_element();
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
                                    item.get_filename(),
                                    item.get_line());

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
                bool have_to_rebuild_type = false;

                TL::Type t = array.get_type();
                if (t.is_any_reference())
                    t = t.references_to();

                Nodecl::List subscripts = array.get_subscripts().as<Nodecl::List>();
                Nodecl::List low_subscripts_list;

                TL::ObjectList<Nodecl::NodeclBase> low_subscripts;

                for (Nodecl::List::iterator it = subscripts.begin();
                        it != subscripts.end();
                        it++)
                {
                    if (it->is<Nodecl::Range>())
                    {
                        low_subscripts.push_back(it->as<Nodecl::Range>().get_lower().shallow_copy());
                    }
                    else
                    {
                        low_subscripts.push_back(it->shallow_copy());
                        have_to_rebuild_type = true;
                    }
                }

                if (have_to_rebuild_type)
                {
                    _data_ref._data_type = extend_array_type_to_regions(array);
                }
                else
                {
                    _data_ref._data_type = t;
                }

                if (array.get_subscripted().is<Nodecl::Shaping>())
                {
                    // The base address of a shaping expression is itself
                }
                else
                {
                    ERROR_CONDITION(!_data_ref._base_address.is<Nodecl::Reference>(), 
                            "Invalid address for the subscripted expression", 0);

                    _data_ref._base_address =
                        Nodecl::Reference::make(
                                Nodecl::ArraySubscript::make(
                                    _data_ref._base_address.as<Nodecl::Reference>().get_rhs(),
                                    Nodecl::List::make(low_subscripts),
                                    t,
                                    array.get_filename(),
                                    array.get_line()
                                    ),
                                t.get_pointer_to(),
                                array.get_filename(),
                                array.get_line());
                }
            }

            virtual void visit(const Nodecl::ClassMemberAccess& member)
            {
                walk(member.get_lhs());

                if (!_data_ref._is_valid)
                    return;

                // Ignore the object 'this' of C++
                if (IS_CXX_LANGUAGE
                        && _data_ref._base_symbol.get_name() == "this")
                {
                    walk(member.get_member());
                    return;
                }


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
                                    t,
                                    member.get_filename(),
                                    member.get_line()
                                    ),
                                t.get_pointer_to(),
                                member.get_filename(),
                                member.get_line());
                }
                else if (IS_CXX_LANGUAGE
                        && member.get_member().get_kind() == NODECL_SYMBOL
                        && _data_ref._base_address.get_kind() == NODECL_SYMBOL
                        && _data_ref._base_address.get_symbol().get_name() == "this")
                {
                    _data_ref._base_address =
                        Nodecl::Reference::make(
                                Nodecl::ClassMemberAccess::make(
                                    _data_ref._base_address,
                                    member.get_member().shallow_copy(),
                                    t,
                                    member.get_filename(),
                                    member.get_line()
                                    ),
                                t.get_pointer_to(),
                                member.get_filename(),
                                member.get_line());
                }
                else
                {
                    internal_error("Unexpected node kind '%s'\n", ast_print_node_type(member.get_member().get_kind()));
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

    Nodecl::NodeclBase DataReference::get_address_of_symbol_helper(Nodecl::NodeclBase expr) const
    {
        if (expr.is<Nodecl::Symbol>())
        {
            TL::Symbol sym = expr.as<Nodecl::Symbol>().get_symbol();
            if (sym.get_type().is_array()
                    || (sym.get_type().is_any_reference()
                        && sym.get_type().references_to().is_array()))
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
                        expr.get_filename(),
                        expr.get_line());
            }
        }
        else if (expr.is<Nodecl::ArraySubscript>())
        {
            Nodecl::NodeclBase subscripted = expr.as<Nodecl::ArraySubscript>().get_subscripted();

            if ((IS_C_LANGUAGE
                        || IS_CXX_LANGUAGE)
                    && (subscripted.get_type().is_pointer()
                        || (subscripted.get_type().is_any_reference()
                            && subscripted.get_type().references_to().is_pointer())))
            {
                return subscripted.shallow_copy();
            }
            else
            {
                return get_address_of_symbol_helper(subscripted);
            }
        }
        else if (expr.is<Nodecl::Reference>())
        {
            return expr.as<Nodecl::Reference>().get_rhs();
        }
        else if (expr.is<Nodecl::Shaping>())
        {
            Nodecl::NodeclBase postfix = expr.as<Nodecl::Shaping>().get_postfix();

            if ((IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    && (postfix.get_type().is_pointer()
                        || (postfix.get_type().is_any_reference()
                            && postfix.get_type().references_to().is_pointer())))
            {
                return postfix.shallow_copy();
            }
            else
            {
                return get_address_of_symbol_helper(postfix);
            }
        }
        else if (expr.is<Nodecl::Dereference>())
        {
            return expr.as<Nodecl::Dereference>().get_rhs();
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

        return get_address_of_symbol_helper(*this);
    }

    Nodecl::NodeclBase DataReference::compute_sizeof_of_type(TL::Type relevant_type) const
    {
        if (relevant_type.is_any_reference())
            relevant_type = relevant_type.references_to();

        if (relevant_type.is_array())
        {
            Nodecl::NodeclBase lower_bound, upper_bound;
            if (!relevant_type.array_is_region())
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
                                upper_bound.get_filename(),
                                upper_bound.get_line()),
                            Nodecl::ParenthesizedExpression::make(
                                lower_bound,
                                TL::Type::get_int_type(),
                                lower_bound.get_filename(),
                                lower_bound.get_line()),
                            TL::Type::get_int_type(),
                            upper_bound.get_filename(),
                            upper_bound.get_line()),
                        const_value_to_nodecl(const_value_get_signed_int(1)),
                        TL::Type::get_int_type(),
                        lower_bound.get_filename(),
                        lower_bound.get_line());
            }

            Nodecl::NodeclBase element_size = compute_sizeof_of_type(relevant_type.array_element());

            return Nodecl::Mul::make(
                    Nodecl::ParenthesizedExpression::make(
                        element_size,
                        TL::Type::get_int_type(),
                        element_size.get_filename(),
                        element_size.get_line()),
                    Nodecl::ParenthesizedExpression::make(
                        array_size,
                        TL::Type::get_int_type(),
                        array_size.get_filename(),
                        array_size.get_line()),
                    TL::Type::get_int_type(),
                    element_size.get_filename(),
                    element_size.get_line());

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
            Nodecl::ArraySubscript array_subscript = expr.as<Nodecl::ArraySubscript>();
            Nodecl::NodeclBase subscripted = array_subscript.get_subscripted();
            TL::Type t = subscripted.get_type();
            if (t.is_any_reference())
                t = t.references_to();

            Nodecl::List subscripts = array_subscript.get_subscripts().as<Nodecl::List>();

            Nodecl::NodeclBase result;

            if (t.is_array())
            {
                Nodecl::List::iterator it = subscripts.begin(), first = it;

                while (t.is_array()
                        && it != subscripts.end())
                {
                    Nodecl::NodeclBase lower_bound, upper_bound;

                    t.array_get_bounds(lower_bound, upper_bound);

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

                    Nodecl::NodeclBase current_index =
                        Nodecl::ParenthesizedExpression::make(
                                Nodecl::Minus::make(
                                    Nodecl::ParenthesizedExpression::make(
                                        lower.shallow_copy(),
                                        TL::Type::get_int_type(),
                                        lower.get_filename(),
                                        lower.get_line()),
                                    Nodecl::ParenthesizedExpression::make(
                                        lower_bound.shallow_copy(),
                                        TL::Type::get_int_type(),
                                        lower_bound.get_filename(),
                                        lower_bound.get_line()),
                                    TL::Type::get_int_type(),
                                    lower.get_filename(),
                                    lower.get_line()),
                                TL::Type::get_int_type(),
                                lower.get_filename(),
                                lower.get_line());

                    if (it == first)
                    {
                        result = current_index;
                    }
                    else
                    {
                        result = Nodecl::Mul::make(
                                Nodecl::ParenthesizedExpression::make(
                                    result,
                                    result.get_type(),
                                    result.get_filename(),
                                    result.get_line()),
                                current_index,
                                TL::Type::get_int_type(),
                                lower.get_filename(),
                                lower.get_line());
                    }

                    t = t.array_element();
                    it++;
                }

                ERROR_CONDITION( (!t.is_array() != (it == subscripts.end())), "Mismatch between array type and subscripts", 0);

                result = Nodecl::Mul::make(
                        const_value_to_nodecl(const_value_get_signed_int(type_get_size(t.get_internal_type()))),
                        result,
                        TL::Type::get_int_type(),
                        result.get_filename(),
                        result.get_line());
            }
            else if (t.is_pointer())
            {
                // In C/C++ one can index a pointer using array notation
                Nodecl::NodeclBase current_subscript = subscripts[0];

                if (current_subscript.is<Nodecl::Range>())
                {
                    current_subscript = current_subscript.as<Nodecl::Range>().get_lower();
                }

                result = Nodecl::Mul::make(
                        const_value_to_nodecl(const_value_get_signed_int(type_get_size(t.points_to().get_internal_type()))),
                        current_subscript.shallow_copy(),
                        TL::Type::get_int_type(),
                        current_subscript.get_filename(),
                        current_subscript.get_line());
            }
            else
            {
                internal_error("Code unreachable", 0);
            }


            // a.b[e]
            if (array_subscript.get_subscripted().is<Nodecl::ClassMemberAccess>())
            {
                internal_error("Not yet implemented", 0);
            }

            return result;
        }
        else if (expr.is<Nodecl::ClassMemberAccess>())
        {
            // a.b
            internal_error("Not yet implemented", 0);
        }

        return const_value_to_nodecl(const_value_get_signed_int(0));
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
