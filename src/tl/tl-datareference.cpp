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


            Nodecl::NodeclBase make_sizeof(Nodecl::NodeclBase ctr)
            {
                Type t = ctr.get_type();
                if (t.is_any_reference())
                    t = t.references_to();

                return Nodecl::Sizeof::make(
                        Nodecl::Type::make(
                            t,
                            ctr.get_filename(),
                            ctr.get_line()),
                        ctr.shallow_copy(),
                        Type(::get_size_t_type()),
                        ctr.get_filename(),
                        ctr.get_line());
            }

            virtual void unhandled_node(const Nodecl::NodeclBase & tree) 
            { 
                _data_ref._is_valid = false;
                _data_ref._error_log = 
                    tree.get_locus() + ": error: expression not allowed in data-reference\n";
            }

            // Symbol
            virtual void visit(const Nodecl::Symbol& sym)
            {
                _data_ref._base_symbol = sym.get_symbol();

                TL::Type t = sym.get_symbol().get_type();
                if (t.is_any_reference())
                    t = t.references_to();

                _data_ref._data_type = t;
                _data_ref._base_address = Nodecl::Reference::make(
                        sym.shallow_copy(),
                        t.get_pointer_to(),
                        sym.get_filename(),
                        sym.get_line());
                _data_ref._sizeof = make_sizeof(sym);
            }

            virtual void visit(const Nodecl::Dereference& derref)
            {
                if (derref.get_rhs().is<Nodecl::Reference>())
                {
                    // *&a is like a
                    walk(derref.get_rhs().as<Nodecl::Reference>().get_rhs());
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
                _data_ref._sizeof = make_sizeof(derref);
            }

            virtual void visit(const Nodecl::Reference& ref)
            {
                if (ref.get_rhs().is<Nodecl::Dereference>())
                {
                    // &*a is like a
                    walk(ref.get_rhs().as<Nodecl::Dereference>().get_rhs());
                    return;
                }

                unhandled_node(ref);
                // walk(ref.get_rhs());
                // if (!_data_ref.is_valid())
                //     return;

                // TL::Type t = ref.get_type();
                // _data_ref._data_type = t;
                // _data_ref._sizeof = make_sizeof(ref);
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
                                    const_value_to_nodecl(const_value_get_signed_int(1)),
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
                _data_ref._sizeof = make_sizeof(array);
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
                _data_ref._sizeof = make_sizeof(member);
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
                _data_ref._sizeof = make_sizeof(shaping_expr);
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

    Nodecl::NodeclBase DataReference::get_sizeof() const
    {
        return _sizeof.shallow_copy();
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
        mw.write(_sizeof);
    }

    void DataReference::module_read(ModuleReader& mr)
    {
        mr.read((Nodecl::NodeclBase&)*this);
        mr.read(_is_valid);

        mr.read(_base_symbol);
        mr.read(_data_type);

        mr.read(_error_log);

        mr.read(_base_address);
        mr.read(_sizeof);
    }
}
