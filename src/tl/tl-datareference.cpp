/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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
#include "tl-symbol.hpp"
#include "cxx-utils.h"

using namespace TL;


DataReference::DataReference(AST_t ast, ScopeLink scope_link)
    : Expression(ast, scope_link),
    _valid(), _base_symbol(NULL), _type(NULL), _size(), _addr()
{
    _valid = gather_info_data_expr(*this, _base_symbol, _size, _addr, _type);
}

DataReference::DataReference(Expression expr)
    : Expression(expr.get_ast(), expr.get_scope_link()), 
    _valid(), _base_symbol(NULL), _type(NULL), _size(), _addr()
{
    _valid = gather_info_data_expr(*this, _base_symbol, _size, _addr, _type);
}

bool DataReference::is_valid() const
{
    return _valid;
}

Symbol DataReference::get_base_symbol() const
{
    return _base_symbol;
}

Source DataReference::get_address() const
{
    return _addr;
}

Source DataReference::get_sizeof() const
{
    return _size;
}

Type DataReference::get_data_type() const
{
    return _type;
}

bool DataReference::gather_info_data_expr_rec(Expression expr, 
        Symbol &base_sym, 
        Source &size, 
        Source &addr, 
        Type &type,
        bool enclosing_is_array)
{
    if (expr.is_id_expression()
            || expr.is_this_variable())
    {
        Symbol sym(NULL);

        if (expr.is_this_variable())
        {
            sym = expr.get_this_symbol();
        } 
        else
        {
            sym = expr.get_id_expression().get_computed_symbol();
        }

        if (!sym.is_valid())
            return false;

        if (!sym.is_variable())
            return false;

        base_sym = sym;

        type = sym.get_type();

        if (type.is_reference())
            type = type.references_to();

        if (type.is_array()
                || enclosing_is_array)
        {
            addr = sym.get_qualified_name();
        }
        else
        {
            addr = "&" + sym.get_qualified_name();
        }
        size = "sizeof(" + type.get_declaration(expr.get_scope(), "") + ")";

        return true;
    }
    else if (expr.is_array_subscript())
    {
        Source arr_size, arr_addr;
        Type arr_type(NULL);
        bool b = gather_info_data_expr_rec(expr.get_subscripted_expression(),
                base_sym,
                arr_size,
                arr_addr,
                arr_type,
                /* enclosing_is_array */ true);
        if (!b)
            return false;

        if (arr_type.is_array())
        {
            type = arr_type.array_element();
        }
        else if (arr_type.is_pointer())
        {
            type = arr_type.points_to();
        }
        else
        {
            return false;
        }

        size = "sizeof(" + type.get_declaration(expr.get_scope(), "") + ")";
        addr = arr_addr << "[" << expr.get_subscript_expression() << "]";
        if (!enclosing_is_array)
        {
            addr = "&(" + addr.get_source() + ")";
        }

        return true;
    }
    else if (expr.is_array_section_range()
            || expr.is_array_section_size())
    {
        Source arr_size, arr_addr;
        Type arr_type(NULL);
        bool b = gather_info_data_expr_rec(expr.array_section_item(),
                base_sym,
                arr_size,
                arr_addr,
                arr_type,
                /* enclosing_is_array */ true);
        if (!b)
            return false;

        if (arr_type.is_array())
        {
            type = arr_type.array_element();
        }
        else if (arr_type.is_pointer())
        {
            type = arr_type.points_to();
        }
        else
        {
            return false;
        }

        Source upper_bound;

        if (expr.is_array_section_size())
        {
            upper_bound 
                << expr.array_section_lower() << " + " << expr.array_section_upper()
                ;
        }
        else // expr.is_array_section_range
        {
            upper_bound 
                << expr.array_section_upper()
                ;
        }

        Source base_expr;

        if (expr.array_section_item().is_shaping_expression())
        {
            base_expr << expr.array_section_item().shaped_expression();
        }
        else
        {
            base_expr << expr.array_section_item();
        }
        
        type = type.get_array_to(expr.array_section_lower().get_ast(), expr.array_section_upper().get_ast(), expr.get_scope());
        addr = arr_addr << "[" << expr.array_section_lower().prettyprint() << "]";
        if (!enclosing_is_array)
        {
            addr = "&(" + addr.get_source() + ")";
        }
        size = "sizeof(" + type.get_declaration(expr.get_scope(), "") + ")";

        return true;
    }
    else if (expr.is_unary_operation())
    {
        // Simplify &(*a)
        if (expr.get_operation_kind() == Expression::REFERENCE)
        {
            Expression ref_expr = expr.get_unary_operand();
            if (ref_expr.is_unary_operation()
                    && ref_expr.get_operation_kind() == Expression::DERREFERENCE)
            {
                return gather_info_data_expr_rec(ref_expr.get_unary_operand(),
                        base_sym,
                        size, 
                        addr, 
                        type,
                        enclosing_is_array);
            }
            else if (ref_expr.is_array_subscript())
            {
                return gather_info_data_expr_rec(ref_expr.get_subscripted_expression(),
                        base_sym,
                        size,
                        addr,
                        type,
                        enclosing_is_array);
            }
            else if (ref_expr.is_array_section_range()
                    || ref_expr.is_array_section_size())
            {
                return gather_info_data_expr_rec(ref_expr.array_section_item(),
                        base_sym,
                        size,
                        addr,
                        type,
                        enclosing_is_array);
            }
        }
        // Simplify *(&a)
        else if (expr.get_operation_kind() == Expression::DERREFERENCE)
        {
            Expression ref_expr = expr.get_unary_operand();
            if (ref_expr.is_unary_operation()
                    && ref_expr.get_operation_kind() == Expression::REFERENCE)
            {
                return gather_info_data_expr_rec(ref_expr.get_unary_operand(),
                        base_sym,
                        size, 
                        addr, 
                        type,
                        enclosing_is_array);
            }
            else
            {
                Source ptr_size, ptr_addr;
                bool b = gather_info_data_expr_rec(ref_expr,
                        base_sym,
                        ptr_size, 
                        ptr_addr, 
                        type,
                        enclosing_is_array);

                if (!b)
                    return false;

                if (type.is_pointer())
                {
                    type = type.points_to();
                }
                else if (type.is_array())
                {
                    type = type.array_element();
                }

                size = "sizeof(" + type.get_declaration(ref_expr.get_scope(), "") + ")";
                addr = "(" + ptr_addr.get_source() + ")";

                return true;
            }
        }

        return false;
    }
    else if (expr.is_shaping_expression())
    {
        Expression shaped_expr = expr.shaped_expression();

        Source arr_size, arr_addr;

        bool b = gather_info_data_expr_rec(shaped_expr, base_sym, 
                arr_size, arr_addr, 
                type, /* enclosing_is_array */ true);

        if (!b)
            return false;

        CXX_LANGUAGE()
        {
            if (type.is_reference())
            {
                type = type.references_to();
            }
        }

        if (!type.is_pointer())
            return false;

        type = type.points_to();

        ObjectList<Expression> shape_list = expr.shape_list();
        for (ObjectList<Expression>::iterator it = shape_list.begin();
                it != shape_list.end();
                it++)
        {
            type = type.get_array_to(it->get_ast(), it->get_scope());
        }
        size = "sizeof(" + type.get_declaration(expr.get_scope(), "") + ")";
        addr = arr_addr;

        return true;
    }
    else if (expr.is_member_access())
    {
        Expression obj_expr = expr.get_accessed_entity();

        Source obj_addr, obj_size;

        bool b = gather_info_data_expr_rec(obj_expr, base_sym, 
                obj_size, obj_addr, 
                type,
                /* enclosing_is_array */ false);

        if (!b)
            return false;

        Symbol member = expr.get_accessed_member().get_computed_symbol();
        if (!member.is_valid())
            return false;

        type = member.get_type();

        CXX_LANGUAGE()
        {
           type = type.get_reference_to();
        }

        addr = obj_addr << "." << expr.get_accessed_member().prettyprint();
        if (!enclosing_is_array)
        {
            addr = "&(" + addr.get_source() + ")";
        }
        size = "sizeof(" + type.get_declaration(expr.get_scope(), "") + ")";

        return true;
    }
    return false;
}

bool DataReference::gather_info_data_expr(Expression &expr, Symbol& base_sym,
        Source &size, Source &addr, Type &type)
{
    return gather_info_data_expr_rec(expr, base_sym, size, addr, type, /* enclosing_is_array */ false);
}
