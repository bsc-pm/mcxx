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
#include "tl-symbol.hpp"
#include "cxx-utils.h"

using namespace TL;


DataReference::DataReference(AST_t ast, ScopeLink scope_link)
    : Expression(ast, scope_link),
    _valid(), _base_symbol(NULL), _type(NULL), _size(), _addr()
{
    _valid = gather_info_data_expr(*this, _base_symbol, _size, _addr, _type, _warnlog);
}

DataReference::DataReference(Expression expr)
    : Expression(expr.get_ast(), expr.get_scope_link()), 
    _valid(), _base_symbol(NULL), _type(NULL), _size(), _addr()
{
    _valid = gather_info_data_expr(*this, _base_symbol, _size, _addr, _type, _warnlog);
}

bool DataReference::is_valid() const
{
    std::string dummy;

    return is_valid(dummy);
}

bool DataReference::is_valid(std::string& reason) const
{
    reason = _warnlog.str();
    return _valid;
}

std::string DataReference::get_warning_log() const
{
    return _warnlog.str();
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
        bool enclosing_is_array,
        bool & pointer_member_access,
        std::stringstream& warnlog)
{
    if (expr.is_id_expression()
            || expr.is_this_variable()
            || expr.is_accessed_member())
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
        {
            warnlog << expr.get_ast().get_locus() << ": warning: unknown symbol in data-reference '" << expr.prettyprint() << "'" 
                    << std::endl;
            return false;
        }

        if (!sym.is_variable())
        {
            warnlog << expr.get_ast().get_locus() << ": warning: symbol in data-reference '" << expr.prettyprint() << "' " 
                    << "is not a variable" << std::endl;
            return false;
        }

        base_sym = sym;
        Source this_accessor; 
        if(expr.is_id_expression() && sym.is_member() && !sym.is_static()) 
        {
            this_accessor << "this->";
        }

        type = sym.get_type();

        if (type.is_reference())
            type = type.references_to();

        if (type.is_array()
                || enclosing_is_array)
        {
            addr << this_accessor << sym.get_qualified_name();
        }
        else
        {
            addr << "&" << this_accessor << sym.get_qualified_name();
        }
        size = safe_expression_size(type, expr.get_scope());

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
                /* enclosing_is_array */ true,
                pointer_member_access,
                warnlog);
        if (!b)
        {
            return false;
        }

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
            warnlog << expr.get_ast().get_locus() 
                << ": warning: array subscript in data-reference '" 
                << expr.prettyprint() 
                << "' is not a variable" << std::endl;
            return false;
        }

        size = safe_expression_size(type, expr.get_scope());
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
        ObjectList<Expression> range_set;

        Expression current_expr = expr;
        while (current_expr.is_array_section_range()
                || current_expr.is_array_section_size())
        {
            range_set.append(current_expr);
            current_expr = current_expr.array_section_item();
        }

        // Now check that everything makes sense.
        // - We allow a pointer or an array at the first dimension
        // - Every other dimension must be an array
        Type current_type = current_expr.get_type();
        for (ObjectList<Expression>::iterator it = range_set.begin();
                it != range_set.end();
                it++)
        {
            if (current_type.is_reference())
                current_type = current_type.references_to();

            if (!(current_type.is_array()
                    || (current_type.is_pointer()
                        && it == range_set.begin())))
            {
                // Not valid
                if (!current_type.is_array()
                        && !current_type.is_pointer())
                {
                    warnlog << expr.get_ast().get_locus() 
                        << ": warning: array section in data-reference '" 
                        << expr.prettyprint() 
                        << "' is not a pointer or array type" << std::endl;
                }
                else if (current_type.is_pointer())
                {
                    warnlog << expr.get_ast().get_locus() 
                        << ": warning: array section in data-reference '" 
                        << expr.prettyprint() 
                        << "' is a pointer type but it is not the first section" << std::endl;
                }
                return false;
            }

            if (current_type.is_array())
            {
                current_type = current_type.array_element();
            }
            else
            {
                current_type = current_type.points_to();
            }
        }

        Source arr_size, arr_addr;
        Type arr_type(NULL);
        bool b = gather_info_data_expr_rec(current_expr,
                base_sym,
                arr_size,
                arr_addr,
                arr_type,
                /* enclosing_is_array */ true,
                pointer_member_access,
                warnlog);
        if (!b)
            return false;

        // Now rebuild the type
        Type rebuilt_type = current_type;
        for (ObjectList<Expression>::iterator it = range_set.begin();
                it != range_set.end();
                it++)
        {
            Expression& section(*it);
            
            Source upper_bound;
            if (section.is_array_section_size())
            {
                upper_bound 
                    << "((" << section.array_section_lower() << ") + (" << section.array_section_upper() << ") - 1)"
                    ;
            }
            else if (section.is_array_section_range())
            {
                upper_bound 
                    << section.array_section_upper()
                    ;
            }
            
            AST_t upper_bound_tree = upper_bound.parse_expression(section.get_ast(),
                    section.get_scope_link());
            rebuilt_type = rebuilt_type.get_array_to(section.array_section_lower().get_ast(), upper_bound_tree, section.get_scope());

            arr_addr << "[" << section.array_section_lower() << "]"
                ;
        }

        addr = arr_addr;
        type = rebuilt_type;
        if (!enclosing_is_array)
        {
            addr = "&(" + addr.get_source() + ")";
        }
        size = safe_expression_size(type, expr.get_scope());
        return true;
    }
    else if (expr.is_unary_operation())
    {
        if (expr.get_operation_kind() == Expression::REFERENCE)
        {
            Expression ref_expr = expr.get_unary_operand();
            if (ref_expr.is_unary_operation()
                    && ref_expr.get_operation_kind() == Expression::DERREFERENCE)
            {
                // Case &(*a)
                return gather_info_data_expr_rec(ref_expr.get_unary_operand(),
                        base_sym,
                        size, 
                        addr, 
                        type,
                        enclosing_is_array, 
                        pointer_member_access,
                        warnlog);
            }
            else if (ref_expr.is_array_subscript())
            {
                // Case &(a[0])
                return gather_info_data_expr_rec(ref_expr.get_subscripted_expression(),
                        base_sym,
                        size,
                        addr,
                        type,
                        enclosing_is_array, 
                        pointer_member_access,
                        warnlog);
            }
            else if (ref_expr.is_array_section_range()
                    || ref_expr.is_array_section_size())
            {
                // Case &(a[0:4])  
                // Case &(a[0;5])
                return gather_info_data_expr_rec(ref_expr.array_section_item(),
                        base_sym,
                        size,
                        addr,
                        type,
                        enclosing_is_array,
                        pointer_member_access,
                        warnlog);
            }
        }
        else if (expr.get_operation_kind() == Expression::DERREFERENCE)
        {
            Expression ref_expr = expr.get_unary_operand();
            if (ref_expr.is_unary_operation()
                    && ref_expr.get_operation_kind() == Expression::REFERENCE)
            {
                // Case *(&a)
                return gather_info_data_expr_rec(ref_expr.get_unary_operand(),
                        base_sym,
                        size, 
                        addr, 
                        type,
                        enclosing_is_array,
                        pointer_member_access,
                        warnlog);
            }
            else
            {
                // Case *a
                Source ptr_size, ptr_addr;
                bool b = gather_info_data_expr_rec(ref_expr,
                        base_sym,
                        ptr_size, 
                        ptr_addr, 
                        type,
                        enclosing_is_array,
                        pointer_member_access,
                        warnlog);

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

                size = safe_expression_size(type, expr.get_scope());
                addr = "(" + ref_expr.prettyprint() + ")";

                return true;
            }
        }

        warnlog << expr.get_ast().get_locus() 
            << ": warning: expression '" 
            << expr.prettyprint() 
            << "' is not a valid data-reference" << std::endl;
        return false;
    }
    else if (expr.is_shaping_expression())
    {
        Expression shaped_expr = expr.shaped_expression();

        Source arr_size, arr_addr;

        bool b = gather_info_data_expr_rec(shaped_expr, base_sym, 
                arr_size, arr_addr, 
                type, /* enclosing_is_array */ true,
                pointer_member_access, warnlog);

        if (!b)
            return false;

        CXX_LANGUAGE()
        {
            if (type.is_reference())
            {
                type = type.references_to();
            }
        }

        // Array to pointer
        if (type.is_array())
        {
            type = type.array_element().get_pointer_to();
        }

        if (!type.is_pointer())
        {
            warnlog << expr.get_ast().get_locus() 
                << ": warning: in data reference '" 
                << expr.prettyprint() 
                << "' shaped expression does not have pointer type" << std::endl;
            return false;
        }

        type = type.points_to();

        ObjectList<Expression> shape_list = expr.shape_list();
        for (ObjectList<Expression>::reverse_iterator it = shape_list.rbegin();
                it != shape_list.rend();
                it++)
        {
            type = type.get_array_to(it->get_ast(), it->get_scope());
        }
        size = safe_expression_size(type, expr.get_scope());

        Type cast_type = type.array_element().get_pointer_to();

        addr << "((" << cast_type.get_declaration(expr.get_scope(), "") << ")" << arr_addr << ")";

        return true;
    }
    else if (expr.is_member_access())
    {
        Expression obj_expr = expr.get_accessed_entity();

        Source obj_addr, obj_size;

        bool b = gather_info_data_expr_rec(obj_expr, base_sym, 
                obj_size, obj_addr, 
                type,
                /* enclosing_is_array */ false, pointer_member_access, warnlog);

        if (!b)
            return false;

        Symbol member = expr.get_accessed_member().get_computed_symbol();
        if (!member.is_valid()
                || !member.is_variable())
        {
            warnlog << expr.get_ast().get_locus() 
                << ": warning: in data reference '" 
                << expr.prettyprint() 
                << "' member is invalid" << std::endl;
            return false;
        }

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
        size = safe_expression_size(type, expr.get_scope());

        return true;
    }
    else if (expr.is_pointer_member_access())
    {
        Expression obj_expr = expr.get_accessed_entity();
        Source obj_addr, obj_size;

        bool b = gather_info_data_expr_rec(obj_expr, base_sym, 
                size, obj_addr, type,
               /* enclosing_is_array */ false, 
               pointer_member_access, warnlog);
 
        if (!b) return false;
        

        Symbol sym = expr.get_accessed_entity().get_symbol();
        
        //The access is not a this or __tmp_this access
        if (!(expr.is_this_access()
                    || (sym.is_valid() 
                        && sym.has_attribute("IS_TMP_THIS"))))
        {
            if(pointer_member_access) 
            {
                warnlog << expr.get_ast().get_locus() 
                        << ": warning: data reference '" 
                        << expr.prettyprint() 
                        << "' is not supported" << std::endl;
                return false;
            }
            //Only supports one pointer member access (and the this)
            pointer_member_access = true;
        }
        else 
        {
            //This->x or __tmp_this->x: the base_sym should be x 
            sym = expr.get_accessed_member().get_computed_symbol();
            if(sym.is_valid()) 
            {
                base_sym = sym;
            }
        }

        type = base_sym.get_type().get_reference_to();
        addr << "&" << expr.prettyprint();
        return true;
    }

    warnlog << expr.get_ast().get_locus() 
        << ": warning: data reference '" 
        << expr.prettyprint() 
        << "' is invalid" << std::endl;
    return false;
}

bool DataReference::gather_info_data_expr(Expression &expr, Symbol& base_sym,
        Source &size, Source &addr, 
        Type &type,
        std::stringstream& warnlog)
{
    bool pointer_member_access = false;
    return gather_info_data_expr_rec(expr, base_sym, size, addr, type, 
        /* enclosing_is_array */ false, pointer_member_access, warnlog);
}

// This function constructs a sizeof but avoids variable length arrays which are not valid in C++
Source TL::DataReference::safe_expression_size(Type type, Scope sc)
{
    Source result;
    if (type.is_reference())
        type = type.references_to();

    if (type.is_array())
    {
        AST_t size = type.array_get_size();

        if ((Expression(size, ScopeLink()).is_constant()
                || IS_C_LANGUAGE)
                && !type.basic_type().is_void())
        {
            result << "sizeof(" << type.get_declaration(sc, "") << ")"
                ;
        }
        else
        {
            result
                << "((" << size.prettyprint() << ") * " << safe_expression_size(type.array_element(), sc) << ")";
        }
    }
    // Simplify pointers to arrays
    else if (type.is_pointer()
            && type.points_to().is_array())
    {
        result << safe_expression_size(type.points_to(), sc)
            ;           // .array_element().get_pointer_to()
    }
    else
    {
        result << "sizeof(" << type.get_declaration(sc, "") << ")"
            ;
    }

    return result;
}


TL::DataReference& TL::DataReference::operator=(const DataReference& data_ref)
{
    if (this != &data_ref)
    {
        this->Expression::operator=(data_ref);
        this->_valid = data_ref._valid;
        this->_base_symbol = data_ref._base_symbol;
        this->_type = data_ref._type;
        this->_size = data_ref._size;
        this->_addr = data_ref._addr;
        this->_warnlog << data_ref._warnlog.str();
    }
    return *this;
}

TL::DataReference::DataReference(const DataReference& data_ref)
    : Expression(data_ref),
    _valid(data_ref._valid),
    _base_symbol(data_ref._base_symbol),
    _type(data_ref._type),
    _size(data_ref._size),
    _addr(data_ref._addr),
    _warnlog()
{
    this->_warnlog << data_ref._warnlog.str();
}
