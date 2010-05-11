#include "tl-datareference.hpp"
#include "tl-symbol.hpp"

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
    if (expr.is_id_expression())
    {
        Symbol sym = expr.get_id_expression().get_computed_symbol();

        if (!sym.is_valid())
            return false;

        if (!sym.is_variable())
            return false;

        base_sym = sym;

        Type t = sym.get_type();

        if (t.is_reference())
            t = t.references_to();

        type = t;

        if (t.is_array())
        {
            addr << sym.get_qualified_name();

            Source dim_size;

            if (!enclosing_is_array)
            {
                size << dim_size << "sizeof(" << t.get_declaration(expr.get_scope(), "") << ")";
            }
            else
            {
                t = t.basic_type();
                size << "sizeof(" << t.get_declaration(expr.get_scope(), "") << ")";
            }
        }
        else if (t.is_pointer() && enclosing_is_array)
        {
            addr << sym.get_qualified_name();
            t = t.points_to();
            size << "sizeof(" << t.get_declaration(expr.get_scope(), "") << ")";

        }
        else 
        {
            addr << "&" << sym.get_qualified_name();
            size << "sizeof(" << t.get_declaration(expr.get_scope(), "") << ")";
        }

        return true;
    }
    else if (expr.is_array_subscript())
    {
        Source arr_size, arr_addr;
        bool b = gather_info_data_expr_rec(expr.get_subscripted_expression(),
                base_sym,
                arr_size,
                arr_addr,
                type,
                /* enclosing_is_array */ true);
        if (!b)
            return 0;

        size << arr_size << " * (" << expr.get_subscript_expression() << ")";

        if (!enclosing_is_array)
        {
            addr << "&" << arr_addr << "[" << expr.get_subscript_expression() << "]";
        }
        else
        {
            addr << arr_addr << "[" << expr.get_subscript_expression() << "]";
        }

        if (type.is_array())
        {
            type = type.array_element();
        }
        else if (type.is_pointer())
        {
            type = type.points_to();
        }
        else
        {
            return false;
        }

        return true;
    }
    else if (expr.is_array_section())
    {
        Source arr_size, arr_addr;
        bool b = gather_info_data_expr_rec(expr.array_section_item(),
                base_sym,
                arr_size,
                arr_addr,
                type,
                /* enclosing_is_array */ true);
        if (!b)
            return 0;

        size << arr_size << "* ( (" << expr.array_section_upper() << ")-(" << expr.array_section_lower() << ") + 1 )";

        if (!enclosing_is_array)
        {
            addr << "&" << arr_addr << "[" << expr.array_section_lower() << "]";
        }
        else
        {
            addr << arr_addr << "[" << expr.array_section_lower() << "]";
        }

        if (type.is_pointer())
        {
            type = type.points_to();
        }
        else if (type.is_array())
        {
            type = type.array_element();
        }
        else
        {
            return false;
        }

        type = type.get_array_to("(" 
                + expr.array_section_upper().prettyprint() 
                + ")-(" 
                + expr.array_section_lower().prettyprint() + ") + 1");

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
            else if (ref_expr.is_array_section())
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

                size << "sizeof(" << type.get_declaration(ref_expr.get_scope(), "") << ")";
                addr << "(" << ref_expr << ")";

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

        ObjectList<Expression> shape_list = expr.shape_list();

        Source factor;
        for (ObjectList<Expression>::iterator it = shape_list.begin();
                it != shape_list.end();
                it++)
        {
            factor.append_with_separator("(" + it->prettyprint() + ")", "*");
        }

        size << "(" << arr_size << ") * " << factor;

        addr << arr_addr;

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

        Type t = obj_expr.get_type();

        if (t.is_reference())
            t = t.references_to();

        type = t;

        if (!enclosing_is_array)
        {
            size << "sizeof(" << t.get_declaration(expr.get_scope(), "") << ")";
            addr << "&" << obj_addr << "." << expr.get_accessed_member();
        }
        else
        {
            t = t.basic_type();
            size << "sizeof(" << t.get_declaration(expr.get_scope(), "") << ")";
            addr << obj_addr << "." << expr.get_accessed_member();
        }

        return true;
    }
    return false;
}

bool DataReference::gather_info_data_expr(Expression &expr, Symbol& base_sym,
        Source &size, Source &addr, Type &type)
{
    return gather_info_data_expr_rec(expr, base_sym, size, addr, type, /* enclosing_is_array */ false);
}
