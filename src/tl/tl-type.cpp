/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "tl-type.hpp"
#include "tl-ast.hpp"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-scope.h"
#include "cxx-exprtype.h"

namespace TL
{
    std::string Type::get_declaration_with_initializer(Scope sc, const std::string& symbol_name,
            const std::string& initializer, TypeDeclFlags flags) const
    {
        return get_declaration_string_internal(_type_info, sc._st, symbol_name.c_str(), 
                initializer.c_str(), 0, NULL, NULL, flags == PARAMETER_DECLARATION);
    }

    std::string Type::get_declaration_with_parameters(Scope sc,
            const std::string& symbol_name, ObjectList<std::string>& parameters,
            TypeDeclFlags flags) const
    {
        char** parameter_names = NULL;
        int num_parameters = 0;
        char* result = get_declaration_string_internal(_type_info, sc._st, symbol_name.c_str(), 
                "", 0, &num_parameters, &parameter_names, flags == PARAMETER_DECLARATION);

        for (int i = 0; i < num_parameters; i++)
        {
            parameters.push_back(std::string(parameter_names[i]));
        }

        return result;
    }

    std::string Type::get_simple_declaration(Scope sc, const std::string&
            symbol_name, TypeDeclFlags flags) const
    {
        return get_declaration_string_internal(_type_info, sc._st,
                symbol_name.c_str(), "", 0, NULL, NULL, flags == PARAMETER_DECLARATION);
    }

    std::string Type::get_declaration(Scope sc, const std::string& symbol_name,
            TypeDeclFlags flags) const
    {
        return get_declaration_string_internal(_type_info, sc._st,
                symbol_name.c_str(), "", 0, NULL, NULL, flags == PARAMETER_DECLARATION);
    }

    Type Type::duplicate()
    {
        type_t* new_type_info = copy_type(_type_info);
        return Type(new_type_info);
    }

    Type Type::get_pointer_to()
    {
        Type result = this->duplicate();
        type_t* result_type = result._type_info;

        if (result_type->kind == TK_REFERENCE)
        {
            // We cannot get a pointer to a reference, remove the reference and
            // convert it into a pointer
            result_type->kind = TK_POINTER;
        }
        else
        {
            type_t* pointer_to = (type_t*)calloc(1, sizeof(*pointer_to));

            pointer_to->kind = TK_POINTER;
            pointer_to->pointer = (pointer_info_t*)calloc(1, sizeof(*(pointer_to->pointer)));
            pointer_to->pointer->pointee = result_type;

            result._type_info = pointer_to;
        }

        return result;
    }

    Type Type::get_array_to(AST_t array_expr, Scope scope)
    {
        Type result = this->duplicate();
        type_t* result_type = result._type_info;

        type_t* array_to = (type_t*)calloc(1, sizeof(*array_to));

        array_to->kind = TK_ARRAY;
        array_to->array = (array_info_t*)calloc(1, sizeof(*(array_to->array)));
        array_to->array->element_type = result_type;
        array_to->array->array_expr = array_expr._ast;
        array_to->array->array_expr_scope = scope._st;

        result._type_info = array_to;

        return result;
    }

    bool Type::operator==(Type t) const
    {
        return this->_type_info == t._type_info;
    }

    bool Type::operator!=(Type t) const
    {
        return !(this->operator==(t));
    }

    bool Type::operator<(Type t) const
    {
        return this->_type_info < t._type_info;
    }

    Type& Type::operator=(Type t)
    {
        this->_type_info = t._type_info;
        return (*this);
    }

    bool Type::is_builtin_type() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return (is_fundamental_type(type_info));
    }

    Type::BuiltinType Type::builtin_type(TypeModifier& type_modif) const
    {
        if (is_builtin_type())
        {
            type_t* type_info = advance_over_typedefs(_type_info);

            if ((type_info->cv_qualifier & CV_CONST) == CV_CONST)
            {
                type_modif |= TypeModifier::CONST;
            }

            if ((type_info->cv_qualifier & CV_VOLATILE) == CV_VOLATILE)
            {
                type_modif |= TypeModifier::VOLATILE;
            }

            if ((type_info->cv_qualifier & CV_RESTRICT) == CV_RESTRICT)
            {
                type_modif |= TypeModifier::RESTRICT;
            }

            if (type_info->type->is_short)
            {
                type_modif |= TypeModifier::SHORT;
            }
            else if (type_info->type->is_long)
            {
                if (type_info->type->is_long == 1)
                {
                    type_modif |= TypeModifier::LONG;
                }
                else
                {
                    type_modif |= TypeModifier::LONG_LONG;
                }
            }

            if (type_info->type->is_unsigned)
            {
                type_modif |= TypeModifier::UNSIGNED;
            }

            switch ((int)(type_info->type->builtin_type))
            {
                case BT_INT:
                    return BuiltinType::INT;
                case BT_BOOL:
                    return BuiltinType::BOOL;
                case BT_FLOAT:
                    return BuiltinType::FLOAT;
                case BT_DOUBLE:
                    return BuiltinType::DOUBLE;
                case BT_CHAR:
                    return BuiltinType::CHAR;
                case BT_WCHAR:
                    return BuiltinType::WCHAR;
                case BT_VOID:
                    return BuiltinType::VOID;
                default:
                    return BuiltinType::UNKNOWN;
            }
        }

        return BuiltinType::UNKNOWN;
    }

    Type::BuiltinType Type::builtin_type() const
    {
        TypeModifier type_modif;
        return builtin_type(type_modif);
    }

    bool Type::is_pointer() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return (is_pointer_type(type_info));
    }

    bool Type::is_array() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return (is_array_type(type_info));
    }

    bool Type::is_reference() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return (is_reference_type(type_info));
    }

    bool Type::is_function() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_function_type(type_info);
    }

    bool Type::is_dependent() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return (is_dependent_type(type_info, default_decl_context));
    }

    Type Type::returns() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return function_return_type(type_info);
    }

    ObjectList<Type> Type::parameters() const
    {
        bool b;
        return parameters(b);
    }

    ObjectList<Type> Type::parameters(bool& has_ellipsis) const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        type_t** parameter_list;
        int num_params = 0;
        char ellipsis = 0;

        parameter_list = function_parameter_types(type_info, &num_params, &ellipsis);
        has_ellipsis = ellipsis;

        ObjectList<Type> result;
        for (int i = 0; i < num_params; i++)
        {
            Type t(parameter_list[i]);
            result.push_back(t);
        }

        return result;
    }

    Type Type::points_to() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return pointer_pointee_type(type_info);
    }

    Type Type::array_element() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return array_element_type(type_info);
    }

    Type Type::references_to() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return reference_referenced_type(type_info);
    }

    bool Type::is_direct_type() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return ::is_direct_type(type_info);
    }

    bool Type::is_void() const
    {
        return (is_builtin_type() &&
                builtin_type() == BuiltinType::VOID);
    }

    bool Type::is_enum() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return (this->is_direct_type()
                && is_enumerated_type(type_info));
    }

    bool Type::is_class() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return (this->is_direct_type()
                && is_named_class_type(type_info));
    }

    bool Type::explicit_array_dimension() const
    {
        if (is_array())
        {
            type_t* type_info = advance_over_typedefs(_type_info);
            return (type_info->array->array_expr != NULL);
        }

        return false;
    }

    AST_t Type::array_dimension() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        AST expression = type_info->array->array_expr;
        return expression;
    }


    Type Type::get_int_type(void)
    {
        return Type(integer_type());
    }
}
