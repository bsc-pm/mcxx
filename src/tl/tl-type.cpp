/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
        return get_declaration_string_internal(_type_info, sc._decl_context, symbol_name.c_str(), 
                initializer.c_str(), 0, NULL, NULL, flags == PARAMETER_DECLARATION);
    }

    std::string Type::get_declaration_with_parameters(Scope sc,
            const std::string& symbol_name, ObjectList<std::string>& parameters,
            TypeDeclFlags flags) const
    {
        char** parameter_names = NULL;
        int num_parameters = 0;
        char* result = get_declaration_string_internal(_type_info, sc._decl_context, symbol_name.c_str(), 
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
        return get_declaration_string_internal(_type_info, sc._decl_context,
                symbol_name.c_str(), "", 0, NULL, NULL, flags == PARAMETER_DECLARATION);
    }

    std::string Type::get_declaration(Scope sc, const std::string& symbol_name,
            TypeDeclFlags flags) const
    {
        return get_declaration_string_internal(_type_info, sc._decl_context,
                symbol_name.c_str(), "", 0, NULL, NULL, flags == PARAMETER_DECLARATION);
    }

    Type Type::get_pointer_to()
    {
        type_t* work_type = this->_type_info;

        if (is_reference())
        {
            // We cannot get a pointer to a reference, get the referenced
            // type and make it pointer
            work_type = reference_type_get_referenced_type(work_type);
        }

        type_t* result_type = get_pointer_type(work_type);

        return result_type;
    }

    Type Type::get_array_to(AST_t array_expr, Scope)
    {
        type_t* result_type = this->_type_info;

        decl_context_t decl_context;
        type_t* array_to = get_array_type(result_type, array_expr._ast, decl_context);

        return Type(array_to);
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
        // type_t* type_info = advance_over_typedefs(_type_info);
        // type_t* _base_type = base_type(type_info);
        // return (is_dependent_type(type_info, _base_type->type->typeof_decl_context));

        // FIXME
        return 0;
    }

    Type Type::returns() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return function_type_get_return_type(type_info);
    }

    ObjectList<Type> Type::parameters() const
    {
        bool b;
        return parameters(b);
    }

    ObjectList<Type> Type::parameters(bool& has_ellipsis) const
    {
        type_t* type_info = advance_over_typedefs(_type_info);

        has_ellipsis = function_type_get_has_ellipsis(type_info);

        ObjectList<Type> result;
        for (int i = 0; i < function_type_get_num_parameters(type_info); i++)
        {
            // The last one is the ellipsis and lacks type
            if (has_ellipsis
                    && ((i + 1) == function_type_get_num_parameters(type_info)))
            {
                break;
            }
            Type t(function_type_get_parameter_type_num(type_info, i));
            result.push_back(t);
        }

        return result;
    }

    Type Type::points_to() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return pointer_type_get_pointee_type(type_info);
    }

    Type Type::array_element() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return array_type_get_element_type(type_info);
    }

    Type Type::references_to() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return reference_type_get_referenced_type(type_info);
    }

    bool Type::is_non_derived_type() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return ::is_non_derived_type(type_info);
    }

    bool Type::is_direct_type() const
    {
        return this->is_non_derived_type();
    }

    bool Type::is_void() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_void_type(type_info);
    }

    bool Type::is_enum() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return (is_enumerated_type(type_info));
    }

    bool Type::is_class() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return (is_named_class_type(type_info));
    }

    bool Type::explicit_array_dimension() const
    {
        if (is_array())
        {
            type_t* type_info = advance_over_typedefs(_type_info);
            return (array_type_get_array_size_expr(type_info));
        }

        return false;
    }

    AST_t Type::array_dimension() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        AST expression = array_type_get_array_size_expr(type_info);
        return expression;
    }


    Type Type::get_int_type(void)
    {
        return Type(get_signed_int_type());
    }

    Type Type::original_type(void) const
    {
        // if (_type_info->original_type != NULL)
        // {
        //     return Type(_type_info->original_type);
        // }
        // else
        // FIXME
        {
            return *this;
        }
    }


    bool Type::is_integral_type() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return ::is_integral_type(type_info);
    }

    bool Type::is_signed_int() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_signed_int_type(type_info);
    }

    bool Type::is_unsigned_int() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_unsigned_int_type(type_info);
    }

    bool Type::is_signed_short_int() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_signed_short_int_type(type_info);
    }

    bool Type::is_unsigned_short_int() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_unsigned_short_int_type(type_info);
    }

    bool Type::is_signed_long_int() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_signed_long_int_type(type_info);
    }

    bool Type::is_unsigned_long_int() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_unsigned_long_int_type(type_info);
    }

    bool Type::is_signed_long_long_int() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_signed_long_long_int_type(type_info);
    }

    bool Type::is_unsigned_long_long_int() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_unsigned_long_long_int_type(type_info);
    }


    bool Type::is_char() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_char_type(type_info);
    }

    bool Type::is_signed_char() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_signed_char_type(type_info);
    }

    bool Type::is_unsigned_char() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_unsigned_char_type(type_info);
    }

    bool Type::is_wchar_t() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_wchar_t_type(type_info);
    }


    bool Type::is_floating_type() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return ::is_floating_type(type_info);
    }

    bool Type::is_long_double() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_long_double_type(type_info);
    }

    bool Type::is_double() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_double_type(type_info);
    }

    bool Type::is_float() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_float_type(type_info);
    }


    bool Type::is_bool() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_bool_type(type_info);
    }

    bool Type::is_pointer_to_member() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_pointer_to_member_type(type_info);
    }

    Type Type::pointed_class() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return pointer_to_member_type_get_class_type(type_info);
    }

    bool Type::is_complex() const
    {
        type_t* type_info = advance_over_typedefs(_type_info);
        return is_complex_type(type_info);
    }

    Type Type::get_reference_to()
    {
        type_t* type_info = get_reference_type(_type_info);
        return Type(type_info);
    }

    Type Type::get_unqualified_type()
    {
        // Might return itself if not qualified
        return get_cv_qualified_type(this->_type_info, CV_NONE);
    }

    Type Type::get_const_type()
    {
        // Might return itself if already const qualified
        return get_cv_qualified_type(this->_type_info, CV_CONST);
    }

    Type Type::get_volatile_type()
    {
        // Might return itself if already volatile qualified
        return get_cv_qualified_type(_type_info, CV_VOLATILE);
    }

    Type Type::get_restrict_type()
    {
        // Might return itself if already restrict qualified
        return get_cv_qualified_type(_type_info, CV_RESTRICT);
    }
}
