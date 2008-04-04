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
        const char** parameter_names = NULL;
        int num_parameters = 0;
        const char* result = get_declaration_string_internal(_type_info, sc._decl_context, symbol_name.c_str(), 
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
        return (is_pointer_type(_type_info));
    }

    bool Type::is_array() const
    {
        return (is_array_type(_type_info));
    }

    bool Type::is_reference() const
    {
        return (is_lvalue_reference_type(_type_info));
    }

    bool Type::is_function() const
    {
        return is_function_type(_type_info);
    }

    bool Type::is_dependent() const
    {
        return 0;
    }

    Type Type::returns() const
    {
        return function_type_get_return_type(_type_info);
    }

    ObjectList<Type> Type::parameters() const
    {
        bool b;
        return parameters(b);
    }

    ObjectList<Type> Type::parameters(bool& has_ellipsis) const
    {
        has_ellipsis = function_type_get_has_ellipsis(_type_info);

        ObjectList<Type> result;
        for (int i = 0; i < function_type_get_num_parameters(_type_info); i++)
        {
            // The last one is the ellipsis and lacks type
            if (has_ellipsis
                    && ((i + 1) == function_type_get_num_parameters(_type_info)))
            {
                break;
            }
            Type t(function_type_get_parameter_type_num(_type_info, i));
            result.push_back(t);
        }

        return result;
    }

    Type Type::points_to() const
    {
        return pointer_type_get_pointee_type(_type_info);
    }

    Type Type::array_element() const
    {
        return array_type_get_element_type(_type_info);
    }

    Type Type::references_to() const
    {
        return reference_type_get_referenced_type(_type_info);
    }

    bool Type::is_non_derived_type() const
    {
        return ::is_non_derived_type(_type_info);
    }

    bool Type::is_direct_type() const
    {
        return this->is_non_derived_type();
    }

    bool Type::is_void() const
    {
        return is_void_type(_type_info);
    }

    bool Type::is_enum() const
    {
        return (is_enumerated_type(_type_info));
    }

    bool Type::is_unnamed_enum() const
    {
        return (is_enumerated_type(_type_info)
                && !is_named_type(_type_info));
    }

    bool Type::is_named_enum() const
    {
        return (is_enumerated_type(_type_info)
                && is_named_type(_type_info));
    }

    bool Type::is_named() const
    {
        return is_named_type(_type_info);
    }

    Symbol Type::get_symbol() const
    {
        return named_type_get_symbol(_type_info);
    }

    bool Type::is_class() const
    {
        return (is_class_type(_type_info));
    }

    bool Type::is_named_class() const
    {
        return (is_named_class_type(_type_info));
    }

    bool Type::is_unnamed_class() const
    {
        return (is_unnamed_class_type(_type_info));
    }

    bool Type::explicit_array_dimension() const
    {
        if (is_array())
        {
            return (array_type_get_array_size_expr(_type_info));
        }

        return false;
    }

    AST_t Type::array_dimension() const
    {
        AST expression = array_type_get_array_size_expr(_type_info);
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
        return ::is_integral_type(_type_info);
    }

    bool Type::is_signed_int() const
    {
        return is_signed_int_type(_type_info);
    }

    bool Type::is_unsigned_int() const
    {
        return is_unsigned_int_type(_type_info);
    }

    bool Type::is_signed_short_int() const
    {
        return is_signed_short_int_type(_type_info);
    }

    bool Type::is_unsigned_short_int() const
    {
        return is_unsigned_short_int_type(_type_info);
    }

    bool Type::is_signed_long_int() const
    {
        return is_signed_long_int_type(_type_info);
    }

    bool Type::is_unsigned_long_int() const
    {
        return is_unsigned_long_int_type(_type_info);
    }

    bool Type::is_signed_long_long_int() const
    {
        return is_signed_long_long_int_type(_type_info);
    }

    bool Type::is_unsigned_long_long_int() const
    {
        return is_unsigned_long_long_int_type(_type_info);
    }


    bool Type::is_char() const
    {
        return is_char_type(_type_info);
    }

    bool Type::is_signed_char() const
    {
        return is_signed_char_type(_type_info);
    }

    bool Type::is_unsigned_char() const
    {
        return is_unsigned_char_type(_type_info);
    }

    bool Type::is_wchar_t() const
    {
        return is_wchar_t_type(_type_info);
    }


    bool Type::is_floating_type() const
    {
        return ::is_floating_type(_type_info);
    }

    bool Type::is_long_double() const
    {
        return is_long_double_type(_type_info);
    }

    bool Type::is_double() const
    {
        return is_double_type(_type_info);
    }

    bool Type::is_float() const
    {
        return is_float_type(_type_info);
    }


    bool Type::is_bool() const
    {
        return is_bool_type(_type_info);
    }

    bool Type::is_pointer_to_member() const
    {
        return is_pointer_to_member_type(_type_info);
    }

    Type Type::pointed_class() const
    {
        return pointer_to_member_type_get_class_type(_type_info);
    }

    bool Type::is_complex() const
    {
        return is_complex_type(_type_info);
    }

    Type Type::get_reference_to()
    {
        return get_lvalue_reference_type(this->_type_info);
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

    bool Type::is_const() const
    {
        return is_const_qualified_type(_type_info);
    }

    bool Type::is_volatile() const
    {
        return is_volatile_qualified_type(_type_info);
    }

    bool Type::is_restrict() const
    {
        return is_restrict_qualified_type(_type_info);
    }

    ObjectList<Symbol> Type::get_fields() const
    {
        return get_nonstatic_data_members();
    }

    ObjectList<Symbol> Type::get_nonstatic_data_members() const
    {
        ObjectList<Symbol> result;
        unsigned int n = class_type_get_num_nonstatic_data_members(_type_info);

        for (unsigned int i = 0; i < n; i++)
        {
            result.push_back(class_type_get_nonstatic_data_member_num(_type_info, i));
        }

        return result;
    }

    ObjectList<Symbol> Type::get_static_data_members() const
    {
        ObjectList<Symbol> result;
        unsigned int n = class_type_get_num_static_data_members(_type_info);

        for (unsigned int i = 0; i < n; i++)
        {
            result.push_back(class_type_get_static_data_member_num(_type_info, i));
        }

        return result;
    }

    bool Type::some_member_is_mutable() const
    {
        ObjectList<Symbol> nonstatic_data_members = get_nonstatic_data_members();

        for (ObjectList<Symbol>::iterator it = nonstatic_data_members.begin();
                it != nonstatic_data_members.end();
                it++)
        {
            Symbol &sym(*it);

            if (sym.is_mutable())
            {
                return true;
            }
        }

        return false;
    }

    bool Type::is_typedef() const
    {
        return is_typedef_type(_type_info);
    }

    Type Type::aliased_type() const
    {
        return typedef_type_get_aliased_type(_type_info);
    }

    bool Type::is_template_specialized_type() const
    {
        return (::is_template_specialized_type(_type_info));
    }

    ObjectList<Symbol> Type::get_template_parameters() const
    {
        ObjectList<Symbol> result;
        template_parameter_list_t* template_parameters = template_specialized_type_get_template_parameters(_type_info);

        int i;
        for (i = 0; i < template_parameters->num_template_parameters; i++)
        {
            template_parameter_t* template_parameter = template_parameters->template_parameters[i];

            Symbol sym(template_parameter->entry);
            result.append(sym);
        }

        return result;
    }

    bool Type::is_same_type(Type t)
    {
        return equivalent_types(this->_type_info, t._type_info);
    }

    bool Type::is_same_type(Type t, Scope)
    {
        return is_same_type(t);
    }

    bool Type::lacks_prototype() const
    {
        return function_type_get_lacking_prototype(this->_type_info);
    }
}
