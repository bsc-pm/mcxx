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

    Type Type::get_vector_to(unsigned int vector_size)
    {
        type_t* work_type = this->_type_info;

        type_t* result_type = get_vector_type(work_type, vector_size);

        return result_type;
    }

    Type Type::get_generic_vector_to()
    {
        type_t* work_type = this->_type_info;

        type_t* result_type = get_generic_vector_type(work_type);

        return result_type;
    }

    Type Type::get_array_to(AST_t array_expr, Scope sc)
    {
        type_t* result_type = this->_type_info;

        decl_context_t decl_context = sc.get_decl_context();
        type_t* array_to = get_array_type(result_type, array_expr._ast, decl_context);

        return Type(array_to);
    }

    Type Type::get_array_to(AST_t lower_bound, AST_t upper_bound, Scope sc)
    {
        type_t* result_type = this->_type_info;

        decl_context_t decl_context = sc.get_decl_context();
        type_t* array_to = get_array_type_bounds(result_type, lower_bound._ast, upper_bound._ast, decl_context);

        return Type(array_to);
    }

    Type Type::get_array_to()
    {
        type_t* result_type = this->_type_info;

        decl_context_t null_decl_context;
        memset(&null_decl_context, 0, sizeof(null_decl_context));
        type_t* array_to = get_array_type(result_type, NULL, null_decl_context);

        return Type(array_to);
    }

    Type Type::get_array_to(const std::string& str)
    {
        type_t* result_type = this->_type_info;

        type_t* array_to = get_array_type_str(result_type, uniquestr(str.c_str()));

        return Type(array_to);
    }

    Type Type::get_function_returning(const ObjectList<Type>& type_list, bool has_ellipsis)
    {
        int i;
        parameter_info_t *parameters_list;
        int num_parameters = type_list.size();
   
        parameters_list = (parameter_info_t *) malloc ((num_parameters+has_ellipsis) * sizeof(parameter_info_t));

        for (i=0; i<num_parameters; i++)
        {
            parameters_list[i].is_ellipsis = 0;
            parameters_list[i].type_info = type_list[i]._type_info;
            parameters_list[i].nonadjusted_type_info = NULL;
        }

        if(has_ellipsis)
        {
            num_parameters++;
            parameters_list[i].is_ellipsis = 1;
            parameters_list[i].type_info = NULL;
            parameters_list[i].nonadjusted_type_info = NULL;
        }

        return (Type(get_new_function_type(_type_info, parameters_list, num_parameters)));
    }

    bool Type::is_error_type() const
    {
        return ::is_error_type(_type_info);
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

    bool Type::is_vector() const
    {
        return (is_vector_type(_type_info));
    }

    bool Type::is_generic_vector() const
    {
        return (is_generic_vector_type(_type_info));
    }

    Type Type::vector_element() const
    {
        return vector_type_get_element_type(_type_info);
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
        return ::is_dependent_type(_type_info);
    }

    bool Type::is_expression_dependent() const
    {
        return ::is_dependent_expr_type(_type_info);
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

    ObjectList<Type> Type::nonadjusted_parameters() const
    {
        bool b;
        return nonadjusted_parameters(b);
    }

    ObjectList<Type> Type::nonadjusted_parameters(bool& has_ellipsis) const
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
            Type t(function_type_get_nonadjusted_parameter_type_num(_type_info, i));
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

    bool Type::array_is_vla() const
    {
        return array_type_is_vla(_type_info);
    }

    Type Type::references_to() const
    {
        return reference_type_get_referenced_type(_type_info);
    }

    bool Type::is_non_derived_type() const
    {
        return this->is_scalar_type();
    }

    bool Type::is_scalar_type() const
    {
        return ::is_scalar_type(_type_info);
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
        return (::is_enum_type(_type_info));
    }

    bool Type::is_unnamed_enum() const
    {
        return (::is_enum_type(_type_info)
                && !is_named_type(_type_info));
    }

    bool Type::is_named_enum() const
    {
        return (::is_enum_type(_type_info)
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
        return array_has_size();
    }

    AST_t Type::array_dimension() const
    {
        return array_get_size();
    }

    bool Type::array_has_size() const
    {
        if (is_array())
        {
            return (array_type_get_array_size_expr(_type_info));
        }

        return false;
    }

    AST_t Type::array_get_size() const
    {
        AST expression = array_type_get_array_size_expr(_type_info);
        return expression;
    }

    void Type::array_get_bounds(AST_t& lower, AST_t& upper)
    {
        lower = AST_t(array_type_get_array_lower_bound(_type_info));
        upper = AST_t(array_type_get_array_upper_bound(_type_info));
    }

    Type Type::get_void_type(void)
    {
        return Type(::get_void_type());
    }
    
    Type Type::get_char_type(void)
    {
        return Type(::get_char_type());
    }

    Type Type::get_unsigned_char_type(void)
    {
        return Type(::get_unsigned_char_type());
    }

    Type Type::get_short_int_type(void)
    {
        return Type(::get_signed_short_int_type());
    }

    Type Type::get_unsigned_short_int_type(void)
    {
        return Type(::get_unsigned_short_int_type());
    }

    Type Type::get_int_type(void)
    {
        return Type(::get_signed_int_type());
    }

    Type Type::get_unsigned_int_type(void)
    {
        return Type(::get_unsigned_int_type());
    }

    Type Type::get_long_int_type(void)
    {
        return Type(::get_signed_long_int_type());
    }

    Type Type::get_unsigned_long_int_type(void)
    {
        return Type(::get_unsigned_long_int_type());
    }

    Type Type::get_long_long_int_type(void)
    {
        return Type(::get_signed_long_long_int_type());
    }

    Type Type::get_unsigned_long_long_int_type(void)
    {
        return Type(::get_unsigned_long_long_int_type());
    }

    Type Type::get_float_type(void)
    {
        return Type(::get_float_type());
    }

    Type Type::get_double_type(void)
    {
        return Type(::get_double_type());
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
        return get_cv_qualified_type(this->_type_info, 
                (cv_qualifier_t)(get_cv_qualifier(this->_type_info) | CV_CONST));
    }

    Type Type::get_volatile_type()
    {
        // Might return itself if already volatile qualified
        return get_cv_qualified_type(this->_type_info, 
                (cv_qualifier_t)(get_cv_qualifier(this->_type_info) | CV_VOLATILE));
    }

    Type Type::get_restrict_type()
    {
        // Might return itself if already restrict qualified
        return get_cv_qualified_type(this->_type_info, 
                (cv_qualifier_t)(get_cv_qualifier(this->_type_info) | CV_RESTRICT));
    }

    int Type::get_alignment_of()
    {
        return type_get_alignment(this->get_internal_type());
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
        unsigned int n = class_type_get_num_nonstatic_data_members(::get_actual_class_type(_type_info));

        for (unsigned int i = 0; i < n; i++)
        {
            result.push_back(class_type_get_nonstatic_data_member_num(::get_actual_class_type(_type_info), i));
        }

        return result;
    }

    ObjectList<Symbol> Type::get_static_data_members() const
    {
        ObjectList<Symbol> result;
        unsigned int n = class_type_get_num_static_data_members(::get_actual_class_type(_type_info));

        for (unsigned int i = 0; i < n; i++)
        {
            result.push_back(class_type_get_static_data_member_num(::get_actual_class_type(_type_info), i));
        }

        return result;
    }

    ObjectList<Symbol> Type::get_all_data_members() const
    {
        ObjectList<Symbol> result = get_nonstatic_data_members();
        result.append(get_static_data_members());
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

    bool Type::is_template_type() const
    {
        return (::is_template_type(_type_info));
    }

    Type Type::get_primary_template() const
    {
        return ::template_type_get_primary_type(_type_info);
    }

    bool Type::is_template_specialized_type() const
    {
        return (::is_template_specialized_type(_type_info));
    }

    ObjectList<TemplateParameter> Type::get_template_parameters() const
    {
        ObjectList<TemplateParameter> result;
        template_parameter_list_t* template_parameters = NULL;

        if (is_template_type())
        {
            template_parameters = template_type_get_template_parameters(_type_info);
        }
        else if (is_template_specialized_type())
        {
            template_parameters = template_specialized_type_get_template_parameters(_type_info);
        }

        int i;
        for (i = 0; i < template_parameters->num_template_parameters; i++)
        {
            template_parameter_t* template_parameter = template_parameters->template_parameters[i];

            result.append(template_parameter);
        }

        return result;
    }

    ObjectList<TemplateArgument> Type::get_template_arguments() const
    {
        ObjectList<TemplateArgument> result;
        template_argument_list_t* arg_list = template_specialized_type_get_template_arguments(_type_info);

        for (int i = 0; i < arg_list->num_arguments; i++)
        {
            result.append(TemplateArgument(arg_list->argument_list[i]));
        }

        return result;
    }

    Type Type::get_related_template_type() const
    {
        return Type(::template_specialized_type_get_related_template_type(_type_info));
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

    Type Type::basic_type() const
    {
        if (this->is_array())
        {
            return this->array_element().basic_type();
        }
        else if (this->is_pointer()
                || this->is_pointer_to_member())
        {
            return this->points_to().basic_type();
        }
        else if (this->is_function())
        {
            return this->returns().basic_type();
        }
        else if (this->is_reference())
        {
            return this->references_to().basic_type();
        }
        else if (this->is_vector())
        {
            return this->vector_element().basic_type();
        }
        else
        {
            return *this;
        }
    }

    Type Type::get_canonical_type()
    {
         return ::canonical_type(this->_type_info);
    }

    Type Type::get_enum_underlying_type()
    {
        return ::enum_type_get_underlying_type(this->_type_info);
    }

    bool Type::is_incomplete() const
    {
        return is_incomplete_type(_type_info);
    }

    unsigned int Type::get_size() 
    {
        unsigned int result;

        if (is_generic_vector_type(_type_info))
        {
            result = this->basic_type().get_size(); 
        }
        else
        {
            result = (unsigned int) type_get_size(_type_info);
        }
        return result;
    }

    Type Type::advance_over_typedefs()
    {
        return Type(::advance_over_typedefs(_type_info));
    }

    Type Type::advance_over_typedefs_cv()
    {
        cv_qualifier_t cv = CV_NONE;
        type_t* type = ::advance_over_typedefs_with_cv_qualif(_type_info, &cv);

        return Type(get_cv_qualified_type(type, cv));
    }

    ObjectList<Symbol> Type::get_bases_class_symbol_list()
    {
        ObjectList<Symbol> base_symbol_list;

        scope_entry_list_t* all_bases = class_type_get_all_bases(_type_info, 0);
        scope_entry_list_t* it = all_bases;

        Scope::convert_to_vector(it, base_symbol_list);

        return base_symbol_list;
    }

    bool Type::is_pod()
    {
        return ::is_pod_type(_type_info);
    }

    bool Type::is_unresolved_overload()
    {
        return ::is_unresolved_overloaded_type(_type_info);
    }

    ObjectList<Symbol> Type::get_unresolved_overload_set()
    {
        ObjectList<Symbol> result;
        scope_entry_list_t* entry_list = ::unresolved_overloaded_type_get_overload_set(_type_info);

        Scope::convert_to_vector(entry_list, result);

        return result;
    }

    //! Returns all the builtins of the type system
    ObjectList<Type> Type::get_integer_types()
    {
        Type all_integer_types[] =
        {
            Type(get_char_type()),
            Type(get_signed_int_type()),
            Type(get_signed_short_int_type()),
            Type(get_signed_long_int_type()),
            Type(get_signed_long_long_int_type()),
            Type(get_signed_char_type()),
            Type(get_unsigned_int_type()),
            Type(get_unsigned_short_int_type()),
            Type(get_unsigned_long_int_type()),
            Type(get_unsigned_long_long_int_type()),
            Type(get_unsigned_char_type()),
        };

        return ObjectList<Type>(all_integer_types);
    }

    ObjectList<Type> Type::get_floating_types()
    {
        Type all_floating_types[] =
        {
            Type(get_float_type()),
            Type(get_double_type()),
            Type(get_long_double_type()),
        };

        return ObjectList<Type>(all_floating_types);
    }

    ObjectList<Type> Type::get_arithmetic_types()
    {
        ObjectList<Type> res(Type::get_integer_types());
        res.append(Type::get_floating_types());
        return res;
        //return Type::get_integer_types().append(Type::get_floating_types());
    }

    bool Type::is_variably_modified() const
    {
        return ::is_variably_modified_type(_type_info);
    }
}
