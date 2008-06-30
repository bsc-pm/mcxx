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
#ifndef TL_TYPE_HPP
#define TL_TYPE_HPP

#include <string>
#include "tl-object.hpp"
#include "tl-symbol.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"
#include "cxx-scope.h"

namespace TL
{
    class Scope;
    class Symbol;
    
    //! \addtogroup Wrap
    //! @{
    
    //! This class wraps a type in the compiler type system
    class Type : public Object
    {
        private:
            type_t* _type_info;
            virtual tl_type_t* get_extended_attribute(const std::string&) const
            {
                return NULL;
            }

            static std::string get_type_name_str(type_t* type, const std::string& symbol_name);
            static void get_type_name_str_internal(type_t* type_info, 
                    const std::string &symbol_name, std::string& left, std::string& right);
            static std::string get_cv_qualifier_str(type_t* type_info);
            static std::string get_simple_type_name_str_internal(type_t* simple_type);
            static std::string get_simple_type_name_str(type_t* simple_type);
            static bool declarator_needs_parentheses(type_t* type_info);
            static std::string get_declaration_str_internal(type_t* type_info, 
                    const std::string& symbol_name, const std::string& initializer, bool semicolon);
        public:

            enum TypeDeclFlags
            {
                NORMAL_DECLARATION,
                PARAMETER_DECLARATION
            };

            Type(type_t* type_info)
                : _type_info(type_info)
            {
            }

            Type(const Type& type)
                : Object(type), _type_info(type._type_info)
            {
            }

            //! States whether the type is valid
            bool is_valid() const
            {
                return (_type_info != NULL);
            }

            bool is_faulty() const
            {
                return is_faulty_type(_type_info);
            }

            virtual ~Type()
            {
            }

            //! States that this is a type
            virtual bool is_type() const
            {
                return true;
            }

            //! Convenience function that returns a wrapped 'signed int'
            static Type get_int_type(void);

            //! Returns a string with a declaration
            std::string get_simple_declaration(Scope sc, const std::string& symbol_name, 
                    TypeDeclFlags flags = NORMAL_DECLARATION) const;
            //! Returns a string with a declaration
            std::string get_declaration(Scope sc, const std::string& symbol_name,
                    TypeDeclFlags flags = NORMAL_DECLARATION) const;

            //! Returns a string with a declaration and suitable initializer
            std::string get_declaration_with_initializer(Scope sc, 
                    const std::string& symbol_name, const std::string& initializer,
                    TypeDeclFlags flags = NORMAL_DECLARATION) const;

            //! Returns a string with a function declaration and parameter names
            std::string get_declaration_with_parameters(Scope sc,
                    const std::string& symbol_name, ObjectList<std::string>& parameters,
                    TypeDeclFlags flags = NORMAL_DECLARATION) const;

            //! Basic type
            /*! Returns the basic type on which this type is built 
             * Note that if the type is originally a scalar it will
             * return the same type.
             */
            Type basic_type() const;

            //! Returns a pointer to the current type
            Type get_pointer_to();
            //! Returns an array to the current type
            /*! 
             * \param expression_array The expression of the array. Can be an invalid tree if the array is unbounded.
             * \param scope Scope of \a expression_array
             */
            Type get_array_to(AST_t expression_array, Scope scope);
            //! Gets a reference (C++) to the current type
            Type get_reference_to();

            bool operator==(Type t) const;
            bool operator!=(Type t) const;
            Type& operator=(Type t);
            bool operator<(Type t) const;

            // Basic types
            //! States whether this type is an integral type 
            /*!
             * An integral type is any 'int', 'bool', 'character',
             * 'wchar_t'. In C, it also includes enum types.
             */
            bool is_integral_type() const;
            //! States whether this type is 'int' or 'signed int'
            bool is_signed_int() const;
            //! States whether this type is 'unsigned int'
            bool is_unsigned_int() const;
            //! States whether this type is 'short int' or 'signed short int'
            bool is_signed_short_int() const;
            //! States whether this type is 'unsigned short int'
            bool is_unsigned_short_int() const;
            //! States whether this type is 'long int' or 'signed long int'
            bool is_signed_long_int() const;
            //! States whether this type is 'unsigned long int'
            bool is_unsigned_long_int() const;
            //! States whether this type is 'long long int' or 'signed long long int'
            bool is_signed_long_long_int() const;
            //! States whether this type is 'unsigned long long int'
            bool is_unsigned_long_long_int() const;

            //! States whether this type is 'char' type
            bool is_char() const;
            //! States whether this type is 'signed char' type
            bool is_signed_char() const;
            //! States whether this type is 'unsigned char' type
            bool is_unsigned_char() const;

            //! States whether this type is 'wchar_t'
            bool is_wchar_t() const;

            //! States whether this type is either 'float', 'double' or 'long double'
            bool is_floating_type() const;
            //! States whether this type is 'long double'
            bool is_long_double() const;
            //! States whether this type is 'double'
            bool is_double() const;
            //! States whether this type is 'float'
            bool is_float() const;

            //! States whether this type is 'bool'
            bool is_bool() const;

            //! States whether this type is '_Complex' qualified
            bool is_complex() const;
            
            //! States wheter this is a direct type
            /*!
             * \deprecated Use instead is_scalar_type
             */
            bool is_direct_type() const DEPRECATED;
            //! States that this type is not structurally derived
            /*!
             * Structurally derived means any of pointer, references,
             * pointer-to-member, arrays or functions types
             */
            bool is_non_derived_type() const;
            //! States that this type is a scalar
            /*!
             * This is the same as is_non_derived_type
             */
            bool is_scalar_type() const;
            //! States that this type is a class-type (either named or unnamed)
            bool is_class() const;
            //! States that this type is an unnamed class-type
            bool is_unnamed_class() const;
            //! States that this type is an named class-type
            bool is_named_class() const;
            //! States that this type is an enum-type (either named or unnamed)
            bool is_enum() const;
            //! States that this type is an unnamed enum-type
            bool is_unnamed_enum() const;
            //! States that this type is a named enum-type
            bool is_named_enum() const;

            //! Returns the related symbol of this named type
            Symbol get_symbol() const;
            //! States whether current type is named
            bool is_named() const;
            //! States whether current type is a typedef
            bool is_typedef() const;
            //! For a typedef, it returns the aliased type
            Type aliased_type() const;

            //! States whether current type type is a function-type
            bool is_function() const;
            //! For a function-type, it gives the returned type
            Type returns() const;
            //! For a function type, it gives a list of parameter types
            /*!
             * \return A list of types of the parameters
             */
            ObjectList<Type> parameters() const;
            //! For a function type, it gives a list of parameter types
            /*!
             * \param has_ellipsis Will be set to true if the function type has ellipsis
             * \return A list of types of the parameters
             */
            ObjectList<Type> parameters(bool& has_ellipsis) const;
            //! For a function type it states whether it has been declared with prototype
            /*! 
             * This is only meaningful in C because in C++ all functions have prototype 
             */
            bool lacks_prototype() const;

            //! States whether current type is a pointer type
            bool is_pointer() const;
            //! In pointer-types or pointer-to-member types returns the referenced type
            Type points_to() const;

            //! States whether current type is a pointer-to-member type
            bool is_pointer_to_member() const;
            //! In pointer-to-member types returns the class of the pointer
            Type pointed_class() const;

            //! States whether current type is an array-type
            bool is_array() const;
            //! Returns the element type of an array-type
            Type array_element() const;
            //! States whether this array-type has an explicit array dimension
            bool explicit_array_dimension() const;
            //! Returns the expression of the array dimension
            AST_t array_dimension() const;

            //! States whether the type is a dependent one
            /*!
             * Symbol t below will have a dependent type
             * \code
             * template <typename _T>
             * struct A
             * {
             *   typename _T::B t;
             * };
             * \endcode
             */
            bool is_dependent() const;

            //! States whether the type is a reference type
            bool is_reference() const;
            //! Returns the referenced type
            Type references_to() const;

            //! States whether this type is void
            bool is_void() const;

            //! Synonim of get_nonstatic_data_members
            ObjectList<Symbol> get_fields() const;

            //! Returns a list with the nonstatic data members of a class-type
            ObjectList<Symbol> get_nonstatic_data_members() const;
            //! Returns a list with the static data members of a class-type
            ObjectList<Symbol> get_static_data_members() const;

            //! States whether any nonstatic member of class-type is defined as mutable
            bool some_member_is_mutable() const;

            //! States whether this type is const qualified
            bool is_const() const;
            //! States whether this type is volatile qualified
            bool is_volatile() const;
            //! States whether this type is restrict qualified
            bool is_restrict() const;

            //! Returns the unqualified type of current type
            Type get_unqualified_type();
            //! Returns a const qualified type of current type
            Type get_const_type();
            //! Returns a volatile qualified type of current type
            Type get_volatile_type();
            //! Returns a restrict qualified type of current type
            Type get_restrict_type();

            /*!
             * \bug What does this function do?
             */
            Type original_type(void) const;

            //! States whether the type is a template specialized one
            bool is_template_specialized_type() const;
            //! For a template-specialized type return the list of template-parameters
            ObjectList<Symbol> get_template_parameters() const;

            //! States whether two types represent the same type
            /*!
            * Note that this function will return true whenever 'operator==' returns
            * true, but this function might return true even if 'operator==' returned false.
            * So do not use 'operator==' to check type system equality.
            */
            bool is_same_type(Type t);

            //! States whether two types represent the same type
            /*!
             * \deprecated Do not use this one instead use is_same_type(Type)
             */
            bool is_same_type(Type t, Scope sc) DEPRECATED;

            /* We should consider to remove this one day */
            friend class Symbol;
            friend class Source;
            friend class Scope;

            /* Do not use it unless directed to do so */
            type_t* get_internal_type()
            {
                return _type_info;
            }
    };
    
    //! @}
}

#endif // TL_TYPE_HPP
