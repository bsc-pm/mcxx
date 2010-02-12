/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#ifndef TL_TYPE_HPP
#define TL_TYPE_HPP

#include "tl-common.hpp"
#include <string>
#include "tl-object.hpp"
#include "tl-symbol.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"
#include "tl-templates.hpp"
#include "cxx-scope.h"

namespace TL
{
    class Scope;
    class Symbol;
    class TemplateParameter;
    class TemplateArgument;
    
    //! \addtogroup Wrap
    //! @{
    
    //! This class wraps a type in the compiler type system
    class LIBTL_CLASS Type : public Object
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

            //! Returns an array to the current type
            /*! 
             * Use this for arrays with empty dimension including C99 
             * wildcard sized arrays
             */
            Type get_array_to();

            //! Convenience function that returns an array type built after a dimension string
            /*!
              The frontend never creates this kind of array types. They exist
              to ease array type creation in TL. They should be only used for
              types that are going to be prettyprinted.
              */
            Type get_array_to(const std::string& str);

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

            //! States this type is POD
            /*! 
              Informally, in C++ terms, a POD types is something you could have
              written in C */
            bool is_pod();
            
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

            //! Advances over typedefs
            /*! 
              This function advances over a typedef as many times
              as needed until it reaches a type that is not a typedef.
              Note that this function does not combine cv-qualifiers
              found while advancing typedefs. This is relevant
              for cases like the one below:

              \code
              typedef int T;
              typedef const T Q;
              typedef volatile S;
              \endcode

              'S' would be simplified to 'int' while if cv-qualifiers
              were properly considered it would be 'const volatile int'.
              Use advance_over_typedefs_cv to get the properly cv-qualified
              type.
              */
            Type advance_over_typedefs();

            //! Advances over typedefs preserving cv-qualification
            /*!
              \see advance_over_typedefs
             */
            Type advance_over_typedefs_cv();

            //! States whether current type type is a function-type
            bool is_function() const;
            //! For a function-type, it gives the returned type
            Type returns() const;
            //! For a function type, it gives a list of parameter types
            /*!
             * \return A list of types of the parameters
             * Note that these types are adjusted so function and array types
             * are adjusted to their pointer type. Top level const is also
             * dropped.
             */
            ObjectList<Type> parameters() const;


            //! For a function type, it gives a list of parameter types
            /*!
             * \param has_ellipsis Will be set to true if the function type has ellipsis
             * \return A list of types of the parameters
             */
            ObjectList<Type> parameters(bool& has_ellipsis) const;

            //! For a function type, it returns a list of parameter types
            /*
             * \return A list of types of the parameters
             * Note that these types are the originals of the first 
             * declaration of the function. They may or may not be what you expect.
             */
            ObjectList<Type> nonadjusted_parameters() const;

            //! For a function type, it gives a list of parameter types
            /*!
             * \param has_ellipsis Will be set to true if the function type has ellipsis
             * \return A list of types of the parameters not adjusted.
             */
            ObjectList<Type> nonadjusted_parameters(bool &has_ellipsis) const;

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

            //! [C only] States whether current array is a VLA
            bool array_is_vla() const;


            //! States whether this type represents an unresolved overload type
            /*! 
              Unresolved overloads are expressions whose type cannot be
              determine because they designate an overloaded function name
              */
            bool is_unresolved_overload();

            //! Get the overload set
            /*! For a type that is an unresolved overload type,
              this returns the candidate function set. This is
              useful when manually solving overloads
              by means of Overload class.

              \see Overload
              */
            ObjectList<Symbol> get_unresolved_overload_set();

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

            //! States whether the type is the result of a type dependent expression
            /*! Consider the following case
             *
             *   template <typename _T>
             *   void f(_T t)
             *   {
             *      t + 1;
             *   }
             *
             * Expression 't + 1' is a type dependent expression since the exact
             * depends on some objects whose type is dependent. These expressions
             * cannot be checked until instatiation time
             */
            bool is_expression_dependent() const;

            //! States whether the current type is incomplete
            bool is_incomplete() const;

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

            //! Is variably modified type
            bool is_variably_modified() const;

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

            //! States that the type is a template type
            /*!
              A template type is the type of a template-name like A and f
              in the example below.

              template <typename _T> struct A { };
              template <typename _T> void f(_T) { }


              Note that 'A<int>' and 'f(3)' (which is like 'f<int>(3)')
              are not template-types but template specialized types
             */
            bool is_template_type() const;

            //! Returns the primary template of a template type
            //! This is always a named type, so you can get a symbol after it
            Type get_primary_template() const;

            //! Returns the template parameters of a template type
            /*!
              This function can be used both in template types and in template 
              specialized types
              */
            ObjectList<TemplateParameter> get_template_parameters() const;

            //! States whether the type is a template specialized one
            /*!
              A template specialized type is a type which was created
              not by a user declaration but the instantiation of
              a template type.
            */
            bool is_template_specialized_type() const;
            //! Returns the template arguments of a specialized template type
            ObjectList<TemplateArgument> get_template_arguments() const;

            //! Returns the related template type of a specialized template type
            /*!
              This function is only valid for template specialized types
            */
            Type get_related_template_type() const;

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

            //! Returns what sizeof would yield for this type
            /*! Note that the result of this function depends on the current type environment */
            unsigned int get_size();

            //! Returns all the arithmetic types
            /*!  These types include all the integer types and floating types */
            static ObjectList<Type> get_arithmetic_types();
            //! Returns all the integer types
            /*! These types include all variants of char 
              and int including all long, short and unsigned varieties */
            static ObjectList<Type> get_integer_types();
            //! Returns all the floating types
            /*! These types include float, double and long double */
            static ObjectList<Type> get_floating_types();

            //! Convenience function that returns a wrapped 'void'
            static Type get_void_type(void);

            //! Convenience function that returns a wrapped 'signed int'
            static Type get_int_type(void);
    };
    
    //! @}
}

#endif // TL_TYPE_HPP
