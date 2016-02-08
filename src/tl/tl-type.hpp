/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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




#ifndef TL_TYPE_HPP
#define TL_TYPE_HPP

#include "tl-common.hpp"
#include <string>
#include "tl-object.hpp"
#include "tl-objectlist.hpp"
#include "tl-scope-fwd.hpp"
#include "tl-type-fwd.hpp"
#include "tl-nodecl-fwd.hpp"

#include "tl-symbol.hpp"

#include "cxx-scope.h"

namespace TL
{
    //! \addtogroup Wrap
    //! @{

    class LIBTL_CLASS TemplateArgument : public Object
    {
        private:
            template_parameter_value_t* _tpl_param_value;
        public:
            TemplateArgument(template_parameter_value_t* tpl_param_value) : _tpl_param_value(tpl_param_value) { }

            typedef enum template_parameter_kind TemplateArgumentKind;

            //! Returns the kind of this template argument
            TemplateArgumentKind get_kind() const;

            //! Retrieves the value of nontype template arguments
            Nodecl::NodeclBase get_value() const;

            //! Retrieves the type of the template argument
            TL::Type get_type() const;

            //! States whether this template argument is actually a default template argument of some template parameter
            bool is_default() const;
    };

    //! This class wraps a context of template parameters and its arguments
    class LIBTL_CLASS TemplateParameters : public Object
    {
        private:
            template_parameter_list_t* _tpl_params;
        public:
            TemplateParameters(template_parameter_list_t* tpl_params) : _tpl_params(tpl_params) { }

            bool operator==(TemplateParameters t) const;
            bool operator!=(TemplateParameters t) const;

            bool is_valid() const;

            /* Do not use it unless directed to do so */
            template_parameter_list_t* get_internal_template_parameter_list()
            {
                return _tpl_params;
            }

            //! Returns the number of parameters in the current parameter level
            int get_num_parameters() const;

            typedef enum template_parameter_kind TemplateParameterKind;

            //! Returns the n-th template parameter as a pair of the symbol and its kind
            std::pair<TL::Symbol, TemplateParameterKind> get_parameter_num(int n) const;

            //! Returns if this template parameter level has an argument at position n
            bool has_argument(int n) const;

            //! Returns the n-th template argument
            TemplateArgument get_argument_num(int n) const;

            //! States whether this level of template parameters is nested in another level of template parameters
            bool has_enclosing_parameters() const;

            //! Returns the enclosing template parameter level
            TemplateParameters get_enclosing_parameters() const;

            bool get_is_explicit_specialization() const;
            bool get_is_explicit_instantiation() const;
    };


    //! This class wraps a type in the compiler type system
    class LIBTL_CLASS Type : public Object
    {
        private:
            type_t* _type_info;
            virtual tl_type_t* get_extended_attribute(const std::string&) const
            {
                return NULL;
            }

            Type fix_references_();
        public:

            enum TypeDeclFlags
            {
                NORMAL_DECLARATION,
                PARAMETER_DECLARATION
            };

            Type() : _type_info(NULL) { }

            Type(type_t* type_info)
                : _type_info(type_info)
            {
            }

            Type(const Type& type)
                : Object(type), _type_info(type._type_info)
            {
            }

            //! States whether the type is valid
            /*!
             * This means that the wrapped type really refers a useful type.
             *
             * If the wrapped type is NULL this function returns true, false otherwise
             * Invalid types appear because we expect a type to be computed
             * somewhere but it was not computed.
             */
            bool is_valid() const
            {
                return (_type_info != NULL);
            }

            //! States whether this type is the error type
            /*!
              Even if the wrapped type is valid (is_valid returns true) it
              might be an error type. Error types appear because the frontend
              encountered some problem when computing the type. Usually
              semantic problems during typechecking cause the error type be
              computed.
              */
            bool is_error_type() const;

            bool is_faulty() const
            {
                return false;
            }

            virtual ~Type()
            {
            }

            Type fix_references();

            //! Returns a string with a C/C++ declaration
            std::string get_simple_declaration(Scope sc, const std::string& symbol_name,
                    TypeDeclFlags flags = NORMAL_DECLARATION) const;
            //! Returns a string with a C/C++ declaration
            std::string get_declaration(Scope sc, const std::string& symbol_name,
                    TypeDeclFlags flags = NORMAL_DECLARATION) const;

            //! Returns a string with a C/C++ declaration and suitable initializer
            std::string get_declaration_with_initializer(Scope sc,
                    const std::string& symbol_name, const std::string& initializer,
                    TypeDeclFlags flags = NORMAL_DECLARATION) const;

            //! Returns a string with a C/C++ function declaration and parameter names
            std::string get_declaration_with_parameters(Scope sc, const std::string& symbol_name,
                    ObjectList<std::string>& parameters, ObjectList<std::string>& parameter_attributes, TypeDeclFlags flags = NORMAL_DECLARATION) const;

            // Returns a string with a Fortran declaration
            std::string get_fortran_declaration(Scope sc, const std::string& symbol_name, TypeDeclFlags flags = NORMAL_DECLARATION);

            //! Basic type
            /*! Returns the basic type on which this type is built
             * Note that if the type is originally a scalar it will
             * return the same type.
             */
            Type basic_type() const;

            //! Returns the canonical unqualified type
            Type get_canonical_type();

            //! Returns the underlying type of an enumeration
            Type get_enum_underlying_type();

            //! For an enum type get all its enumerators
            ObjectList<Symbol> enum_get_enumerators();

            //! Returns a pointer to the current type
            Type get_pointer_to() const;

            //! Returns a vector to the current type
            /*!
             * \param vector_size The size of the vector in bytes.
             */
            Type get_vector_of_bytes(unsigned int vector_size) const;

            //! Returns a vector to the current type
            /*!
             * \param num_elements The number of scalar elements of the vector
             */
            Type get_vector_of_elements(unsigned int num_elements) const;
       
            //! Returns a generic vector to the current type
            Type get_generic_vector_to();

            //! Returns an array to the current type
            /*!
             * \param expression_array The expression of the array. Can be an invalid tree if the array is unbounded.
             * \param scope Scope of \a expression_array
             */
            Type get_array_to(Nodecl::NodeclBase expression_array, Scope scope);

            //! Returns an array to the current type
            /*!
             * Use this for arrays with empty dimension including C99
             * wildcard sized arrays
             */
            Type get_array_to();

            //! Returns a ranged array to the current type
            /*!
             * \param lower_bound The lower bound expression of the array.
             * \param upper_bound The upper bound expression of the array.
             * \param scope Scope of \a lower_bound and \a upper_bound
             */
            Type get_array_to(Nodecl::NodeclBase lower_bound, Nodecl::NodeclBase upper_bound, Scope scope);

            //! Returns a ranged array to the current type with descriptor
            /*!
             * \param lower_bound The lower bound expression of the array.
             * \param upper_bound The upper bound expression of the array.
             * \param scope Scope of \a lower_bound and \a upper_bound
             */
            Type get_array_to_with_descriptor(Nodecl::NodeclBase lower_bound, Nodecl::NodeclBase upper_bound, Scope scope);

            //! Returns a ranged array to the current type with descriptor
            /*!
             * \param lower_bound The lower bound expression of the array.
             * \param upper_bound The upper bound expression of the array.
             * \param scope Scope of \a lower_bound and \a upper_bound
             */
            Type get_array_to_with_region(Nodecl::NodeclBase lower_bound,
                    Nodecl::NodeclBase upper_bound,
                    Nodecl::NodeclBase region_lower_bound,
                    Nodecl::NodeclBase region_upper_bound,
                    Scope scope);

            //! Gets a lvalue reference (C++) to the current type
            Type get_lvalue_reference_to();

            //! Gets a rvalue reference (C++2011) to the current type
            Type get_rvalue_reference_to();

            //! Gets a rebindable reference (Mercurium extension) to the current type
            Type get_rebindable_reference_to();

            //! Returns a function to the current list of parameter types
            /*!
             * \param type_list List of parameter types of the function.
             * \param has_ellipsis Will be set to true if the function type has ellipsis
             * \param reference_qualifier Sets the ref-qualifier of the function. This is for C++2011
             */
            Type get_function_returning(const ObjectList<Type>& type_list,
                    bool has_ellipsis = false,
                    ref_qualifier_t reference_qualifier = REF_QUALIFIER_NONE);

            //! Returns the reference qualifier
            /*!
             * This is only meaningful in C++2011
             */
            ref_qualifier_t get_reference_qualifier() const;

            //! Returns a function to the current list of parameter types
            /*!
             * \param type_list List of parameter types of the function.
             * \param nonadjusted_type_list List of nonadjusted parameter types of the function
             * \param has_ellipsis Will be set to true if the function type has ellipsis
             * \param reference_qualifier Sets the ref-qualifier of the function. This is for C++2011
             */
            Type get_function_returning(const ObjectList<Type>& type_list,
                    const ObjectList<Type>& nonadjusted_type_list,
                    bool has_ellipsis = false,
                    ref_qualifier_t reference_qualifier = REF_QUALIFIER_NONE);

            //! If the type is a reference, it returns the referenced tye
            /*!
             * This function is a no-op in C and Fortran
             */
            Type no_ref() const;

            //! Returns the alignment of the type
            int get_alignment_of();

            Type& operator=(Type t);
            bool operator<(Type t) const;

            // Basic types
            //! States whether this type is an integral type
            /*!
             * An integral type is any 'int', 'bool', 'character',
             * 'wchar_t'. In C, it also includes enum types.
             */
            bool is_integral_type() const;
            //! States whether this type is a signed integral type
            bool is_signed_integral() const;
            //! States whether this type is a unsigned integral type
            bool is_unsigned_integral() const;
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

            //! States this type is a POD in C++ Standard meaning
            bool is_pod();

            //! States if this type is an aggregate in the meaning of C++
            bool is_aggregate();

            //! States if this type is a builtin type
            bool is_builtin();

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
            //! Returns the underlying integer of this enum type
            Type enum_get_underlying_type() const;

            //! Returns the related symbol of this named type
            Symbol get_symbol() const;
            //! States whether current type is named
            bool is_named() const;
            //! States whether current type is indirect
            /*!
             * All indirect types are named types but not the opposite
             * Indirect types are like named type but the symbol is neither a class nor an enum
             */
            bool is_indirect() const;

            //! Advances over typedefs
            /*!
              This function advances over a typedef as many times
              as needed until it reaches a type that is not a typedef.
              */
            Type advance_over_typedefs();

            //! Get the symbol list of classes which are base of the type
            ObjectList<Symbol> get_bases_class_symbol_list();

            struct BaseInfo
            {
                TL::Symbol base;
                bool is_virtual;
                bool is_dependent;
                bool is_expansion;
                access_specifier_t access_specifier;

                BaseInfo(TL::Symbol _base,
                        bool _is_virtual,
                        bool _is_dependent,
                        bool _is_expansioexpansion,
                        access_specifier_t _access_specifier);
            };

            ObjectList<BaseInfo> get_bases();

            //! Returns the friends of this class
            ObjectList<Symbol> class_get_friends();

            //! Returns the list of classes the constructors of which are inherited
            ObjectList<Symbol> class_get_inherited_constructors();

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

            //! For a function type in C99, it states whether it has been declared with prototype
            /*!
             * This is only meaningful in C because in C++ all functions have prototype
             */
            bool lacks_prototype() const;

            //! For a function type in C++2011, it states whether it has been declared with a trailing return
            bool is_trailing_return() const;

            //! States whether current type is a pointer type
            bool is_pointer() const;
            //! In pointer-types or pointer-to-member types returns the referenced type
            Type points_to() const;

            //! States whether the current type is a pointer to a class type
            /*!
             * \note Not to be confused with being a pointer to member of class
             */
            bool is_pointer_to_class() const;

            //! States whether current type is a pointer-to-member type
            bool is_pointer_to_member() const;
            //! In pointer-to-member types returns the class of the pointer
            Type pointed_class() const;

            //! Current class is a base of t
            bool is_base_class(Type t) const;

            //! Current class is a derived class of t
            bool is_derived_class(Type t) const;

            //! States whether current type is an array-type
            bool is_array() const;

            //! States whether current type is an array-type in Fortran terminology
            // All fortran array are array types but not all array types are fortran arrays
            // In particular CHARACTER(LEN=x) are arrays but not Fortran array
            bool is_fortran_array() const;

            //! Returns the rank of a fortran array type
            int fortran_rank() const;

            //! Returns the element type of an array-type
            Type array_element() const;
            //! States whether this array-type has an explicit array dimension
            bool array_has_size() const;

            //! Returns the expression of the array dimension
            Nodecl::NodeclBase array_get_size() const;

            //! States whether the frontend flagged this array as requiring an in-memory descriptor
            /*!
             * This only happens in Fortran for some array kinds
             */
            bool array_requires_descriptor() const;

            //! Return the number of dimensions for an array type or 0 for the rest of types
            int get_num_dimensions() const;

            //! This returns the bounds of the array
            /*!
              The array bounds are expressed as a range of [lower, upper] (both ends included)
              \a lower Output argument with the tree of the lower expression boundary
              \a upper Output argument with the tree of the upper expression boundary

              In C all the arrays with explicit size will have a lower of zero
              and an upper of N-1 where N is the size of the array as returned
              by array_get_size
              */
            void array_get_bounds(Nodecl::NodeclBase& lower, Nodecl::NodeclBase& upper) const;

            //! States that this array type has region attached to it
            bool array_is_region() const;

            //! This returns the bounds of the array region
            /*! See array_get_bounds for an explanation of the returned Nodecl::NodeclBase */
            void array_get_region_bounds(Nodecl::NodeclBase& region_lower, Nodecl::NodeclBase& region_upper) const;

            //! This returns the expression of the array region size
            Nodecl::NodeclBase array_get_region_size() const;

            //! [C and Fortran] States whether current array is a VLA
            /*!
             * There are no VLAs in Fortran but this attribute will be true for
             * those arrays not requiring descriptors the size of which is non
             * constant (like explicit shape arrays with bounds depending on
             * dummy arguments)
             */
            bool array_is_vla() const;

            //! States whether current type is a vector-type
            bool is_vector() const;
            //! States whether current type is a mask-type
            bool is_mask() const;
            //! Returns the size of a mask type
            int get_mask_num_elements() const;
            //! States whether current type is a generic vector-type
            bool is_generic_vector() const;
            //! Returns the element type of a vector-type
            Type vector_element() const;

            // ! States whether the current type is auto
            bool is_auto() const;

            // ! States whether the current type is decltype(auto)
            bool is_decltype_auto() const;

            //! Returns the number of elements of a vector-type
            int vector_num_elements() const;

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

            //! Get the explicit template arguments of an unresolved overload type
            TemplateParameters unresolved_overloaded_type_get_explicit_template_arguments();

            //! States whether the type is a dependent one
            /*!
             * Symbol t below will have a dependent type
             * \code
             * template \<typename _T\>
             * struct A
             * {
             *   typename _T::B t;
             * };
             * \endcode
             */
            bool is_dependent_typename() const;

            bool is_dependent() const;

            //! Decomposes the dependent typename into its entry symbol and its syntactic part
            void dependent_typename_get_components(Symbol& entry_symbol, Nodecl::NodeclBase& parts);

            //! States whether the type is the result of a type dependent expression
            /*! Consider the following case
             *
             *   template \<typename _T\>
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

            //! States whether the current type is a pack
            bool is_pack() const;

            //! Returns the packed type of an pack type
            TL::Type pack_type_get_packed() const;

            //! States whether the current type is incomplete
            bool is_incomplete() const;

            bool class_type_is_complete_independent() const;
            bool class_type_is_complete_dependent() const;
            bool class_type_is_incomplete_independent() const;
            bool class_type_is_incomplete_dependent() const;

            type_tag_t class_type_get_class_kind() const;

            bool class_type_is_packed() const;

            //! States whether the type is a lvalue or rvalue reference type
            bool is_any_reference() const;

            bool is_lvalue_reference() const;
            bool is_rvalue_reference() const;
            bool is_rebindable_reference() const;

            //! States whether the type is a lvalue reference to a class type
            bool is_any_reference_to_class() const;

            //! States that this is an interoperable type. Only meaningful in Fortran
            bool is_interoperable() const;

            //! Returns the referenced type
            Type references_to() const;

            //! Get the base type of a complex type
            Type complex_get_base_type() const;

            //! States whether this type is void
            bool is_void() const;

            //! Synonim of get_nonstatic_data_members
            ObjectList<Symbol> get_fields() const;

            //! Returns a list with the nonstatic data members of a class-type
            ObjectList<Symbol> get_nonstatic_data_members() const;
            //! Returns a list with the static data members of a class-type
            ObjectList<Symbol> get_static_data_members() const;

            //! Returns all the data members, either static or non-static
            ObjectList<Symbol> get_all_data_members() const;

            //! Returns all the data members, either static or non-static
            ObjectList<Symbol> get_all_members() const;

            //! Returns (all) the data member declarations
            ObjectList<MemberDeclarationInfo> get_member_declarations() const;

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

            //! Returns the unqualified type of current type but keeps restrict
            Type get_unqualified_type_but_keep_restrict();

            //! Qualifies current type with the qualifier of t
            Type get_as_qualified_as(TL::Type t);

            //! Adds the qualification of t to the current type
            Type get_added_qualification_of(TL::Type t);

            //! States that the type is a template type
            /*!
              A template type is the type of a template-name like A and f
              in the example below.

              template \<typename _T\> struct A { };
              template \<typename _T\> void f(_T) { }


              Note that 'A\<int\>' and 'f(3)' (which is like 'f\<int\>(3)')
              are not template-types but template specialized types
             */
            bool is_template_type() const;

            //! Returns all the specializations of a template type
            ObjectList<Type> get_specializations() const;

            //! Returns the template parameters of a template type
            TemplateParameters template_type_get_template_parameters() const;

            //! Returns the primary template of a template type
            //! This is always a named type, so you can get a symbol after it
            Type get_primary_template() const;

            //! States whether the type is a template specialized one
            /*!
              A template specialized type is a type which belongs to
              the specialized set of a template type
            */
            bool is_template_specialized_type() const;

            //! Returns the template parameters of a specialized template type
            TemplateParameters template_specialized_type_get_template_parameters() const;

            //! Returns the template arguments of a specialized template type
            TemplateParameters template_specialized_type_get_template_arguments() const;


            //! Returns the related template type of a specialized template type
            /*!
              This function is only valid for template specialized types
            */
            Type get_related_template_type() const;

            //! Returns the symbol of this template type
            /*!
             * \note Do not confuse with get_related_template_type which only applies for specialized_template types
             */
            Symbol get_related_template_symbol() const;

            //! States whether two types represent the same type
            /*!
            * Note that this function will return true whenever 'operator==' returns
            * true, but this function might return true even if 'operator==' returned false.
            * So do not use 'operator==' to check type system equality.
            */
            bool is_same_type(Type t) const;

            //! States whether the current type depends on nonconstant values
            bool depends_on_nonconstant_values() const;

            /* We should consider to remove this one day */
            friend class Symbol;
            friend class Source;
            friend class Scope;

            /* Do not use it unless directed to do so */
            type_t* get_internal_type() const
            {
                return _type_info;
            }

            //! Returns what sizeof would yield for this type
            /*! Note that the result of this function depends on the current type environment */
            unsigned int get_size() const;

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

            //! Convenience function that returns a wrapped 'bool'
            static Type get_bool_type(void);

            //! Convenience function that returns a wrapped 'signed char'
            static Type get_char_type(void);

            //! Convenience function that returns a wrapped 'unsigned char'
            static Type get_unsigned_char_type(void);

            //! Convenience function that returns a wrapped 'signed short int'
            static Type get_short_int_type(void);

            //! Convenience function that returns a wrapped 'unsigned short int'
            static Type get_unsigned_short_int_type(void);

            //! Convenience function that returns a wrapped 'signed int'
            static Type get_int_type(void);

            //! Convenience function that returns a wrapped 'unsigned int'
            static Type get_unsigned_int_type(void);

            //! Convenience function that returns a wrapped 'signed long int'
            static Type get_long_int_type(void);

            //! Convenience function that returns a wrapped 'unsigned long int'
            static Type get_unsigned_long_int_type(void);

            //! Convenience function that returns a wrapped 'signed long long int'
            static Type get_long_long_int_type(void);

            //! Convenience function that returns a wrapped 'unsigned long long int'
            static Type get_unsigned_long_long_int_type(void);

            //! Convenience function that returns a wrapped 'float'
            static Type get_float_type(void);

            //! Convenience function that returns a wrapped 'double'
            static Type get_double_type(void);

            //! Convenience function that returns a wrapped vector mask
            static Type get_mask_type(unsigned int mask_size);

            //! Concenience function that returns an 'auto' type specifier
            static Type get_auto_type();
            
            //! Integer type of size_t 
            /*!
             * This type is the underlying integer type of a size_t, since it
             * may change depending on the architecture
             */
            static Type get_size_t_type();
            
            //! Integer type of ptrdiff_t 
            /*!
             * This type is the underlying integer type of a ptrdiff_t, since it
             * may change depending on the architecture
             */
            static Type get_ptrdiff_t_type();

            std::string print_declarator() const;
    };

    //! @}
}

#endif // TL_TYPE_HPP
