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
    class Type : public Object
    {
        private:
            type_t* _type_info;
            virtual tl_type_t* get_extended_attribute(const std::string& str) const
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
                : _type_info(type._type_info)
            {
            }

            bool is_valid() const
            {
                return (_type_info != NULL);
            }

            virtual ~Type()
            {
            }

            virtual bool is_type() const
            {
                return true;
            }

            static Type get_int_type(void);

            std::string get_simple_declaration(Scope sc, const std::string& symbol_name, 
                    TypeDeclFlags flags = NORMAL_DECLARATION) const;
            std::string get_declaration(Scope sc, const std::string& symbol_name,
                    TypeDeclFlags flags = NORMAL_DECLARATION) const;

            std::string get_declaration_with_initializer(Scope sc, 
                    const std::string& symbol_name, const std::string& initializer,
                    TypeDeclFlags flags = NORMAL_DECLARATION) const;

            std::string get_declaration_with_parameters(Scope sc,
                    const std::string& symbol_name, ObjectList<std::string>& parameters,
                    TypeDeclFlags flags = NORMAL_DECLARATION) const;

            Type get_pointer_to();
            Type get_array_to(AST_t expression_array, Scope scope);

            bool operator==(Type t) const;
            bool operator!=(Type t) const;
            Type& operator=(Type t);
            bool operator<(Type t) const;

            // Basic types
            bool is_integral_type() const;
            bool is_signed_int() const;
            bool is_unsigned_int() const;
            bool is_signed_short_int() const;
            bool is_unsigned_short_int() const;
            bool is_signed_long_int() const;
            bool is_unsigned_long_int() const;
            bool is_signed_long_long_int() const;
            bool is_unsigned_long_long_int() const;

            bool is_char() const;
            bool is_signed_char() const;
            bool is_unsigned_char() const;

            bool is_wchar_t() const;

            bool is_floating_type() const;
            bool is_long_double() const;
            bool is_float() const;

            bool is_bool() const;
            
            // Direct types
            bool is_direct_type() const DEPRECATED;
            bool is_non_derived_type() const;
            bool is_class() const;
            bool is_enum() const;

            // Functions
            bool is_function() const;
            Type returns() const;
            ObjectList<Type> parameters() const;
            ObjectList<Type> parameters(bool& has_ellipsis) const;

            // Pointers
            bool is_pointer() const;
            Type points_to() const;

            bool is_pointer_to_member() const;
            Type pointed_class() const;

            bool is_array() const;
            Type array_element() const;
            bool explicit_array_dimension() const;
            AST_t array_dimension() const;

            bool is_dependent() const;

            bool is_reference() const;
            Type references_to() const;

            bool is_void() const;

            // Returns the original type, if any, otherwise
            // returns the same
            Type original_type(void) const;

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
}

#endif // TL_TYPE_HPP
