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
            static std::string get_simple_type_name_str_internal(simple_type_t* simple_type);
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
                return (_type_info == NULL)
                    || (_type_info->kind != TK_UNKNOWN);
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

            Type duplicate();
            Type get_pointer_to();
            Type get_array_to(AST_t expression_array, Scope scope);

            bool operator==(Type t) const;
            bool operator!=(Type t) const;
            Type& operator=(Type t);
            bool operator<(Type t) const;

            struct BuiltinType;

            struct TypeModifier;

            bool is_builtin_type() const;
            BuiltinType builtin_type() const;
            BuiltinType builtin_type(TypeModifier& type_modif) const;

            bool is_direct_type() const;
            bool is_class() const;
            bool is_enum() const;

            bool is_function() const;
            Type returns() const;
            ObjectList<Type> parameters() const;
            ObjectList<Type> parameters(bool& has_ellipsis) const;

            bool is_pointer() const;
            Type points_to() const;

            bool is_array() const;
            Type array_element() const;
            bool explicit_array_dimension() const;
            AST_t array_dimension() const;

            bool is_dependent() const;

            bool is_reference() const;
            Type references_to() const;

            bool is_void() const;

            friend class Symbol;
            friend class Source;
            friend class Scope;
    };

    struct Type::BuiltinType
    {
        enum _enum_type
        {
            UNKNOWN = 0,
            INT,
            BOOL,
            FLOAT,
            DOUBLE,
            CHAR,
            WCHAR,
            VOID
        };

        BuiltinType()
            : _val(UNKNOWN)
        {
        }

        BuiltinType(const _enum_type& t)
            : _val(t) { }

        BuiltinType& operator=(const _enum_type& t)
        {
            _val = t;
            return (*this);
        }

        bool operator==(const _enum_type& t)
        {
            return (_val == t);
        }

        bool operator!=(const _enum_type& t)
        {
            return !(this->operator==(t));
        }

        bool operator==(const BuiltinType& t)
        {
            return _val == t._val;
        }

        bool operator!=(const BuiltinType& t)
        {
            return !(this->operator==(t));
        }

        private:
        _enum_type _val;
    };

    struct Type::TypeModifier
    {
        enum _enum_type
        {
            NONE = 0,
            SHORT = 1,
            LONG = 2,
            LONG_LONG = 4,
            UNSIGNED = 8,
            CONST = 16,
            VOLATILE = 32,
            RESTRICT = 64,
        };

        TypeModifier()
            : _val(NONE)
        {
        }

        TypeModifier(const _enum_type& t)
            : _val(t) { }

        TypeModifier& operator=(const _enum_type& t)
        {
            _val = t;
            return (*this);
        }

        bool operator==(const _enum_type& t)
        {
            return (_val == t);
        }

        bool operator!=(const _enum_type& t)
        {
            return !(this->operator==(t));
        }

        bool operator==(const TypeModifier& t)
        {
            return _val == t._val;
        }

        bool operator!=(const TypeModifier& t)
        {
            return !(this->operator==(t));
        }

        void operator|=(const _enum_type& t)
        {
            _val = (_enum_type)((int)(_val) | (int)(t));
        }

        void operator|=(const TypeModifier& t)
        {
            _val = (_enum_type)((int)(_val) | (int)(t._val));
        }
        
        TypeModifier operator&(const _enum_type& t)
        {
            TypeModifier result;
            result._val = (_enum_type)((int)(_val) & ((int)t));
            return result;
        }

        TypeModifier operator&(const TypeModifier& t)
        {
            TypeModifier result;
            result._val = (_enum_type)((int)(_val) & ((int)t._val));
            return result;
        }

        private:
        _enum_type _val;
    };
}

#endif // TL_TYPE_HPP
