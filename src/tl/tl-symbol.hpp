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
#ifndef TL_SYMBOL_HPP
#define TL_SYMBOL_HPP

#include <string>
#include <vector>
#include <sstream>
#include "tl-ast.hpp"
#include "tl-object.hpp"
#include "tl-type.hpp"
#include "cxx-scope.h"

namespace TL
{
    class Type;
    class Scope;
    class Symbol : public Object
    {
        public:
            scope_entry_t* _symbol;

        private:
            virtual tl_type_t* get_extended_attribute(const std::string& str) const
            {
                return NULL;
            }

        public:
            static const Symbol invalid();

            bool is_invalid() const;

            bool is_valid() const;

            Symbol(scope_entry_t* symbol)
                : _symbol(symbol)
            {
            }

            Symbol(RefPtr<Object> obj)
            {
                RefPtr<Symbol> pint = RefPtr<Symbol>::cast_dynamic(obj);
                if (pint.get_pointer() != NULL)
                {
                    this->_symbol = pint->_symbol;
                }
                else
                {
                    if (typeid(*obj.get_pointer()) != typeid(Undefined))
                    {
                        std::cerr << "Bad initialization of Symbol" << std::endl;
                    }
                    this->_symbol = NULL;
                }
            }

            Type get_type() const;
            std::string get_name() const;

            std::string get_qualified_name() const;

            Scope get_scope() const;

            virtual ~Symbol()
            {
            }

            virtual bool is_symbol() const
            {
                return true;
            }

            bool operator<(Symbol s) const;
            bool operator==(Symbol s) const;
            bool operator!=(Symbol s) const;
            Symbol& operator=(Symbol s);

            bool is_variable() const;
            bool is_typename() const;
            bool is_function() const;
            bool is_template_function() const;
            bool is_member() const;
            bool is_parameter() const;

            Type get_class_type() const;

            AST_t get_point_of_declaration() const;
    };
}

#endif // TL_SYMBOL_HPP
