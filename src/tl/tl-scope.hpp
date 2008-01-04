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
#ifndef TL_SCOPE_HPP
#define TL_SCOPE_HPP

#include <string>
#include <vector>
#include <map>
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "tl-object.hpp"
#include "tl-objectlist.hpp"
#include "tl-symbol.hpp"
#include "tl-ast.hpp"

namespace TL
{
    class Symbol;
    class Scope : public Object
    {
        private:
            // This class used to save scope_t* but not it holds
            // decl_context_t, that allows us to reach all the implied scopes
            // in some place
            bool _valid;
            decl_context_t _decl_context;
            static void convert_to_vector(scope_entry_list_t* entry_list, ObjectList<Symbol>& out);
            static void get_head(const ObjectList<Symbol>& in, Symbol& out);
        protected:
            virtual tl_type_t* get_extended_attribute(const std::string& str) const;
        public:
            Scope()
                : _valid(0)
            {
            }

            Scope(const decl_context_t& decl_context)
                : _valid(1), _decl_context(decl_context)
            {
            }

            Scope(const Scope& sc)
                : Object(sc), _valid(1), _decl_context(sc._decl_context)
            {
            }

            bool is_valid() const
            {
                if (_valid)
                    return false;
                return _decl_context.current_scope != NULL;
            }

            Scope(RefPtr<Object> obj)
            {
                RefPtr<Scope> sc = RefPtr<Scope>::cast_dynamic(obj);
                if (sc.get_pointer() != NULL)
                {
                    this->_decl_context = sc->_decl_context;
                }
                else
                {
                    if (typeid(*obj.get_pointer()) != typeid(Undefined))
                    {
                        std::cerr << "Bad initialization for Scope" << std::endl;
                    }
                }
            }

            void printscope();

            ObjectList<Symbol> get_symbols_from_name(const std::string& str) const;

            Symbol get_symbol_from_name(const std::string& str) const;
            
            ObjectList<Symbol> get_symbols_from_id_expr(TL::AST_t ast) const;

            Symbol get_symbol_from_id_expr(TL::AST_t ast) const;

            Scope temporal_scope() const;

            virtual bool is_scope() const
            {
                return true;
            }

            Scope& operator=(Scope sc);
            bool operator<(Scope sc) const;
            bool operator==(Scope sc) const;
            bool operator!=(Scope sc) const;

            friend class Type;
            friend class Source;
            friend class Expression;
    };
}

#endif // TL_CONTEXT_HPP
