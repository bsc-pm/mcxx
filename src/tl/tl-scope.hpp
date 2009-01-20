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
#include <cstring>
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
    
    //! \addtogroup Wrap 
    //! @{

    //! Class that represents a context
    /*!
     * This class used to hold a scope_t* but now it holds a decl_context_t.
     * This allows greater flexibility.
     */
    class Scope : public Object
    {
        private:
            bool _valid;
            decl_context_t _decl_context;
            static void convert_to_vector(scope_entry_list_t* entry_list, ObjectList<Symbol>& out);
            static void get_head(const ObjectList<Symbol>& in, Symbol& out);
        protected:
            virtual tl_type_t* get_extended_attribute(const std::string& str) const;
        public:
            Scope()
                : _valid(0), _decl_context(decl_context_empty())
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

			//! Do not use this one unless directed to do so
			decl_context_t get_decl_context()
			{
				return _decl_context;
			}

            //! States whether the scope is valid
            bool is_valid() const
            {
                if (_valid)
                    return false;
                return _decl_context.current_scope != NULL;
            }

            //! Creates a scope after a reference to Object
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

            //! Debugging function that prints the scope
            /*!
             * The compiler will dump the scope calling the internal
             * function
             */
            void printscope();

            //! Get a list of symbols in this scope with name \a str
            /*!
             * \param str The unqualified name looked up
             * \return A list of Symbol that have this name \a str in the current scope
             */
            ObjectList<Symbol> get_symbols_from_name(const std::string& str) const;

            //! Convenience function where only one symbol is expected
            Symbol get_symbol_from_name(const std::string& str) const;
            
            //! Get a list of symbols denoted by the id-expression in \a ast
            /*!
             * \param ast A tree representing an id-expression
             */
            ObjectList<Symbol> get_symbols_from_id_expr(TL::AST_t ast) const;

            //! Convenience function where only one symbol is expected
            Symbol get_symbol_from_id_expr(TL::AST_t ast) const;

            //! Builds a fake temporal scope not related to any real code
            Scope temporal_scope() const;

            //! Returns all symbols signed in in this scope
            /*! 
              \param include_hidden If true, hidden symbols (most of 
              the time internal to the compiler and not visible in the code) 
              will be considered as well
             */
            ObjectList<Symbol> get_all_symbols(bool include_hidden);

            //! States that this is a scope
            virtual bool is_scope() const
            {
                return true;
            }

            Scope& operator=(Scope sc);
            bool operator<(Scope sc) const;
            bool operator==(Scope sc) const;
            bool operator!=(Scope sc) const;

            friend class Symbol;
            friend class Type;
            friend class Source;
            friend class Expression;
    };
    
    //! @}
}

#endif // TL_SCOPE_HPP
