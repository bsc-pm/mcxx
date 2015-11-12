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




#ifndef TL_SCOPE_HPP
#define TL_SCOPE_HPP

#include "tl-common.hpp"
#include "tl-object.hpp"
#include "tl-objectlist.hpp"
#include "tl-symbol-fwd.hpp"
#include "tl-type-fwd.hpp"
#include "tl-source-fwd.hpp"

#include "cxx-scope.h"
#include "cxx-buildscope.h"

#include <string>
#include <cstring>
#include <vector>
#include <map>

namespace TL
{
    //! \addtogroup Wrap 
    //! @{

    //! Class that represents a context
    /*!
     * This class used to hold a scope_t* but now it holds a const decl_context_t*.
     * This allows greater flexibility.
     */
    class LIBTL_CLASS Scope : public Object
    {
        private:
            const decl_context_t* _decl_context;
            static void get_head(const ObjectList<Symbol>& in, Symbol& out);
        protected:
            virtual tl_type_t* get_extended_attribute(const std::string& str) const;
        public:
            Scope()
                : _decl_context(decl_context_empty())
            {
            }

            Scope(const decl_context_t* decl_context)
                : _decl_context(decl_context)
            {
            }

            Scope(const Scope& sc)
                : Object(sc), _decl_context(sc._decl_context)
            {
            }

            //! Do not use this one unless directed to do so
            const decl_context_t* get_decl_context() const
            {
                return _decl_context;
            }

            //! States whether the scope is valid
            bool is_valid() const
            {
                return _decl_context != NULL;
            }

            //! States if the current scope is either a class scope or a scope within a class scope
            bool inside_class_scope() const
            {
                return (_decl_context->class_scope != NULL);
            }

            //! Returns the related class symbol
            /*!
              When inside_class_scope or is_class_scope return true, this function can be used
              to retrieve the related class symbol
              */
            Symbol get_class_of_scope();

            //! States if the current scope is either a block scope or a scope within a block scope
            bool inside_block_scope() const
            {
                return (_decl_context->block_scope != NULL);
            }

            //! States if the current scope is function scope
            /*!
              Function scope is the scope of labels and spans a whole function
              definition
              */
            bool is_function_scope() const
            {
                return _decl_context->current_scope->kind == FUNCTION_SCOPE;
            }

            //! States if the current scope is block scope
            bool is_block_scope() const
            {
                return _decl_context->current_scope->kind == BLOCK_SCOPE;
            }

            //! States if the current scope is class scope
            bool is_class_scope() const
            {
                return _decl_context->current_scope->kind == CLASS_SCOPE;
            }

            //! States if the current scope is namespace scope
            /*!
              Global scope is also a namespace scope
              */
            bool is_namespace_scope() const
            {
                return _decl_context->current_scope->kind == NAMESPACE_SCOPE;
            }

            //! States if the current scope is strictly enclosed into a potential encloser scope
            /*! When both scopes are the same, the method returns false
             */
            bool scope_is_enclosed_by(Scope potential_encloser) const;

            //! States if the current scope is prototype scope
            /*!
              Prototype scope only exists for parameters in function-type
              declarators. In function definitions, parameters are signed in in
              the outermost block scope
              */
            bool is_prototype_scope() const
            {
                return _decl_context->current_scope->kind == PROTOTYPE_SCOPE;
            }

            //! States if the current scope is template scope
            /*!
              Template scope is where template parameters are signed in.
              It is an aside scope which has more priority than any other
              scope and where template parameter names are stored.
              */
            bool is_template_scope() const
            {
                return false;
            }

            //! Debugging function that prints the scope
            /*!
             * The compiler will dump the scope calling the internal
             * function
             */
            void printscope();

            //! Get the symbol associated to this scope
            Symbol get_related_symbol() const;

            //! Get a list of symbols in this scope with name \a str
            /*!
             * \param str The unqualified name looked up
             * \return A list of Symbol that have this name \a str in the current scope
             */
            ObjectList<Symbol> get_symbols_from_name(const std::string& str) const;

            //! Get a list of symbols only in this scope with name \a str
            /*!
             * \param str The unqualified name looked up
             * \return A list of Symbol that have this name \a str in the current scope
             */
            ObjectList<Symbol> get_symbols_from_name_in_scope(const std::string& str) const;

            //! Convenience function where only one symbol is expected
            Symbol get_symbol_from_name(const std::string& str) const;

            //! Convenience function where only one symbol is expected
            Symbol get_symbol_from_name_in_scope(const std::string& str) const;

            //! Returns the global scope of the current compiled file 
            static Scope get_global_scope();

            //! Builds a fake temporal scope not related to any real code
            Scope temporal_scope() const;

            //! Returns the template parameters related to this scope
            template_parameter_list_t* get_template_parameters() const;

            Symbol get_symbol_this() const;

            //! Returns all symbols signed in in this scope
            /*! 
              \param include_hidden If true, hidden symbols (most of 
              the time internal to the compiler and not visible in the code) 
              will be considered as well
             */
            ObjectList<Symbol> get_all_symbols(bool include_hidden);

            //! This function inserts a symbol using its name in the current scope
            /*! Use this function to bring the information of one symbol into another scope
             */
            void insert_symbol(Symbol sym);

            //! Creates an artificial symbol
            /*!
              This function is used to create an artificial symbol. Artificial
              symbols are useful when some information must be stored scope-wise.
              These symbols should have a name that is not language-accessible, this
              is, its name it is not possible to be referenced in the program under
              the syntax of the language (for instance a symbol like ".foo").

              Once the symbol has been created, data can be linked to it using
              Object::set_attribute and retrieved using Object::get_attribute.

              \param artificial_name An arbitrary name for the symbol which should not be language accessible
              \param reuse_symbol If set to true and the symbol already exists
              in this scope, it will not be created twice.
              */
            Symbol new_artificial_symbol(const std::string& artificial_name, bool reuse_symbol=false);

            Symbol new_symbol(const std::string& name);

            //! Convenience function
            static void convert_to_vector(scope_entry_list_t* entry_list, ObjectList<Symbol>& out);

            Scope& operator=(Scope sc);
            bool operator<(Scope sc) const;
            bool operator==(Scope sc) const;
            bool operator!=(Scope sc) const;

            friend class Symbol;
            friend class Type;
            friend class Source;
    };


    
    //! @}
}

#endif // TL_SCOPE_HPP
