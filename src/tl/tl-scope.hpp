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
    class TemplateParameter;
    
    //! \addtogroup Wrap 
    //! @{

    //! Class that represents a context
    /*!
     * This class used to hold a scope_t* but now it holds a decl_context_t.
     * This allows greater flexibility.
     */
    class LIBTL_CLASS Scope : public Object
    {
        private:
            bool _valid;
            decl_context_t _decl_context;
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
                    return true;
                return _decl_context.current_scope != NULL;
            }

            //! States if the current scope is either a class scope or a scope within a class scope
            bool inside_class_scope() const
            {
                return (_decl_context.class_scope != NULL);
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
                return (_decl_context.block_scope != NULL);
            }

            //! States if the current scope is function scope
            /*!
              Function scope is the scope of labels and spans a whole function
              definition
              */
            bool is_function_scope() const
            {
                return _decl_context.current_scope->kind == FUNCTION_SCOPE;
            }

            //! States if the current scope is block scope
            bool is_block_scope() const
            {
                return _decl_context.current_scope->kind == BLOCK_SCOPE;
            }

            //! States if the current scope is class scope
            bool is_class_scope() const
            {
                return _decl_context.current_scope->kind == CLASS_SCOPE;
            }

            //! States if the current scope is namespace scope
            /*!
              Global scope is also a namespace scope
              */
            bool is_namespace_scope() const
            {
                return _decl_context.current_scope->kind == NAMESPACE_SCOPE;
            }

            //! States if the current scope is lexically contained in \a sc
            bool is_contained_in(Scope sc) const;

            //! States if the current scope is prototype scope
            /*!
              Prototype scope only exists for parameters in function-type
              declarators. In function definitions, parameters are signed in in
              the outermost block scope
              */
            bool is_prototype_scope() const
            {
                return _decl_context.current_scope->kind == PROTOTYPE_SCOPE;
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

            //! Get the symbol associated to this scope
            Symbol get_related_symbol() const;

            //! Get a list of symbols in this scope with name \a str
            /*!
             * \param str The unqualified name looked up
             * \return A list of Symbol that have this name \a str in the current scope
             */
            ObjectList<Symbol> get_symbols_from_name(const std::string& str) const;

            //! Convenience function where only one symbol is expected
            Symbol get_symbol_from_name(const std::string& str) const;
            
            //! Builds a fake temporal scope not related to any real code
            Scope temporal_scope() const;

            //! Returns the template parameters related to this scope
            ObjectList<TemplateParameter> get_template_parameters() const;

            //! Returns all symbols signed in in this scope
            /*! 
              \param include_hidden If true, hidden symbols (most of 
              the time internal to the compiler and not visible in the code) 
              will be considered as well
             */
            ObjectList<Symbol> get_all_symbols(bool include_hidden);

            //! This function performs a cascaded lookup
            /*!
              Normal C/C++ lookup, when looking up an unqualified name,
              stops when a name (or names) are found in the current
              or enclosing stops. A cascaded lookup, which is not
              what C/C++ does, continues to lookup enclosing scopes.

              This is useful, for instance, when you have names
              that are somehow inherited through enclosing scopes
              but would be hidden because of symbols of the current
              scope if a normal lookup was performed.

              Note that qualified names are not eligible for a cascaded
              lookup. This is why this function simply receives
              a string.

              The order of the symbols goes from the innermost scope to the
              outermost LEXICAL scope. So checking each symbol sequentially
              will give you the expected semantics of checking from inner to
              outer.
              */
            ObjectList<Symbol> cascade_lookup(const std::string& name, 
                    const std::string& filename, int line);

            //! This function inserts a symbol using its name in the current scope
            /*! Use this function to bring the information of one symbol into another scope
             */
            void insert_symbol(Symbol sym);

            //! Creates an artificial symbol
            /*!
              This function is used to create an artifical symbol. Artificial
              symbols are useful when some information must be stored scope-wise.
              These symbols should have a name that is not language-accesible, this
              is, its name it is not possible to be referenced in the program under
              the syntax of the language (for instance a symbol like ".foo").

              Once the symbol has been created, data can be linked to it using
              Object::set_attribute and retrieved using Object::get_attribute.

              \param artificial_name An arbitrary name for the symbol which should not be language accessible
              \param reuse_symbol If set to true and the symbol already exists
              in this scope, it will not be created twice.
              */
            Symbol new_artificial_symbol(const std::string& artificial_name, bool reuse_symbol=false);

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
