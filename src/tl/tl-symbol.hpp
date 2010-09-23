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

#ifndef TL_SYMBOL_HPP
#define TL_SYMBOL_HPP

#include "tl-common.hpp"
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
    
    //! \addtogroup Wrap 
    //! @{
    
    //! This class wraps a symbolic entity in the compiler
    class LIBTL_CLASS Symbol : public Object
    {
        public:
            scope_entry_t* _symbol;

        public:
            //! Returns an invalid symbol
            static const Symbol invalid();

            //! States whether this is an invalid symbol
            bool is_invalid() const;

            //! States whether this is a valid symbol
            bool is_valid() const;

            //! Schema of this extensible object
            static Schema schema;

            Symbol()
                : _symbol(NULL)
            {
            }

            Symbol(scope_entry_t* symbol)
                : _symbol(symbol)
            {
            }

            //! Implements the access to extended attributes of a Symbol
            virtual tl_type_t* get_extended_attribute(const std::string& name) const;
            //! Implements the access to extended attributes of a Symbol
            virtual bool set_extended_attribute(const std::string&, const tl_type_t &data);

            //! Constructs a Symbol after a reference to Object
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

            //! Gets the type related to this symbol
            Type get_type() const;
            //! Gets the unqualified name of the symbol
            std::string get_name() const;

            //! Returns a fully qualified name
            /*
             * \remark This function will give bogus names to templates parameters. Use get_qualified_name(Scope)
             * instead.
             */
            std::string get_qualified_name(bool without_template_id = 0) const;
            //! Returns a fully qualified name
            /*
             * \param sc Scope used to lookup template parameter names
             */
            std::string get_qualified_name(Scope sc, bool without_template_id = 0) const;

            //! Gets the scope where this symbol is defined
            Scope get_scope() const;

            //! Returns the location of the symbol
            std::string get_locus() const;

            //! Returns the filename where the symbol was declared
            std::string get_filename() const;

            //! Returns the line where the symbol was declared
            int get_line() const;

            virtual ~Symbol()
            {
            }

            //! States whether is a symbol
            virtual bool is_symbol() const
            {
                return true;
            }

            bool operator<(Symbol s) const;
            bool operator==(Symbol s) const;
            bool operator!=(Symbol s) const;
            Symbol& operator=(Symbol s);

            //! States whether this symbol is a variable
            bool is_variable() const;
            //! States whether this symbol is a typedef
            bool is_typedef() const;
            //! States whether this symbol is a name of a type
            bool is_typename() const;
            //! States whether this symbol is a function
            bool is_function() const;
            //! States whether this symbol is a template function
            bool is_template_function_name() const;
            //! States whether this symbol is a parameter of a function
            bool is_parameter() const;
            //! Returns the position of this parameter
            int get_parameter_position() const;

            //! States whether what was named is a dependent entity
            bool is_dependent_entity() const;

            //! States if this is a member entity
            bool is_member() const;

            //! States if this symbol is artificial
            /*!
              See Scope::new_artificial_symbol for more information on
              artificial symbols
              */
            bool is_artificial() const;

            //! Returns the class to which this member belongs
            Type get_class_type() const;

            //! Returns the point where this symbol was declared
            AST_t get_point_of_declaration() const;

            //! Returns the point where this symbol was declared
            /*!
              If is_defined returns false this function will return an invalid tree.
              If is_defined returns ture, this function might or might not return a valid tree.
              If a tree is returned it always will be the enclosing declaration where the
              symbol was defined, so it can be safely wrapped into a Declaration
             */
            AST_t get_point_of_definition() const;

            //! States whether this symbol has been initialized
            bool has_initialization() const;
            //! Returns the initialization tree
            AST_t get_initialization() const;

            //! States whether this symbol is static
            bool is_static() const;
            //! States whether this symbol is register
            bool is_register() const;

            //! States whether this symbol is extern
            /*!
            * \bug : This only holds if the 'extern' qualifier was given
            * in the declaration of the symbol but global symbols
            * without it are 'extern' too. Using 'is_static' is better
            * till this gets fixed
            */
            bool is_extern() const;

            //! States whether this symbol is mutable
            bool is_mutable() const;
            // States whether this template is exported
            /*
             * \bug The compiler does not honour this flag (it will always return false)
             */
            bool is_exported_template() const;

            //! States whether this function was defined inline
            bool is_inline() const;

            //! States whether this member function was defined as virtual
            bool is_virtual() const;

            //! States whether this member function is a pure virtual function
            bool is_pure() const;

            //! States whether this member function is a conversion function
            bool is_conversion_function() const;

            //! States whether this member function is a constructor
            bool is_constructor() const;

            //! States whether this member function is a constructor flagged as explicit
            bool is_explicit_constructor() const;

            //! States whether the symbol has been defined in namespace scope
            bool has_namespace_scope() const;
            //! States whether the symbol has been defined in block scope
            bool has_block_scope() const;
            //! This is an alias for has_block_scope
            bool has_local_scope() const; 
            //! States whether the symbol has been defined in class scope
            /*!
             * This is roughly equivalent to a member symbol
             */
            bool has_class_scope() const;
            //! States whether this symbol has template scope
            /*!
             * Only template parameters should have template scope
             */
            bool has_template_scope() const;
            //! States whether this symbol has prototype scope
            /*!
             * Named parameter declarations of functional declarators have
             * prototype scope only if the declaration itself is not a function
             * definition.
             */
            bool has_prototype_scope() const;

            //! States whether the symbol is actually a builtin of the compiler
            bool is_builtin() const;

            //! States whether the symbol has been create because of a typedef
            //against an unnamed struct/enum
            /*!
             * @code
             * typedef struct { int b; } A;
             * @endcode
             *
             * Symbol 'A' will be created as a class-name (and not as a
             * typedef-name) because of typedef against the unnamed struct
             */
            bool is_created_after_typedef() const;

            //! Returns the definition tree
            /*! 
              This is only valid for functions and class symbols.  It will be
              an invalid tree if the function has not been defined or if it is
              an incomplete class type symbol name

              For functions it returns the function definition. For classes the
              class specifier.
             */
            AST_t get_definition_tree() const;

            //! States whether the symbol has a given gcc attribute
            bool has_gcc_attribute(const std::string &str) const;
            
            //! Returns the associated argument of a gcc attribute
            AST_t get_argument_of_gcc_attribute(const std::string &str) const;

            // States whether the symbol is defined 
            /*! This function might not make sense for all kind of symbols
             */
            bool is_defined() const;

            //! Do not use unless told to do so
            scope_entry_t* get_internal_symbol() const
            {
                return _symbol;
            }
    };
    
    //! @}
}

#endif // TL_SYMBOL_HPP
