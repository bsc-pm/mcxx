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



#ifndef TL_SYMBOL_HPP
#define TL_SYMBOL_HPP

#include "tl-common.hpp"
#include <string>
#include <vector>
#include <sstream>
#include "tl-object.hpp"
#include "tl-nodecl-fwd.hpp"
#include "tl-symbol-fwd.hpp"
#include "tl-scope-fwd.hpp"
#include "tl-type-fwd.hpp"
#include "tl-objectlist.hpp"
#include "cxx-gccsupport-decls.h"
#include "cxx-scope.h"

namespace TL
{
    //! \addtogroup Wrap 
    //! @{
    
    //! This class wraps a symbolic entity in the compiler
    class LIBTL_CLASS Symbol : public Object
    {
        public:
            //! Returns an invalid symbol
            static const Symbol invalid();

            //! States whether this is an invalid symbol
            bool is_invalid() const;

            //! States whether this is a valid symbol
            bool is_valid() const;

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
            /*!
             * \remark This function will give bogus names to templates parameters. Use get_qualified_name(Scope)
             * instead.
             */
            std::string get_qualified_name(bool without_template_id = false) const;
            
            //! Returns a fully qualified name
            /*!
             * \param sc Scope used to lookup template parameter names
             */
            std::string get_qualified_name(Scope sc, bool without_template_id = false) const;

            //! Returns the part of the qualified name that involves classes
            std::string get_class_qualification(bool without_template_id = false) const;
            
            //! Returns the part of the qualified name that involves classes
            /*!
             * \param sc Scope used to lookup template parameters names
             */
            std::string get_class_qualification(Scope sc, bool without_template_id = false) const;

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

            bool operator<(Symbol s) const;
            bool operator==(Symbol s) const;
            bool operator!=(Symbol s) const;
            Symbol& operator=(Symbol s);

            //! States whether this symbol is a variable
            bool is_variable() const;
            //! States whether this symbol is a typedef
            bool is_typedef() const;
            //! States whether this symbol is a class
            bool is_class() const;
            //! States whether this symbol is an enum name
            bool is_enum() const;
            //! States whether this symbol is an enumerator name
            bool is_enumerator() const;
            //! States whether this symbol is template name
            bool is_template() const;
            //! States whether this symbol is a function
            bool is_function() const;
            //! States whether this symbol is a template function
            bool is_template_function_name() const;
            //! States whether this symbol is an anonymous union
            bool is_anonymous_union() const;
            //! States that this symbol is the injected class name
            bool is_injected_class_name() const;
            
            //! States whether this symbol is a parameter of a function
            bool is_parameter() const;
            //! Returns the position of this parameter
            int get_parameter_position() const;

            //! States whether this symbol is a template parameter
            bool is_template_parameter() const;

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

            //! Returns the access specifier of a member or base class
            access_specifier_t get_access_specifier();

            //! States whether this symbol has been initialized
            bool has_initialization() const;
            //! Returns the initialization tree
            Nodecl::NodeclBase get_initialization() const;

            //! States whether this symbol is static
            bool is_static() const;
            //! States whether this symbol is register
            bool is_register() const;
            
            //! States whether this symbol is __thread
            bool is_thread() const;

            //! States if this member is a bitfield
            bool is_bitfield() const;
            
            //! Returns the size of the bitfield
            Nodecl::NodeclBase get_bitfield_size() const;

            //! States whether the symbol is user declared
            /*!
             * \note This only applies to member functions
             */
            bool is_user_declared() const;

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

            //! States whether this member function is a destructor
            bool is_destructor() const;

            //! States whether this member function is a constructor
            bool is_constructor() const;

            //! States whether this member function is a constructor flagged as explicit
            bool is_explicit_constructor() const;

            //! States whether symbol exists just because was mentioned in a friend declaration
            /*!
             * This symbol has not been technically declared by the user but the compiler
             * created it because it appeared in a friend declaration
             */
            bool is_friend_declared() const;

            //! States whether this function was defined with no exception-specifier
            bool function_throws_any_exception() const;

            //! Returns the thrown exceptions
            /*!
             * \note This function returns empty if function_throws_any_exception is true
             * but it may return empty if it function_throws_any_exception returns false.
             * That latter case means 'throw()'
             */
            ObjectList<TL::Type> get_thrown_exceptions() const;

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

            //! Special symbol for using A::x inside classes
            bool is_using_symbol() const;

            //! States whether the symbol is actually a builtin of the compiler
            bool is_builtin() const;

            //! States whether the symbol is actually a Fortran intrinsic
            /*!
              This is actually an alias of is_builtin
              */
            bool is_intrinsic() const;

            //! Returns the definition tree
            /*! 
              This is only valid for functions and class symbols.  It will be
              an invalid tree if the function has not been defined or if it is
              an incomplete class type symbol name

              For functions it returns the function definition. For classes the
              class specifier.
             */
            Nodecl::NodeclBase get_definition_tree() const;

            // States whether the symbol is defined 
            /*! This function might not make sense for all kind of symbols
             */
            bool is_defined() const;

            //! Do not use unless told to do so
            scope_entry_t* get_internal_symbol() const
            {
                return _symbol;
            }

            //! Internal usage
            bool not_to_be_printed() const;

            //! Is a COMMON name
            /*! 
              States whether this symbol is the symbol of a Fortran COMMON name. 

              This function is only meaningful in Fortran. In C/C++ it always returns false
              */
            bool is_common() const;

            //! This symbol is ALLOCATABLE
            /*! 
              States whether this symbol has the ALLOCATABLE attribute set
              This function is only meaningful in Fortran. In C/C++ it always returns false
              */
            bool is_allocatable() const;

            //! This symbol is in a common
            /*! 
              States whether this symbol has been defined to be in a COMMON.
              Use get_common to retrieve the COMMON symbol containing this symbol

              This function is only meaningful in Fortran. In C/C++ it always returns false
              */
            bool is_in_common() const;

            //! This symbol is in a namelist
            /*! 
              States whether this symbol has been defined to be in a NAMELIST
              Use get_common to retrieve the NAMELIST symbol containing this symbol

              This function is only meaningful in Fortran. In C/C++ it always returns false
              */
            bool is_in_namelist() const;

            //! This symbol is OPTIONAL
            /*! 
              States whether this dummy argument is an OPTIONAL dummy argument

              This function is only meaningful in Fortran. In C/C++ it always returns false
              */
            bool is_optional() const;

            //! This symbol is TARGET
            /*! 
              States whether this symbol has the TARGET attribute

              This function is only meaningful in Fortran. In C/C++ it always returns false
              */
            bool is_target() const;

            //! This symbol is VALUE
            /*! 
              States whether this dummy argument has the VALUE attribute

              This function is only meaningful in Fortran. In C/C++ it always returns false
              */
            bool is_value() const;

            //! This symbol is ELEMENTAL
            /*! 
              States whether this dummy argument has the ELEMENTAL attribute

              This function is only meaningful in Fortran. In C/C++ it always returns false
              */
            bool is_elemental() const;

            //! This symbol is RECURSIVE
            /*! 
              States whether this dummy argument has the RECURSIVE attribute

              This function is only meaningful in Fortran. In C/C++ it always returns false
              */
            bool is_recursive() const;

            //! This symbol is RESULT
            /*! 
              States whether this symbol is a RESULT attribute

              This function is only meaningful in Fortran. In C/C++ it always returns false
              */
            bool is_result() const;

            //! This symbol is a generic-specifier name
            /*! 
              States whether this symbol is a generic-specifier name

              This function is only meaningful in Fortran. In C/C++ it always returns false
              */
            bool is_generic_specifier() const;

            //! Returns the symbols related to this one
            /*!
             * The exact set returned depends on the kind of the symbol as kept by the frontend
             */
            ObjectList<TL::Symbol> get_related_symbols() const;

            //! Returns the gcc attributes of this symbol
            ObjectList<GCCAttribute> get_gcc_attributes() const;

            //! __asm__ specifier
            /*!
             * The tree related to the __asm__ specifier. This is a GCC extension
             */
            Nodecl::NodeclBase get_asm_specification() const;
        private:
            scope_entry_t* _symbol;
    };

    class LIBTL_CLASS GCCAttribute
    {
        private:
            gather_gcc_attribute_t _attr;
        public:
            GCCAttribute(gather_gcc_attribute_t attr) : _attr(attr) { }

            std::string get_attribute_name() const;
            Nodecl::List get_expression_list() const;
    };
    
    //! @}
}

#endif // TL_SYMBOL_HPP
