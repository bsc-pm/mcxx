/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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



#ifndef TL_DATA_REFERENCE_HPP
#define TL_DATA_REFERENCE_HPP

#include "tl-langconstruct.hpp"
#include "tl-source.hpp"
#include "tl-type.hpp"
#include <sstream>

namespace TL
{
    /*!
      This class is used to handle a subset of expressions which have the following property:

        They express objects or subobjects of a known program entity.

      Only the following expressions can satisfy this property:

           d -> id
                d[e1]
                d[e1:e2]
                d.id
                [e1]...[eN] pd
                *pd
                this            [C++]

      Where 'd' is a data reference and 'pd' a data reference whose type is pointer

      Note that convoluted expressions like '&*d' and '*&d' are accepted and
      assumed to mean 'd'
    */
    class DataReference : public Expression
    {
        private:
            bool _valid;
            Symbol _base_symbol;
            Type _type;
            Source _size;
            Source _addr;
            std::stringstream _warnlog;

            static bool gather_info_data_expr_rec(Expression expr, 
                    Symbol &base_sym, 
                    Source &size, 
                    Source &addr, 
                    Type& type,
                    bool enclosing_is_array,
                    bool & pointer_access_member,
                    std::stringstream& warnlog);

            static bool gather_info_data_expr(Expression &expr, 
                    Symbol &base_sym, 
                    Source &size, 
                    Source &addr,
                    Type &type,
                    std::stringstream& warnlog);

            static Source safe_expression_size(Type type, Scope sc);
        public:
            DataReference(AST_t ast, ScopeLink scope_link);
            //! Constructors of a DataReference
            /*! 
              Use is_valid to know if the expression wrapped as a DataReference
              is eligible as a data reference.
             */
            DataReference(Expression expr);

            //! Copy constructor
            DataReference(const DataReference& data_ref);

            //! Copy assignment operator
            DataReference& operator=(const DataReference& data_ref);

            //! States whether this expression is a data reference
            /*!
              Not all expressions are data references, as defined by this class,
              use this function to check it
              */
            bool is_valid() const;

            //! States whether this expression is a data reference
            /*!
              Not all expressions are data references, as defined by this class,
              use this function to check it
              */
            bool is_valid(std::string& reason) const;
            
            //! Returns the warning log
            /*!
              This is the same message as is_valid(std::string&) stores in its first parameter
              */
            std::string get_warning_log() const;

            //! Gets the base symbol
            /*!
              The base symbol is the entity to which we know we are expressing
              its object or a subobject

              Note for instance that a.b and a.c have the same base symbol, while
              the subobject being named is different.
              */
            Symbol get_base_symbol() const;

            //! Returns a way to obtain an address of the data reference
            /*!
              Since data references express named entities, there is a way to
              get its address. This function returns a Source with an
              expression which evaluates to the address of the data reference.
              */
            Source get_address() const;

            //! Returns the size of the data reference
            /*!
              This function returns an expression which evaluates to the known
              size of a data reference
              */
            Source get_sizeof() const;

            //! Returns a type representing the data covered by the data reference
            /*!
              This function returns a type which represents the data covered
              by the data reference.

              \note The type returned may not be fully valid if it contains arrays
              as this function uses Type::get_array_to(const std::string&)
             */
            Type get_data_type() const;
    };
}

#endif // TL_DATA_REFERENCE_HPP
