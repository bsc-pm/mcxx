/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "tl-nodecl.hpp"
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
    class DataReference : public Nodecl::NodeclBase
    {
        public:
            //! Constructors of a DataReference
            /*! 
              Use is_valid to know if the expression wrapped as a DataReference
              is eligible as a data reference.
             */
            DataReference(Nodecl::NodeclBase expr);

            //! States whether this expression is a data reference
            /*!
              Not all expressions are data references, as defined by this class,
              use this function to check it
              */
            bool is_valid() const;

            //! Returns the warning log
            /*!
              This is the same message as is_valid(std::string&) stores in its first parameter
              */
            std::string get_error_log() const;

            //! Gets the base symbol
            /*!
              The base symbol is the entity to which we know we are expressing
              its object or a subobject

              Note for instance that a.b and a.c have the same base symbol, while
              the subobject being named is different.
              */
            Symbol get_base_symbol() const;

            //! Returns a type representing the data covered by the data reference
            /*!
              This function returns a type which represents the data covered
              by the data reference.

              \note The type returned may not be fully valid if it contains arrays
              as this function uses Type::get_array_to(const std::string&)
             */
            Type get_data_type() const;

            //! Returns the base address of the DataReference
            Nodecl::NodeclBase get_base_address() const;

            //! Returns an expression that designates the size of the DataReference
            Nodecl::NodeclBase get_sizeof() const;

            friend struct DataReferenceVisitor;

            ~DataReference();
        private:
            bool _is_valid;

            TL::Symbol _base_symbol;
            TL::Type _data_type;

            // Error log
            std::string _error_log;

            Nodecl::NodeclBase _base_address;
            Nodecl::NodeclBase _sizeof;
    };
}

#endif // TL_DATA_REFERENCE_HPP
