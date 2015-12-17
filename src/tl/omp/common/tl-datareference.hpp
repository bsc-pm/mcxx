/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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
#include "tl-modules.hpp"
#include "cxx-diagnostic.h"

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
            DataReference() : _is_valid(false), _diagnostic_context(NULL) { }

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

            //! States whether this expression refers to an assumed size
            /*!
             This is only relevant for Fortran
             */
            bool is_assumed_size_array() const;

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
             */
            Type get_data_type() const;

            //! Returns the base address of the DataReference
            Nodecl::NodeclBase get_base_address() const;

            //! Returns the base address of the DataReference as an integer expression
            /*
               This is needed in Fortran
             */
            Nodecl::NodeclBase get_base_address_as_integer() const;

            //! Returns an expression denoting the base address of the base symbol of the data reference
            Nodecl::NodeclBase get_address_of_symbol() const;

            //! Returns an expression that designates the size of the DataReference
            Nodecl::NodeclBase get_sizeof() const;

            //! Returns an expression that computes the offset of a dependence in bytes
            Nodecl::NodeclBase get_offsetof_dependence() const;

            //! Returns an expression that computes the offset in bytes
            Nodecl::NodeclBase get_offsetof_copy() const;

            //! Returns an expression that computes the offset in bytes
            /*!
             * The extra parameters are a reference expression used to compute
             * runtime bounded parameters in Fortran
             */
            Nodecl::NodeclBase get_offsetof_copy(Nodecl::NodeclBase reference, TL::Scope sc) const;

            //! States if this is a multireference data-reference
            bool is_multireference() const;
            typedef std::pair<TL::Symbol, Nodecl::NodeclBase> MultiRefIterator;
            TL::ObjectList<MultiRefIterator> multireferences() const;

            friend struct DataReferenceVisitor;

            void commit_diagnostic();

            ~DataReference();

            DataReference(const DataReference& data_ref)
                : Nodecl::NodeclBase(data_ref),
                _base_address(data_ref._base_address),
                _is_valid(data_ref._is_valid),
                _is_assumed_size(data_ref._is_assumed_size),
                _base_symbol(data_ref._base_symbol),
                _data_type(data_ref._data_type),
                _iterators(data_ref._iterators),
                _diagnostic_context(NULL) // diagnostics cannot be copied
            {
            }

            DataReference& operator=(const DataReference& data_ref)
            {
                if (this != &data_ref)
                {
                    Nodecl::NodeclBase::operator=(data_ref);
                    _base_address = data_ref._base_address;
                    _is_valid = data_ref._is_valid;
                    _is_assumed_size = data_ref._is_assumed_size;
                    _base_symbol = data_ref._base_symbol;
                    _data_type = data_ref._data_type;
                    _iterators = data_ref._iterators;
                    _diagnostic_context = NULL; // diagnostics cannot be copied
                }
                return *this;
            }

            void module_write(ModuleWriter& mw);
            void module_read(ModuleReader& mw);
        private:

            Nodecl::NodeclBase compute_sizeof_of_type(TL::Type relevant_type, bool ignore_regions = false) const;

            Nodecl::NodeclBase compute_offsetof_array_subscript(Nodecl::NodeclBase expr, TL::Scope scope) const;

            Nodecl::NodeclBase compute_offsetof_dependence(
                    Nodecl::NodeclBase expr,
                    TL::Scope sc) const;

            Nodecl::NodeclBase compute_offsetof_copy(
                    Nodecl::NodeclBase expr,
                    TL::Scope sc) const;

            Nodecl::NodeclBase get_address_of_symbol_helper(Nodecl::NodeclBase expr, bool reference) const;

            /* data members */
            Nodecl::NodeclBase _base_address;

            bool _is_valid;
            bool _is_assumed_size;

            TL::Symbol _base_symbol;
            TL::Type _data_type;

            TL::ObjectList<MultiRefIterator> _iterators;

            // Error log
            diagnostic_context_t* _diagnostic_context;
    };
}

#endif // TL_DATA_REFERENCE_HPP
