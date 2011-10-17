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


#ifndef EXTENSIBLE_SYMBOL_HPP
#define EXTENSIBLE_SYMBOL_HPP

#include "tl-nodecl.hpp"
#include "tl-symbol.hpp"

namespace TL
{  
    /*!
      This class is used to stored extended information of a Symbol.
      It can express:
      - the accessed member of an structure.
      - the set of accessed positions of an array.
    */
    class LIBTL_CLASS ExtensibleSymbol : public TL::Object
    {
        private:
            Nodecl::NodeclBase _n;
            
            //! Returns the symbol contained in a nodecl which is an Extensible Symbol or 
            //! a part of a nodecl which is an Extensible Symbol
            Symbol get_nodecl_symbol(Nodecl::NodeclBase n) const;
            
        public:
            // *** Constructors *** //
           
            ExtensibleSymbol();
           
            //! Constructor building an Extensible Symbol from a valid Symbol.
            /*!
             * Use is valid if the nodecl is be an lvalue.
             * \param n Nodecl containing s Symbol. 
             *          This will be more than a Symbol when the nodecl is a member access or an array access
             */
            ExtensibleSymbol(Nodecl::NodeclBase n);
            
            
            // *** Modifiers *** //
            
            void propagate_constant_values(std::map<Symbol, Nodecl::NodeclBase> values_map);
            
            
            // *** Getters and Setters *** //

            //! Returns the symbol wrapped in the Extended Symbol
            Symbol get_symbol() const;
            
            //! Returns the name of the wrapped symbol.
            std::string get_name() const;
            
            //! Returns the type of the wrapped symbol.
            Type get_type() const;

            //! Returns the nodecl associated with the wrapped symbol.
            Nodecl::NodeclBase get_nodecl() const;           

            //! Returns true when the extensible symbol contains a symbols which do not represents
            //! neither an array access nor a member access, but a symbol.
            bool is_simple_symbol() const;
            
            //! Returns true when the symbol stored is a position in an array
            bool is_array() const;
            
            // *** Overloaded methods *** //
            bool operator==(const ExtensibleSymbol &es) const;
            bool operator<(const ExtensibleSymbol &es) const;
    };
    
    // FIXME This should be changed by an unordered_set in c++0x
    typedef ObjectList<ExtensibleSymbol> ext_sym_set;
}

#endif // EXTENSIBLE_SYMBOL_HPP