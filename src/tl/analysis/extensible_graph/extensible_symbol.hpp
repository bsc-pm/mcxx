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

#include <set>

#include "tl-langconstruct.hpp"
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
            Symbol _sym;
            IdExpression _member;                // Only when _sym is a struct
            std::set<int> _array_accessed_pos;   // Only when _sym is an array
            
        public:
            // *** Constructors *** //
            
            //! Empty Constructor of an Extensible Symbol.
            /*!
              It builds an Extensible Symbol with non-associated Symbol.
             */
            ExtensibleSymbol();
            
            //! Constructor building an Extensible Symbol from a valid Symbol.
            /*!
              Use is_valid if the Symbol wrapped as an ExtensibleSymbol is eligible as an 
              extensible symbol.
              \param s Symbol which is wrapped in the new ExtensibleSymbol
             */
            ExtensibleSymbol(Symbol s);
            
            
            // *** Getters and Setters *** //
            
            //! Returns the name of the wrapped symbol.
            std::string get_name() const;
            
            //! Returns the type of the wrapped symbol.
            Type get_type() const;
            
            //! Returns the IdExpression of the wrapped member.
            /*!
              If the ExtensibleSymbol does not represents a member, then returns an empty
              IdExpression.
             */
            IdExpression get_member() const;
            
            //! Returns the set of accessed positions of an array.
            /*!
              If the ExtensibleSymbol does not represents an array, then returns an empty set.
            */
            std::set<int> get_accessed_positions() const;
            
            
            // *** Overloaded methods *** //
            bool operator==(const ExtensibleSymbol &cfgs) const;
            bool operator<(const ExtensibleSymbol &cfgs) const;
    };
    
    
    //! Compare class for ExtensibleSymbols
    /*!
      It is used as Comparison Class when a std::set of ExtensibleSymbols is built.
     */
    struct ExtensibleSymbol_comp
    {
        bool operator() (const ExtensibleSymbol& cfgs1, 
                         const ExtensibleSymbol& cfgs2) const
        { 
            return cfgs1 < cfgs2; 
        }
    };
}

#endif // EXTENSIBLE_SYMBOL_HPP