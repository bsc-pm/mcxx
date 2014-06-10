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

#ifndef TL_EXTENDED_SYMBOL_HPP
#define TL_EXTENDED_SYMBOL_HPP

#include "tl-nodecl.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-symbol.hpp"

#include <map>
#include <set>

namespace TL {
namespace Analysis {
namespace Utils {

    //! Returns all nodecl bases of a given nodecl
    ObjectList<Nodecl::NodeclBase> get_nodecls_base( const Nodecl::NodeclBase& n );
    
    //!Returns the nodecl base of a nodecl when it only has one (a nodecl base has always a related symbol)
    Nodecl::NodeclBase get_nodecl_base( const Nodecl::NodeclBase& n );
    
    /*!This class is used to store l-values
        * It can express:
        * - the accessed member of an structure.
        * - the set of accessed positions of an array.
        */
    class LIBTL_CLASS ExtendedSymbol : public TL::Object
    {
        private:
            Nodecl::NodeclBase _n;

        public:
            // *** Constructors *** //

            //! Empty constructor
            ExtendedSymbol( );

            /*! Constructor building an Extensible Symbol from a valid Symbol.
                * Use is valid if the nodecl is be an lvalue.
                * \param n Nodecl containing s Symbol.
                *          This will be more than a Symbol when the nodecl is a member access or an array access
                */
            ExtendedSymbol( const Nodecl::NodeclBase& n );


            // *** Getters and Setters *** //

            //! Returns the symbol wrapped in the Extended Symbol
            ObjectList<Symbol> get_symbols( ) const;

            Symbol get_symbol( ) const;

            //! Returns the nodecl associated with the wrapped symbol.
            Nodecl::NodeclBase get_nodecl( ) const;

            //! Returns true when the extensible symbol contains a symbols which do not represents
            //! neither an array access nor a member access, but a symbol.
            bool is_simple_symbol( ) const;

            //! Returns true when the symbol stored is a position in an array
            bool is_array( ) const;

            // *** Overloaded methods *** //
            ExtendedSymbol& operator=( const ExtendedSymbol &es );
            bool operator==( const ExtendedSymbol &es ) const;
            bool operator<( const ExtendedSymbol &es ) const;
    };

    struct ExtendedSymbol_structural_less {
        bool operator() (const ExtendedSymbol& n1, const ExtendedSymbol& n2) const;
    };
    
    typedef std::set<ExtendedSymbol> ext_sym_set;
    typedef std::set<Nodecl::NodeclBase, Nodecl::Utils::Nodecl_structural_less> nodecl_set;
    typedef std::multimap<ExtendedSymbol, Nodecl::NodeclBase> ext_sym_map;

}
}
}

#endif // TL_EXTENDED_SYMBOL_HPP