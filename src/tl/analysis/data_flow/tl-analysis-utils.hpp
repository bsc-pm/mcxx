/*--------------------------------------------------------------------
 ( C) Copyright 2006-2012 Barcelona Supercomputing Center             **
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

#include <sstream>

#include "tl-extended-symbol.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL {
namespace Analysis {
namespace Utils {    
    
    // ******************************************************* //
    // ************* Nodecl consultant operations ************ //
    
    bool nodecl_is_arithmetic_op( Nodecl::NodeclBase n );
    bool nodecl_is_comparison_op( Nodecl::NodeclBase n );
    bool nodecl_is_logical_op( Nodecl::NodeclBase n );
    bool nodecl_is_bitwise_op( Nodecl::NodeclBase n );
    bool nodecl_is_assignment_op( Nodecl::NodeclBase n );
    bool nodecl_is_lvalue(Nodecl::NodeclBase n);
    bool nodecl_is_rvalue(Nodecl::NodeclBase n);
    
    // *********** End nodecl consultant operations ********** //
    // ******************************************************* //
    
    
    // ******************************************************* //
    // ******** Common methods with analysis purposes ******** //
    
    //! Returns a hashed string depending on \ast
    std::string generate_hashed_name(Nodecl::NodeclBase ast);
    
    // ****** End common methods with analysis purposes ****** //
    // ******************************************************* //
    
    
    //!Visitor to get the l-values contained in a given nodecl
    class LIBTL_CLASS SymbolVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        ObjectList<ExtendedSymbol> _ext_syms;   
        
    public:
        //! Getters and setters
        ObjectList<ExtendedSymbol> get_symbols( );
        
        //! Visiting methods
        Ret unhandled_node( const Nodecl::NodeclBase& n );
        Ret visit( const Nodecl::Symbol& n );
        Ret visit( const Nodecl::ArraySubscript& n );
    };
}
}
}