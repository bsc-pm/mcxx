/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 B*arcelona Supercomputing Center
 Centro Nacional de Supercomputacion
 
 This file is part of Mercurium C/C++ source-to-source compiler.
 
 See AUTHORS file in the top level directory for information
 regarding developers and contributors.
 
 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 3 of the License, or (at your option ) any later version.
 
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

#ifndef TL_POINTER_SIZE_HPP
#define TL_POINTER_SIZE_HPP

#include "tl-symbol.hpp"
#include "tl-extensible-graph.hpp"

#include <map>

namespace TL {
namespace Analysis {
    
    //! Class implementing Pointer Analysis to calculate the number of elements hidden in a pointer variable
    class LIBTL_CLASS PointerSize 
    {
    private:
        ExtensibleGraph* _pcfg;
        
        void compute_pointer_vars_size_rec(Node* current);
        
    public:
        PointerSize(ExtensibleGraph* pcfg);
        
        void compute_pointer_vars_size();
    };
    
}
}

#endif      // TL_POINTER_SIZE_HPP