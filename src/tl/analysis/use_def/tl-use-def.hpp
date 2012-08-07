/*--------------------------------------------------------------------
 ( C) Copyright 2006-2012 Barcelona* Supercomputing Center
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



#ifndef TL_USE_DEF_HPP
#define TL_USE_DEF_HPP

namespace TL {
namespace Analysis {

    //! Class implementing Use-Def Analysis
    class LIBTL_CLASS UseDef
    {
    private:
        int silly_var_1;
        int silly_var_2;

    public:
        //! Constructor
        UseDef( int a, int b );
    };
}
}

#endif      // TL_USE_DEF_HPP


// #ifndef TL_USE_DEF_HPP
// #define TL_USE_DEF_HPP
//
// namespace TL  {
// namespace Analysis {
//
// //     class Node;
//
//     //!This class implements the computation of Use-Definition chains over a PCFG
//     class LIBTL_CLASS UseDef {
//     public:
//         //! Constructor
//         UseDef( );
//
//         //! Sets the variable represented by a symbol as a killed or an upper exposed variable
//         //! depending on @defined attribute
//         /*!
//          * A variable is killed when it is defined or redefined
//          * A variable is upper exposed when it is used before of being killed
//          * \param defined Action performed over the symbol: 1 if defined, 0 if not
//          * \param n Nodecl containing the whole expression about the use/definition
//          */
//         void fill_use_def_sets( Nodecl::NodeclBase n, bool defined );
//
//         //! Wrapper method for #fill_use_def_sets when there is more than one symbol to be analyzed
//         void fill_use_def_sets( Nodecl::List n_l, bool defined );
//
//         //! Returns a list with two elements. The firs is the list of upper exposed variables of the graph node;
//         //! The second is the list of killed variables of the graph node (Used in composite nodes)
//         ObjectList<Utils::ext_sym_set> get_use_def_over_nodes( Node* current );
//
//         //!Propagate the Use-Def information from inner nodes to outer nodes
//         void set_graph_node_use_def( Node* outer_node );
//     };
//
// }
// }
//
// #endif      // TL_USE_DEF_HPP