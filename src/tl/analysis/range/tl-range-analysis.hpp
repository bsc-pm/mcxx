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

#ifndef TL_RANGE_ANALYSIS_HPP
#define TL_RANGE_ANALYSIS_HPP

#include "tl-extensible-graph.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // ******************************** Class implementing range analysis ********************************* //
    
    class LIBTL_CLASS RangeAnalysis
    {
    private:
        ExtensibleGraph* _graph;
        
        void initialize_range_values( Node* current );
        void compute_node_range_analysis( Node* node, bool& changed );
        void compute_range_analysis_rec( Node* current, bool& changed );
        
    public:
        //! Constructor
        RangeAnalysis( ExtensibleGraph* graph );
        
        //! Method computing the Ranges information on the member #graph
        void compute_range_analysis( );
    };

    // ****************************** End class implementing range analysis ******************************* //
    // **************************************************************************************************** //
    
    
    
    // **************************************************************************************************** //
    // *************** Class implementing reaching definitions substitution and propagation *************** //
    
    class LIBTL_CLASS DefinitionsPropagationVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        Utils::ext_sym_map _reaching_definitions;
        
    public:
        // *** Constructor *** //
        DefinitionsPropagationVisitor( Utils::ext_sym_map reaching_defs );
        
        // *** Visiting methods *** //
        Ret visit( const Nodecl::ArraySubscript& n );
        Ret visit( const Nodecl::ClassMemberAccess& n );
        Ret visit( const Nodecl::Symbol& n );
    };
    
    // ************* END class implementing reaching definitions substitution and propagation ************* //
    // **************************************************************************************************** //
    
}
}

#endif      // TL_LIVENESS_HPP