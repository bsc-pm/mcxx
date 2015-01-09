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

#include "tl-vectorization-analysis-internals.hpp"

#include "tl-expression-evolution-visitor.hpp"
#include "tl-suitable-alignment-visitor.hpp"
#include "tl-analysis-internals.hpp"

namespace TL
{
namespace Vectorization
{
    bool is_adjacent_access_internal(
            Analysis::Node* const scope_node,
            Analysis::Node* const n_node,
            const Nodecl::NodeclBase& n,
            Analysis::ExtensibleGraph* const pcfg,
            std::set<Nodecl::NodeclBase> visited_nodes)
    {
        bool result = false;

        if(n.is<Nodecl::ArraySubscript>())
        {
            result = true;

            Nodecl::List subscript = n.as<Nodecl::ArraySubscript>()
                .get_subscripts().as<Nodecl::List>();
            Nodecl::List::iterator it = subscript.begin();

            for(; it != subscript.end() - 1 && result; ++it )
            {   
                // All dimensions but the less significant must be constant
                if(!is_uniform_internal(scope_node, n_node, *it, pcfg))
                {
                    result = false;
                }
            }
            // Esto de aquí arriba no lo hace ya el is_adjacent_access de abajo?

            if(result)
            {   
                Nodecl::NodeclBase last_dim_n = *it;

                // The less significant dimension must be accessed by an (+/-)c +/- IV, where c is a constant
                // If the subscript is another ArraySubscript, then it is not adjacent
                if (last_dim_n.is<Nodecl::ArraySubscript>())
                {
                    result = false;
                }
                else
                {
                    ExpressionEvolutionVisitor expr_evolution_info(
                            scope_node, n_node, pcfg, visited_nodes);
                    expr_evolution_info.walk(last_dim_n);
                    result = expr_evolution_info.is_adjacent_access( );
                }
            }
        }

        return result;
    }

    bool is_simd_aligned_access_internal(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n,
            const map_tlsym_int_t& aligned_expressions,
            const objlist_nodecl_t& suitable_expressions,
            int unroll_factor, int alignment,
            int& alignment_module,
            VectorizationAnalysisInterface* analysis)
    {
        if( !n.is<Nodecl::ArraySubscript>( ) )
        {
            std::cerr << "warning: returning false for is_simd_aligned_access when asking for nodecl '"
                      << n.prettyprint( ) << "' which is not an array subscript" << std::endl;
            return false;
        }

        Nodecl::ArraySubscript array_subscript = n.as<Nodecl::ArraySubscript>( );
        Nodecl::NodeclBase subscripted = array_subscript.get_subscripted( );

        int type_size = subscripted.get_type().basic_type().get_size();

        SuitableAlignmentVisitor sa_v( scope, suitable_expressions,
                unroll_factor, type_size, alignment, analysis );

        return sa_v.is_aligned_access(
                array_subscript, aligned_expressions, alignment_module);
    }

    bool is_suitable_expression_internal(
            const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n,
            const objlist_nodecl_t& suitable_expressions,
            int unroll_factor, int alignment, int& vector_size_module,
            VectorizationAnalysisInterface* analysis)
    {
        bool result = false;
        int type_size = n.get_type().basic_type().get_size();
        
        SuitableAlignmentVisitor sa_v( scope, suitable_expressions,
                unroll_factor, type_size, alignment, analysis);
        int subscript_alignment = sa_v.walk( n );

        vector_size_module = ( ( subscript_alignment == -1 ) ? subscript_alignment :
                                                               (subscript_alignment % alignment)/type_size );

        if( vector_size_module == 0 )
            result = true;

        return result;
    }
}
}
