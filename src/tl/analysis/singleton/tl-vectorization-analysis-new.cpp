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

#include "tl-vectorization-analysis-new.hpp"

#include "tl-expression-evolution-visitor.hpp"
#include "tl-suitable-alignment-visitor.hpp"

namespace TL  
{
namespace Analysis
{
    VectorizationAnalysis::VectorizationAnalysis(const Nodecl::NodeclBase& n, WhichAnalysis analysis_mask,
            WhereAnalysis nested_analysis_mask, int nesting_level)
        : AnalysisInterface(n, analysis_mask, nested_analysis_mask, nesting_level)
    {
    }

    VectorizationAnalysis::~VectorizationAnalysis()
    {
    }

    bool VectorizationAnalysis::is_adjacent_access(const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n) 
    {
        bool result = false;

        // Retrieve PCFG
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(scope);
        ERROR_CONDITION(pcfg==NULL, "No PCFG found for nodecl %s\n",
                n.prettyprint().c_str());

        // Retrieve nodes from PCFG
        Node* n_node = pcfg->find_nodecl_pointer(n);
        ERROR_CONDITION(n_node==NULL, "No PCFG node found for nodecl '%s:%s'. \n", 
                n.get_locus_str().c_str(), n.prettyprint().c_str());
        Node* scope_node = pcfg->find_nodecl_pointer(scope);
        ERROR_CONDITION(scope_node==NULL, "No PCFG node found for nodecl '%s:%s'. \n",
                scope.get_locus_str().c_str(), scope.prettyprint().c_str());


        if(n.is<Nodecl::ArraySubscript>())
        {
            result = true;

            Nodecl::List subscript = n.as<Nodecl::ArraySubscript>()
                .get_subscripts().as<Nodecl::List>();
            Nodecl::List::iterator it = subscript.begin();

            for(; it != subscript.end() - 1 && result; ++it )
            {   
                // All dimensions but the less significant must be constant
                if(!variable_is_constant_at_statement(scope_node, n_node, n, pcfg) )
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
                    ExpressionEvolutionVisitor iv_v(scope_node, n_node, pcfg);
                    iv_v.walk(last_dim_n);
                    result = iv_v.is_adjacent_access( );
                }
            }
        }

        return result;
    }

    bool VectorizationAnalysis::is_simd_aligned_access(const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n,
            const std::map<TL::Symbol, int>& aligned_expressions,
            const TL::ObjectList<Nodecl::NodeclBase>& suitable_expressions,
            int unroll_factor, int alignment) 
    {
        // Retrieve PCFG
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(scope);
        ERROR_CONDITION(pcfg==NULL, "No PCFG found for nodecl %s\n",
                n.prettyprint().c_str());

        // Retrieve nodes from PCFG
        Node* n_node = pcfg->find_nodecl_pointer(n);
        ERROR_CONDITION(n_node==NULL, "No PCFG node found for nodecl '%s:%s'. \n", 
                n.get_locus_str().c_str(), n.prettyprint().c_str());
        Node* scope_node = pcfg->find_nodecl_pointer(scope);
        ERROR_CONDITION(scope_node==NULL, "No PCFG node found for nodecl '%s:%s'. \n",
                scope.get_locus_str().c_str(), scope.prettyprint().c_str());

        if( !n.is<Nodecl::ArraySubscript>( ) )
        {
            std::cerr << "warning: returning false for is_simd_aligned_access when asking for nodecl '"
                      << n.prettyprint( ) << "' which is not an array subscript" << std::endl;
            return false;
        }

        Nodecl::ArraySubscript array_subscript = n.as<Nodecl::ArraySubscript>( );

        Nodecl::NodeclBase subscripted = array_subscript.get_subscripted( );
        int type_size = subscripted.get_type().basic_type().get_size();

        SuitableAlignmentVisitor sa_v( scope_node, suitable_expressions,
                unroll_factor, type_size, alignment );

        return sa_v.is_aligned_access( array_subscript, aligned_expressions );
    }

    bool VectorizationAnalysis::is_suitable_expression(const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n,
            const TL::ObjectList<Nodecl::NodeclBase>& suitable_expressions,
            int unroll_factor, int alignment, int& vector_size_module)
    {
        bool result = false;

        // Retrieve PCFG
        ExtensibleGraph* pcfg = retrieve_pcfg_from_func(scope);
        ERROR_CONDITION(pcfg==NULL, "No PCFG found for nodecl %s\n",
                n.prettyprint().c_str());

        // Retrieve nodes from PCFG
        // n_node is not necessary in this query
        Node* scope_node = pcfg->find_nodecl_pointer(scope);
        ERROR_CONDITION(scope_node==NULL, "No PCFG node found for nodecl '%s:%s'. \n",
                scope.get_locus_str().c_str(), scope.prettyprint().c_str());


        int type_size = n.get_type().basic_type().get_size();

        SuitableAlignmentVisitor sa_v( scope_node, suitable_expressions, unroll_factor, type_size, alignment );
        int subscript_alignment = sa_v.walk( n );

        vector_size_module = ( ( subscript_alignment == -1 ) ? subscript_alignment :
                                                               subscript_alignment % alignment );
        if( vector_size_module == 0 )
            result = true;

        return result;
    }
}
}
