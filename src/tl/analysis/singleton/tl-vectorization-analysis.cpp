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

#include "tl-analysis-static-info.hpp"

#include "tl-expression-evolution-visitor.hpp"
#include "tl-suitable-alignment-visitor.hpp"

#include "tl-analysis-utils.hpp"
#include "tl-expression-reduction.hpp"
#include <algorithm>

namespace TL  {
namespace Analysis {

    // ********************************************************************************************* //
    // ************** Class to retrieve SIMD analysis info about one specific nodecl *************** //

    bool NodeclStaticInfo::is_induction_variable_dependent_expression( const Nodecl::NodeclBase& n,
                                                        Node* scope_node ) const
    {
        return contains_induction_variable( n, scope_node ) ||
                var_is_iv_dependent_in_scope( n, scope_node );
    }

    bool NodeclStaticInfo::contains_induction_variable( const Nodecl::NodeclBase& n,
                                                        Node* scope_node ) const
    {
        bool result = false;

        Optimizations::ReduceExpressionVisitor v;
        Nodecl::NodeclBase s = n.shallow_copy( );
        v.walk( s );

        ExpressionEvolutionVisitor iv_v( scope_node->get_induction_variables(), scope_node->get_killed_vars(), scope_node,  NULL, NULL );
        iv_v.walk( s );
        result = iv_v.depends_on_induction_vars( );

        return result;
    }

    bool NodeclStaticInfo::var_is_iv_dependent_in_scope( const Nodecl::NodeclBase& n,
                                                         Node* scope_node ) const
    {
        bool result = false;

        ExpressionEvolutionVisitor iv_v( _induction_variables, _killed, scope_node, NULL, NULL );
        ObjectList<Nodecl::Symbol> syms = Nodecl::Utils::get_all_symbols_occurrences( n );
        for( ObjectList<Nodecl::Symbol>::iterator it = syms.begin( ); it != syms.end( ) && !result; ++it )
        {
            result = iv_v.var_is_iv_dependent_in_scope( *it );
        }

        return result;
    }

    bool NodeclStaticInfo::is_constant_access( const Nodecl::NodeclBase& n ) const
    {
        bool result = true;

        if( n.is<Nodecl::ArraySubscript>( ) )
        {
            Nodecl::ArraySubscript array = n.as<Nodecl::ArraySubscript>( );

            //Check subscripted
            if ( !is_constant( array.get_subscripted( ) ))
            {
                return false;
            }

            // Check subscrips
            Nodecl::List subscript = array.get_subscripts( ).as<Nodecl::List>( );
            Nodecl::List::iterator it = subscript.begin( );
            for( ; it != subscript.end( ); ++it )
            {   // All dimensions must be constant
                if( !is_constant( *it ) )
                {
                    result = false;
                    break;
                }
            }
        }

        return result;
    }

    bool NodeclStaticInfo::is_simd_aligned_access( const Nodecl::NodeclBase& n,
            const std::map<TL::Symbol, int>& aligned_expressions,
            const TL::ObjectList<Nodecl::NodeclBase>& suitable_expressions,
            int unroll_factor, int alignment ) const
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

        SuitableAlignmentVisitor sa_v( _induction_variables, suitable_expressions, unroll_factor, type_size, alignment );

        return sa_v.is_aligned_access( array_subscript, aligned_expressions );
    }

    bool NodeclStaticInfo::is_suitable_expression( const Nodecl::NodeclBase& n,
            const TL::ObjectList<Nodecl::NodeclBase>& suitable_expressions,
            int unroll_factor, int alignment, int& vector_size_module ) const
    {
        bool result = false;
        int type_size = n.get_type().basic_type().get_size();

        SuitableAlignmentVisitor sa_v( _induction_variables, suitable_expressions, unroll_factor, type_size, alignment );
        int subscript_alignment = sa_v.walk( n );

        for(TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = suitable_expressions.begin();
                it != suitable_expressions.end();
                it ++)
        {
            printf("%s ", it->prettyprint().c_str());
        }
        printf("\n");
        // End Remove me!

        vector_size_module = ( ( subscript_alignment == -1 ) ? subscript_alignment :
                                                               subscript_alignment % alignment );
        if( vector_size_module == 0 )
            result = true;

        return result;
    }

    // ************ END class to retrieve SIMD analysis info about one specific nodecl ************* //
    // ********************************************************************************************* //
} 
}
