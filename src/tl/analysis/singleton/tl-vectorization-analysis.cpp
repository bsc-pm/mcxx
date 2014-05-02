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

        ExpressionEvolutionVisitor iv_v( scope_node,  NULL, NULL );
        iv_v.walk( n );
        result = iv_v.depends_on_induction_vars( );

        return result;
    }

    bool NodeclStaticInfo::var_is_iv_dependent_in_scope( const Nodecl::NodeclBase& n,
                                                         Node* scope_node ) const
    {
        bool result = false;

        ExpressionEvolutionVisitor iv_v( scope_node, NULL, NULL );
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

    // ************ END class to retrieve SIMD analysis info about one specific nodecl ************* //
    // ********************************************************************************************* //
}
}
