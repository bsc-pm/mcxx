/*--------------------------------------------------------------------
 ( C) Copyright 2006-2012 Barcelona Supercomputing Center             *
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


#include "tl-cfg-constants-analysis.hpp"
#include "tl-node.hpp"

namespace TL {
namespace Analysis {
namespace Utils {
    
    bool nodecl_is_arithmetic_op( Nodecl::NodeclBase n )
    {
        bool res = false;
        if ( n.is<Nodecl::Add>( ) || n.is<Nodecl::Minus>( )
            || n.is<Nodecl::Mul>( ) || n.is<Nodecl::Div>( )
            || n.is<Nodecl::Mod>( ) || n.is<Nodecl::Plus>( )
            || n.is<Nodecl::Preincrement>( ) || n.is<Nodecl::Postincrement>( ) 
            || n.is<Nodecl::Predecrement>( ) || n.is<Nodecl::Postdecrement>( )
            || nodecl_is_assignment_op( n ) || /* Fortran */ n.is<Nodecl::Power>( ) )
        {
            res = true;
        }
        return res;
    }
    
    bool nodecl_is_comparison_op( Nodecl::NodeclBase n )
    {
        bool res = false;
        if ( n.is<Nodecl::Equal>( ) || n.is<Nodecl::Different>( )
            || n.is<Nodecl::LowerThan>( ) || n.is<Nodecl::GreaterThan>( )
            || n.is<Nodecl::LowerOrEqualThan>( ) || n.is<Nodecl::GreaterOrEqualThan>( ) )
        {
            res = true;
        }
        return res;
    }
    
    bool nodecl_is_logical_op( Nodecl::NodeclBase n )
    {
        bool res = false;
        if ( n.is<Nodecl::LogicalAnd>( ) || n.is<Nodecl::LogicalOr>( ) 
            || n.is<Nodecl::LogicalNot>( ) )
        {
            res = true;
        }
        return res;
    }
    
    bool nodecl_is_bitwise_op( Nodecl::NodeclBase n )
    {
        bool res = false;
        if ( n.is<Nodecl::BitwiseAnd>( ) || n.is<Nodecl::BitwiseOr>( )
            || n.is<Nodecl::BitwiseXor>( ) || n.is<Nodecl::BitwiseNot>( ) 
            || n.is<Nodecl::Shr>( ) || n.is<Nodecl::Shl>( ))
        {
            res = true;
        }
        return res;
    }

    bool nodecl_is_assignment_op ( Nodecl::NodeclBase n )
    {
        bool res = false;
        if ( n.is<Nodecl::Assignment>( ) || n.is<Nodecl::AddAssignment>( )
            || n.is<Nodecl::MinusAssignment>( ) || n.is<Nodecl::DivAssignment>( )
            || n.is<Nodecl::MulAssignment>( ) || n.is<Nodecl::ModAssignment>( )
            || n.is<Nodecl::ShrAssignment>( ) || n.is<Nodecl::ShlAssignment>( )
            || n.is<Nodecl::BitwiseAndAssignment>( ) || n.is<Nodecl::BitwiseOrAssignment>( )
            || n.is<Nodecl::BitwiseXorAssignment>( ) )
        {
            res = true;
        }
        return res;
    }
    
    bool nodecl_is_rvalue( Nodecl::NodeclBase n )
    {
        return !nodecl_is_rvalue( n );
    }
    
    std::string generate_hashed_name(Nodecl::NodeclBase ast)
    {
        std::string result = ast.get_filename();
        
        int line = ast.get_line();
        size_t hash_value = nodecl_hash_table(ast.get_internal_nodecl());
        
        std::stringstream ss;
        ss << line << "_" << hash_value;
        result += "_" + ss.str(); 
        
        return result;
    }
    
}
}
}