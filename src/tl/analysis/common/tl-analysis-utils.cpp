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

#include <cassert>
#include <sstream>

#include "tl-analysis-utils.hpp"
#include "tl-nodecl.hpp"

namespace TL {
namespace Analysis {
namespace Utils {

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

    // *********************************************************************** //
    // ****************** Assigned Extended Symbols Visitor ****************** //

    AssignedExtSymVisitor::AssignedExtSymVisitor( )
        : _assigned_ext_syms( ), _is_lhs( false )
    {}

    ObjectList<ExtendedSymbol> AssignedExtSymVisitor::get_assigned_ext_syms()
    {
        return _assigned_ext_syms;
    }

    void AssignedExtSymVisitor::visit_assignment( Nodecl::NodeclBase ass_lhs, Nodecl::NodeclBase ass_rhs )
    {
        //! Keep record of the value of \_lhs for nested assignments
        bool is_lhs = _is_lhs;

        // Traverse lhs
        _is_lhs = true;
        walk( ass_lhs );
        // Traverse rhs
        _is_lhs = is_lhs;
        walk( ass_rhs );
    }

    void AssignedExtSymVisitor::visit_xx_crements( Nodecl::NodeclBase n )
    {
        bool is_lhs = _is_lhs;
        _is_lhs = true;
        walk( n );
        _is_lhs = is_lhs;
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::AddAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::ArithmeticShrAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        if ( _is_lhs )
            _assigned_ext_syms.insert( n );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::Assignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::BitwiseAndAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::BitwiseOrAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::BitwiseShlAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::BitwiseShrAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::BitwiseXorAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::ClassMemberAccess& n )
    {
        if ( _is_lhs )
            _assigned_ext_syms.insert( n );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::DivAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::MinusAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::ModAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::MulAssignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::Postdecrement& n )
    {
        visit_xx_crements( n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::Postincrement& n )
    {
        visit_xx_crements( n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::Predecrement& n )
    {
        visit_xx_crements( n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::Preincrement& n )
    {
        visit_xx_crements( n.get_rhs( ) );
    }

    AssignedExtSymVisitor::Ret AssignedExtSymVisitor::visit( const Nodecl::Symbol& n )
    {
        if ( _is_lhs )
            _assigned_ext_syms.insert( n );
    }

    // **************** End assigned Extended Symbols Visitor **************** //
    // *********************************************************************** //



    // *********************************************************************** //
    // ************************ Main function Visitor ************************ //

    Nodecl::NodeclBase find_main_function( Nodecl::NodeclBase ast )
    {
        MainFunctionVisitor mv;
        mv.walk( ast );
        return mv.get_main( );
    }

    MainFunctionVisitor::MainFunctionVisitor( )
        : _main ( Nodecl::NodeclBase::null( ) )
    {}

    Nodecl::NodeclBase MainFunctionVisitor::get_main( )
    {
        return _main;
    }

    AssignedExtSymVisitor::Ret visit( const Nodecl::AsmDefinition& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::GccAsmDefinition& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::GccAsmSpec& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::GccBuiltinVaArg& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::CxxDecl& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::CxxDef& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::CxxExplicitInstantiation& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::CxxExternExplicitInstantiation& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::CxxUsingNamespace& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::CxxUsingDecl& n ) {}

    AssignedExtSymVisitor::Ret MainFunctionVisitor::visit( const Nodecl::FunctionCode& n )
    {
        Symbol sym = n.get_symbol( );
        assert( sym.is_valid( ) );

        std::string name = sym.get_name( );
        if ( name == "main" )
            _main = n;
    }

    AssignedExtSymVisitor::Ret visit( const Nodecl::GxxTrait& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::ObjectInit& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::PragmaCustomDeclaration& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::PragmaCustomDirective& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::PreprocessorLine& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::SourceComment& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::Text& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::UnknownPragma& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::UpcSyncStatement& n ) {}

    AssignedExtSymVisitor::Ret visit( const Nodecl::Verbatim& n ) {}

    // ********************** END main function Visitor ********************** //
    // *********************************************************************** //
}
}
}