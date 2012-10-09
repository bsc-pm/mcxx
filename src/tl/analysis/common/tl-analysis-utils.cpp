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

#include <sstream>

#include "cxx-codegen.h"
#include "cxx-process.h"
#include "tl-analysis-utils.hpp"
#include "tl-nodecl.hpp"

namespace TL {
namespace Analysis {
namespace Utils {

    // ******************************************************************************************* //
    // ************************** Common methods with analysis purposes ************************** //

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

    Nodecl::NodeclBase find_main_function( Nodecl::NodeclBase ast )
    {
        TopLevelVisitor tlv;
        tlv.walk( ast );
        return tlv.get_main( );
    }

    // ************************ END common methods with analysis purposes ************************ //
    // ******************************************************************************************* //



    // ******************************************************************************************* //
    // ****************************** Visitor for Top Level nodes ******************************** //

    TopLevelVisitor::TopLevelVisitor( )
            : _main ( Nodecl::NodeclBase::null( ) ), _functions( ), _filename( "" )
    {}

    Nodecl::NodeclBase TopLevelVisitor::get_main( ) const
    {
        return _main;
    }

    ObjectList<Nodecl::NodeclBase> TopLevelVisitor::get_functions( ) const
    {
        return _functions;
    }

    void TopLevelVisitor::walk_functions( const Nodecl::NodeclBase& n )
    {
        _filename = n.get_filename( );
        walk( n );
    }

    TopLevelVisitor::Ret TopLevelVisitor::unhandled_node( const Nodecl::NodeclBase& n )
    {
        nodecl_t intern_n = n.get_internal_nodecl( );
        WARNING_MESSAGE( "Unhandled node '%s' while PCFG construction of type '%s''",
                         codegen_to_str( intern_n, nodecl_retrieve_context( intern_n ) ),
                         ast_print_node_type( n.get_kind( ) ) );
        return Ret( );
    }

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::AsmDefinition& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::GccAsmDefinition& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::GccAsmSpec& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::GccBuiltinVaArg& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::CxxDecl& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::CxxDef& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::CxxExplicitInstantiation& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::CxxExternExplicitInstantiation& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::CxxUsingNamespace& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::CxxUsingDecl& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::FunctionCode& n )
    {
        if( _filename == n.get_filename( ) )
        {
            Symbol sym = n.get_symbol( );
            ASSERT_MESSAGE( sym.is_valid( ), "TopLevelVisitor::FunctionCode node has an invalid symbol", 0 );

            std::string name = sym.get_name( );
            if ( name == "main" )
                _main = n;

            _functions.append( n );
        }
    }

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::GxxTrait& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::ObjectInit& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::PragmaCustomDeclaration& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::PragmaCustomDirective& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::PreprocessorLine& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::SourceComment& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::Text& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::UnknownPragma& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::UpcSyncStatement& n ) {}

    TopLevelVisitor::Ret TopLevelVisitor::visit( const Nodecl::Verbatim& n ) {}

    // **************************** END visitor for Top Level nodes ****************************** //
    // ******************************************************************************************* //
}
}
}