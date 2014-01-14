/*--------------------------------------------------------------------
 ( C) Copyright 2006-2013 Barcelona Supercomputing Center             *
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
#include "filename.h"
#include "tl-analysis-utils.hpp"
#include "tl-nodecl.hpp"

namespace TL {
namespace Analysis {
namespace Utils {

    // ******************************************************************************************* //
    // ************************** Common methods with analysis purposes ************************** //

    std::string generate_hashed_name(Nodecl::NodeclBase ast)
    {
        std::string result;
        
        std::string date_str;
        {
            time_t t = time(NULL);
            struct tm* tmp = localtime(&t);
            if (tmp == NULL)
                internal_error("localtime failed", 0);
            char outstr[200];
            if (strftime(outstr, sizeof(outstr), "%s", tmp) == 0)
                internal_error("strftime failed", 0);
            outstr[199] = '\0';
            date_str = outstr;
        }
        
        std::string filename = ::give_basename(ast.get_filename().c_str());
        int line = ast.get_line();
        std::stringstream ss; ss << line;
        
        result = filename + "_" + ss.str() + "_" + date_str;

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
            : _main ( Nodecl::NodeclBase::null( ) ), _functions( ), _analysis_asserted_funcs( ), _filename( "" )
    {}

    Nodecl::NodeclBase TopLevelVisitor::get_main( ) const
    {
        return _main;
    }

    ObjectList<Nodecl::NodeclBase> TopLevelVisitor::get_functions( ) const
    {
        return _functions;
    }

    std::map<Symbol, Nodecl::NodeclBase> TopLevelVisitor::get_asserted_funcs( ) const
    {
        return _analysis_asserted_funcs;    
    }
    
    void TopLevelVisitor::walk_functions( const Nodecl::NodeclBase& n )
    {
        _filename = n.get_filename( );
        walk( n );
    }

    void TopLevelVisitor::unhandled_node( const Nodecl::NodeclBase& n )
    {
        nodecl_t intern_n = n.get_internal_nodecl( );
        WARNING_MESSAGE( "Unhandled node '%s' while PCFG construction of type '%s''",
                         codegen_to_str( intern_n, nodecl_retrieve_context( intern_n ) ),
                         ast_print_node_type( n.get_kind( ) ) );
    }
    
    void TopLevelVisitor::visit( const Nodecl::AsmDefinition& n ) {}

    void TopLevelVisitor::visit( const Nodecl::Analysis::AssertDecl& n ) 
    {
        Symbol s = n.get_symbol( );
        ERROR_CONDITION( !s.is_valid( ), "The symbol associated to the declaration assertion '%s' is not valid.", 
                            n.prettyprint( ).c_str( ) );
        ERROR_CONDITION( _analysis_asserted_funcs.find( s ) != _analysis_asserted_funcs.end( ), 
                            "Function %s has more than one '#pragma analysis_checker assert' associated. Only one is allowed", 
                            s.get_name( ).c_str( ) );
        _analysis_asserted_funcs[s] = n.get_environment( );
    }
    
    void TopLevelVisitor::visit( const Nodecl::GccAsmDefinition& n ) {}

    void TopLevelVisitor::visit( const Nodecl::GccAsmSpec& n ) {}

    void TopLevelVisitor::visit( const Nodecl::GccBuiltinVaArg& n ) {}

    void TopLevelVisitor::visit( const Nodecl::CxxDecl& n ) {}

    void TopLevelVisitor::visit( const Nodecl::CxxDef& n ) {}

    void TopLevelVisitor::visit( const Nodecl::CxxExplicitInstantiation& n ) {}

    void TopLevelVisitor::visit( const Nodecl::CxxExternExplicitInstantiation& n ) {}

    void TopLevelVisitor::visit( const Nodecl::CxxUsingNamespace& n ) {}

    void TopLevelVisitor::visit( const Nodecl::CxxUsingDecl& n ) {}

    void TopLevelVisitor::visit( const Nodecl::FunctionCode& n )
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

    void TopLevelVisitor::visit( const Nodecl::GxxTrait& n ) {}

    void TopLevelVisitor::visit( const Nodecl::ObjectInit& n ) {}
    
    void TopLevelVisitor::visit( const Nodecl::OpenMP::SimdFunction& n )
    {
        if( _filename == n.get_filename( ) )
        {
            _functions.append( n );
        }
    }
    
    void TopLevelVisitor::visit( const Nodecl::OpenMP::TaskCall& n )
    {
        if( _filename == n.get_filename( ) )
        {
            _functions.append( n );
        }
    }
    
    void TopLevelVisitor::visit( const Nodecl::PragmaCustomDeclaration& n ) {}

    void TopLevelVisitor::visit( const Nodecl::PragmaCustomDirective& n ) {}

    void TopLevelVisitor::visit( const Nodecl::PreprocessorLine& n ) {}

    void TopLevelVisitor::visit( const Nodecl::SourceComment& n ) {}

    void TopLevelVisitor::visit( const Nodecl::Text& n ) {}

    void TopLevelVisitor::visit( const Nodecl::UnknownPragma& n ) {}

    void TopLevelVisitor::visit( const Nodecl::UpcSyncStatement& n ) {}

    void TopLevelVisitor::visit( const Nodecl::Verbatim& n ) {}

    // **************************** END visitor for Top Level nodes ****************************** //
    // ******************************************************************************************* //
    
    
    
    // ******************************************************************************************* //
    // ************************************ Printing methods ************************************* //
    
    void makeup_dot_block( std::string& str )
    {
        int pos;
        // Escape double quotes
        pos = 0;
        while( ( pos=str.find( "\"", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\\"" );
            pos += 2;
        }
        // Delete implicit line feeds
        pos = 0;
        while( ( pos=str.find( "\n", pos ) ) != -1 ) {
            str.replace ( pos, 1, "" );
        }
        // Escape explicit line feeds
        pos = 0;
        while( ( pos=str.find( "\\n", pos ) ) != -1 ) {
            str.replace ( pos, 2, "\\\\n" );
            pos += 3;
        }
        // Escape the comparison symbols '<' and '>'
        pos = 0;
        while( ( pos=str.find( "<", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\<" );
            pos += 2;
        }
        pos = 0;
        while( ( pos=str.find( ">", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\>" );
            pos += 2;
        }
        // Escape the brackets '{' '}'
        pos = 0;
        while( ( pos=str.find( "{", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\{" );
                pos += 2;
        }
        pos = 0;
        while( ( pos=str.find( "}", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\}" );
            pos += 2;
        }
        // Escape the OR operand
        pos = 0;
        while( ( pos=str.find( "|", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\|" );
            pos += 2;
        }
        // Escape '%' operand
        pos = 0;
        while( ( pos=str.find( "%", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\%" );
            pos += 2;
        }
        // Escape '?' token
        pos = 0;
        while( ( pos=str.find( "?", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\?" );
            pos += 2;
        }
        // Replace $$ intruced to break the line
        // We don't use '\n' because it is replaced previously
        pos = 0;
        while( ( pos=str.find( "$$", pos ) ) != -1 ) {
            str.replace ( pos, 2, "\\n" );
            pos += 2;
        }
        
    }
    
    std::string prettyprint_ext_sym_set( ext_sym_set s, bool print_in_dot )
    {
        std::string result = "";
        int line_size = 0;
        for( ext_sym_set::iterator it = s.begin( ); it != s.end( ); ++it )
        {
            std::string it_str = it->get_nodecl( ).prettyprint( );
            if( line_size + it_str.size( ) > 100 )
            {
                result += "$$";
                line_size = it_str.size( );
            }
            else
                line_size += it_str.size( ) + 3;
            result += it_str +  ", ";
            if( line_size > 100 )
                result += "$$";
        }
        
        if( !result.empty( ) )
        {
            result = result.substr( 0, result.size( ) - 2 );
            if( print_in_dot )
                makeup_dot_block( result );
        }
        
        return result;
    }
    
    std::string prettyprint_ext_sym_map( ext_sym_map s, bool print_in_dot )
    {
        std::string result = "";
        int line_size = 0;
        for( ext_sym_map::iterator it = s.begin( ); it != s.end( ); ++it )
        {
            if( it->second.is_null( ) )
            {
                std::string it_str = it->first.get_nodecl( ).prettyprint( ) + "=UNKNOWN VALUE; ";
                if( line_size + it_str.size( ) > 100 )
                {
                    result += "$$";
                    line_size = it_str.size( );
                }
                else
                    line_size += it_str.size( );
                result += it_str;
                if( line_size > 100 )
                    result += "$$";
            }
            else
            {
                std::string it_str = it->first.get_nodecl( ).prettyprint( ) + "=" + it->second.prettyprint( ) + "; ";
                if( line_size + it_str.size( ) > 100 )
                {
                    result += "$$";
                    line_size = it_str.size( );
                }
                else
                    line_size += it_str.size( );
                result += it_str;
                if( line_size > 100 )
                    result += "$$";
            }
        }
        
        if( !result.empty( ) )
        {
            result = result.substr( 0, result.size( ) - 2 );
            if( print_in_dot )
                makeup_dot_block(result);
        }
        
        return result;
    }
    
    // ********************************** END printing methods *********************************** //
    // ******************************************************************************************* //
}
}
}
