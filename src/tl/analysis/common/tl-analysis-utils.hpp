/*--------------------------------------------------------------------
 ( C) Copyright 2006-2012 Barcelona Supercomputing Center             **
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

#ifndef TL_ANALYSIS_UTILS_HPP
#define TL_ANALYSIS_UTILS_HPP

#include "tl-extended-symbol.hpp"
#include "tl-induction-variables-data.hpp"
#include "tl-nodecl-visitor.hpp"

#define VERBOSE ( CURRENT_CONFIGURATION->debug_options.analysis_verbose || \
                  CURRENT_CONFIGURATION->debug_options.enable_debug_code )

namespace TL {
namespace Analysis {
namespace Utils {

    // ******************************************************************************************* //
    // ************************** Common methods with analysis purposes ************************** //

    //! Returns a hashed string depending on \ast
    std::string generate_hashed_name( Nodecl::NodeclBase ast );

    //! Returns the Nodecl containing the main in \ast, if it exists.
    //! Returns a null Nodecl otherwise
    Nodecl::NodeclBase find_main_function( Nodecl::NodeclBase ast );

    // ************************ END common methods with analysis purposes ************************ //
    // ******************************************************************************************* //



    // ******************************************************************************************* //
    // ****************************** Visitor for Top Level nodes ******************************** //

    //! Visitor to visit Top Level nodes
    //! It also recognizes the main function in C/C++ codes if it exists
    class LIBTL_CLASS TopLevelVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        // ******* Class attributes ******* //
        Nodecl::NodeclBase _main;
        ObjectList<Nodecl::NodeclBase> _functions;
        std::map<Symbol, Nodecl::NodeclBase> _analysis_asserted_funcs;
        std::string _filename;

    public:
        // ********* Constructors ********* //
        //! Constructor
        TopLevelVisitor( );

        // ****** Getters and setters ****** //
        Nodecl::NodeclBase get_main( ) const;
        ObjectList<Nodecl::NodeclBase> get_functions( ) const;
        std::map<Symbol, Nodecl::NodeclBase> get_asserted_funcs( ) const;

        // ******** Visiting methods ******* //

        void walk_functions( const Nodecl::NodeclBase& n );

        //! Visiting methods
        /*!We re-implement all TopLevel nodecl to avoid continue visiting in case
         * the main function has already been found.
         * TopLevel
         *      FunctionCode
         *      ObjectInit
         *      CxxDecl             : CxxDecl, CxxDef, CxxExplicitInstantiation
         *                            CxxExternExplicitInstantiation, CxxUsingNamespace, CxxUsingDecl
         *      PragmaDirective     : PragmaCustomDirective, PragmaCustomDeclaration
         *      Compatibility       : UnknownPragma, SourceComment, PreprocessorLine, Verbatim
         *                            AsmDefinition, GccAsmDefinition, GccAsmSpec, UpcSyncStatement
         *                            GccBuiltinVaArg, GxxTrait, Text
         */
        Ret unhandled_node( const Nodecl::NodeclBase& n );
        Ret visit( const Nodecl::AsmDefinition& n );
        Ret visit( const Nodecl::Analysis::AssertDecl& n );
        Ret visit( const Nodecl::GccAsmDefinition& n );
        Ret visit( const Nodecl::GccAsmSpec& n );
        Ret visit( const Nodecl::GccBuiltinVaArg& n );
        Ret visit( const Nodecl::CxxDecl& n );
        Ret visit( const Nodecl::CxxDef& n );
        Ret visit( const Nodecl::CxxExplicitInstantiation& n );
        Ret visit( const Nodecl::CxxExternExplicitInstantiation& n );
        Ret visit( const Nodecl::CxxUsingNamespace& n );
        Ret visit( const Nodecl::CxxUsingDecl& n );
        Ret visit( const Nodecl::FunctionCode& n );
        Ret visit( const Nodecl::GxxTrait& n );
        Ret visit( const Nodecl::ObjectInit& n );
        Ret visit( const Nodecl::OpenMP::SimdFunction& n );
        Ret visit( const Nodecl::OpenMP::TaskCall& n );
        Ret visit( const Nodecl::PragmaCustomDeclaration& n );
        Ret visit( const Nodecl::PragmaCustomDirective& n );
        Ret visit( const Nodecl::PreprocessorLine& n );
        Ret visit( const Nodecl::SourceComment& n );
        Ret visit( const Nodecl::Text& n );
        Ret visit( const Nodecl::UnknownPragma& n );
        Ret visit( const Nodecl::UpcSyncStatement& n );
        Ret visit( const Nodecl::Verbatim& n );
    };

    // **************************** END visitor for Top Level nodes ****************************** //
    // ******************************************************************************************* //
    
    
    
    // ******************************************************************************************* //
    // **************************** Class defining the types of usage **************************** //
    
    struct UsageKind {
        enum Usage_tag {
            NONE        = 1u << 1,
            USED        = 1u << 2,
            DEFINED     = 1u << 3,
            UNDEFINED   = 1u << 4
        } _usage_type;
        
        UsageKind( )
            : _usage_type( NONE )
        {}
        
        UsageKind( Usage_tag u )
            : _usage_type( u )
        {}
        
        UsageKind( int u )
            : _usage_type( Usage_tag( u ) )
        {}
        
        UsageKind operator|( UsageKind u )
        {
            return UsageKind( int(this->_usage_type) | int( u._usage_type ) );
        }
    };
    
    // ************************** END class defining the types of usage ************************** //
    // ******************************************************************************************* //
    
    
    
    // ******************************************************************************************* //
    // ************************ Class defining the range analysis values ************************* //
    
    union RangeValue_tag {
        Nodecl::NodeclBase* n;
        InductionVariableData* iv;
    };
    
    typedef std::pair<Nodecl::NodeclBase, ObjectList<RangeValue_tag> > RangeValuesMapEntry;
    typedef std::map<Nodecl::NodeclBase, ObjectList<RangeValue_tag> > RangeValuesMap;
    
    bool map_pair_compare( std::pair<Nodecl::NodeclBase, ObjectList<Utils::RangeValue_tag> > pair1, 
                           std::pair<Nodecl::NodeclBase, ObjectList<Utils::RangeValue_tag> > pair2 );

    // ********************** END class defining the range analysis values *********************** //
    // ******************************************************************************************* //
    
    
    
    // ******************************************************************************************* //
    // ************************************ Printing methods ************************************* //
    
    void makeup_dot_block( std::string& str );
    std::string prettyprint_ext_sym_set( Utils::ext_sym_set s, bool print_in_dot );
    std::string prettyprint_ext_sym_map( Utils::ext_sym_map s, bool print_in_dot );
    std::string prettyprint_range_values_map( Utils::RangeValuesMap s, bool print_in_dot  );
    
    // ********************************** END printing methods *********************************** //
    // ******************************************************************************************* //
    
}
}
}

#endif          // TL_ANALYSIS_UTILS_HPP