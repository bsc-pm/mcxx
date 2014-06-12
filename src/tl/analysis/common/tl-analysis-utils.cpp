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

    std::string generate_hashed_name(NBase ast)
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

    NBase find_main_function(NBase ast)
    {
        TopLevelVisitor tlv;
        tlv.walk( ast );
        return tlv.get_main( );
    }

    // ************************ END common methods with analysis purposes ************************ //
    // ******************************************************************************************* //



    // **************************************************************************************** //
    // ********************* Methods to manage nodecls and their containers ******************* //
    
    NBase get_nodecl_base(const NBase& n)
    {
        NBase nodecl;
        if(n.is<Nodecl::Symbol>() || n.is<Nodecl::PointerToMember>() || n.is<Nodecl::ObjectInit>())
        {
            nodecl = n;
        }
        else if(Nodecl::Utils::nodecl_is_literal(n))
        {
            nodecl = NBase::null();
        }
        else if (n.is<Nodecl::ClassMemberAccess>())
        {
            nodecl = get_nodecl_base(n.as<Nodecl::ClassMemberAccess>().get_lhs());
        }
        else if (n.is<Nodecl::ArraySubscript>())
        {
            nodecl = get_nodecl_base(n.as<Nodecl::ArraySubscript>().get_subscripted());
        }
        else if (n.is<Nodecl::Reference>())
        {
            nodecl = get_nodecl_base(n.as<Nodecl::Reference>().get_rhs());
        }
        else if (n.is<Nodecl::Dereference>())
        {
            nodecl = get_nodecl_base(n.as<Nodecl::Dereference>().get_rhs());
        }
        else if(n.is<Nodecl::Conversion>())
        {
            nodecl = get_nodecl_base(n.as<Nodecl::Conversion>().get_nest());
        }
        else if(n.is<Nodecl::Cast>())
        {
            nodecl = get_nodecl_base(n.as<Nodecl::Cast>().get_rhs());
        }
        else if(n.is<Nodecl::Postdecrement>())
        {
            nodecl = get_nodecl_base(n.as<Nodecl::Postdecrement>().get_rhs());
        }
        else if(n.is<Nodecl::Postincrement>())
        {
            nodecl = get_nodecl_base(n.as<Nodecl::Postincrement>().get_rhs());
        }
        else if (n.is<Nodecl::Predecrement>())
        {
            nodecl = get_nodecl_base(n.as<Nodecl::Predecrement>().get_rhs());
        }
        else if(n.is<Nodecl::Preincrement>())
        {
            nodecl = get_nodecl_base(n.as<Nodecl::Preincrement>().get_rhs());
        }
        else
        {
            nodecl = NBase::null();
        }

        return nodecl;
    }
    
    NodeclList get_nodecls_base(const NBase& n)
    {
        if (n.is<Nodecl::Symbol>() || n.is<Nodecl::PointerToMember>() || n.is<Nodecl::ObjectInit>() || n.is<Nodecl::FunctionCall>())
        {
            return NodeclList(1, n);
        }
        else if (n.is<Nodecl::IntegerLiteral>() || n.is<Nodecl::FloatingLiteral>() || n.is<Nodecl::ComplexLiteral>()
                || n.is<Nodecl::StringLiteral>() || n.is<Nodecl::BooleanLiteral>() || n.is<Nodecl::MaskLiteral>())
        {
            return NodeclList();
        }
        else if (n.is<Nodecl::ClassMemberAccess>())
        {
            Nodecl::ClassMemberAccess aux = n.as<Nodecl::ClassMemberAccess>();
            return get_nodecls_base(aux.get_lhs());
        }
        else if (n.is<Nodecl::ArraySubscript>())
        {
            Nodecl::ArraySubscript aux = n.as<Nodecl::ArraySubscript>();
            return get_nodecls_base(aux.get_subscripted());
        }
        else if (n.is<Nodecl::Reference>())
        {
            Nodecl::Reference aux = n.as<Nodecl::Reference>();
            return get_nodecls_base(aux.get_rhs());
        }
        else if (n.is<Nodecl::Dereference>())
        {
            Nodecl::Dereference aux = n.as<Nodecl::Dereference>();
            return get_nodecls_base(aux.get_rhs());
        }
        else if (n.is<Nodecl::Conversion>())
        {
            Nodecl::Conversion aux = n.as<Nodecl::Conversion>();
            return get_nodecls_base(aux.get_nest());
        }
        else if (n.is<Nodecl::Cast>())
        {
            Nodecl::Cast aux = n.as<Nodecl::Cast>();
            return get_nodecls_base(aux.get_rhs());
        }
        /*!
        * We can have (pre- post-) in- de-crements and other arithmetic operations
        * Example:
        * T *curr_high = ...;
        * *curr_high-- = l;
        * "*curr_high--" is a _KILLED_VAR
        */
        else if (n.is<Nodecl::Predecrement>())
        {
            Nodecl::Predecrement aux = n.as<Nodecl::Predecrement>();
            return get_nodecls_base(aux.get_rhs());
        }
        else if (n.is<Nodecl::Postdecrement>())
        {
            Nodecl::Postdecrement aux = n.as<Nodecl::Postdecrement>();
            return get_nodecls_base(aux.get_rhs());
        }
        else if (n.is<Nodecl::Preincrement>())
        {
            Nodecl::Preincrement aux = n.as<Nodecl::Preincrement>();
            return get_nodecls_base(aux.get_rhs());
        }
        else if (n.is<Nodecl::Postincrement>())
        {
            Nodecl::Postincrement aux = n.as<Nodecl::Postincrement>();
            return get_nodecls_base(aux.get_rhs());
        }
        else
        {
            WARNING_MESSAGE("Unexpected type of nodecl '%s'.", ast_print_node_type(n.get_kind()));
            return NodeclList();
        }
    }
    
    bool nodecl_set_contains_nodecl(const NBase& nodecl, const NodeclSet& set)
    {
        for(NodeclSet::const_iterator it = set.begin(); it != set.end(); ++it)
            if(Nodecl::Utils::structurally_equal_nodecls(nodecl, *it, /*skip_conversions*/true))
                return true;
        return false;
    }
    
    bool nodecl_set_contains_nodecl_pointer(const NBase& nodecl, const NodeclSet& set)
    {
        for(NodeclSet::const_iterator it = set.begin(); it != set.end(); ++it)
            if(nodecl == *it)
                return true;
            return false;
    }
    
    NBase nodecl_set_contains_enclosing_nodecl(const NBase& n, const NodeclSet& set)
    {
        if(n.is<Nodecl::ArraySubscript>())
        {
            Nodecl::ArraySubscript arr = n.as<Nodecl::ArraySubscript>();
            if(nodecl_set_contains_nodecl(n, set))
                return n;
            else
                return nodecl_set_contains_enclosing_nodecl(arr.get_subscripted(), set);
        }
        else if(n.is<Nodecl::ClassMemberAccess>())
        {
            Nodecl::ClassMemberAccess memb_access = n.as<Nodecl::ClassMemberAccess>();
            if(nodecl_set_contains_nodecl(n, set))
                return n;
            else
                return nodecl_set_contains_enclosing_nodecl(memb_access.get_lhs(), set);
        }
        else if(n.is<Nodecl::Conversion>())
        {
            return nodecl_set_contains_enclosing_nodecl(n.as<Nodecl::Conversion>().get_nest(), set);
        }
        else
        {
            if(nodecl_set_contains_nodecl(n, set))
                return n;
            else
                return NBase::null();
        }
    }
    
    Nodecl::List nodecl_set_contains_enclosed_nodecl(const NBase& n, const NodeclSet& set)
    {
        Nodecl::List result;
        
        // Symbols which are pointers are not considered to contain any access to the pointed object
        if(!n.no_conv().is<Nodecl::Symbol>() || !n.no_conv().get_symbol().get_type().is_pointer())
        {
            NodeclSet fake_set;
            fake_set.insert(n);
            
            for(NodeclSet::iterator it = set.begin(); it != set.end(); ++it)
            {
                if(!nodecl_set_contains_enclosing_nodecl(*it, fake_set).is_null())
                    result.append(it->shallow_copy());
            }
        }
        else
        {   // But check whether the pointer is in the set
            if(nodecl_set_contains_nodecl(n, set))
                result.append(n.shallow_copy());
        }
        return result;
    }
    
    NodeclSet nodecl_set_union(const NodeclSet& s1, const NodeclSet& s2)
    {
        NodeclSet result;
        std::set_union(s1.begin(), s1.end(), s2.begin(), s2.end(),
                       std::inserter(result, result.begin()), 
                       Nodecl::Utils::Nodecl_structural_less());
        return result;
    }
    
    NodeclMap nodecl_map_union(const NodeclMap& m1, const NodeclMap& m2)
    {
        NodeclMap result = m1;
        
        for(NodeclMap::const_iterator it = m2.begin(); it != m2.end(); ++it)
        {
            bool pair_already_in_map = false;
            std::pair<NodeclMap::iterator, NodeclMap::iterator> current_key_in_result = result.equal_range(it->first);
            for(NodeclMap::iterator itt = current_key_in_result.first; itt != current_key_in_result.second; ++itt)
            {
                if(itt->second == it->second)
                {
                    pair_already_in_map = true;
                    break;
                }
            }
            if(!pair_already_in_map)
            {
                result.insert(std::pair<NBase, NodeclPair>(it->first, it->second));
            }
        }
        
        return result;
    }
    
    NodeclSet nodecl_set_difference(const NodeclSet& s1, const NodeclSet& s2)
    {
        NodeclSet result;
        std::set_difference(s1.begin(), s1.end(), s2.begin(), s2.end(),
                            std::inserter(result, result.begin()), 
                            Nodecl::Utils::Nodecl_structural_less());
        return result;
    }
    
    NodeclMap nodecl_map_minus_nodecl_set(const NodeclMap& m, const NodeclSet& s)
    {
        NodeclMap result;
        for(NodeclMap::const_iterator it = m.begin(); it != m.end(); ++it)
            if(s.find(it->first) == s.end())
                result.insert(std::pair<NBase, NodeclPair>(it->first, it->second));
        return result;
    }
    
    bool nodecl_set_equivalence(const NodeclSet& s1, const NodeclSet& s2)
    {
        if(s1.size() == s2.size())
        {
            NodeclSet intersection;
            std::set_intersection(s1.begin(), s1.end(), s2.begin(), s2.end(),
                                  std::inserter(intersection, intersection.begin()), 
                                  Nodecl::Utils::Nodecl_structural_less());
            if(intersection.size() == s1.size())
                return true;
        }
        return false;
    }
    
    bool nodecl_map_equivalence(const NodeclMap& m1, const NodeclMap& m2)
    {
        if(m1.size() != m2.size())
            return false;
        
        NodeclMap::const_iterator it1 = m1.begin();
        NodeclMap::const_iterator it2 = m2.begin();
        for( ; it1 != m1.end(); ++it1, ++it2)
            if((it1->first != it2->first) || (it1->second != it2->second))
                return false;
        
        return true;
    }
    
    // ********************* Methods to manage nodecls and their containers ******************* //
    // **************************************************************************************** //
    
    
    
    // **************************************************************************************** //
    // **************************** Class for Auto-Scoping purposes *************************** //

    AutoScopedVariables::AutoScopedVariables()
            : _private_vars(), _firstprivate_vars(), _race_vars(), _shared_vars(), _undef_vars()
    {}

    AutoScopedVariables::AutoScopedVariables(NodeclSet private_vars, NodeclSet firstprivate_vars,
                                             NodeclSet race_vars, NodeclSet shared_vars, NodeclSet undef_vars)
            : _private_vars(private_vars), _firstprivate_vars(firstprivate_vars),
              _race_vars(race_vars), _shared_vars(shared_vars), _undef_vars(undef_vars)
    {}

    NodeclSet AutoScopedVariables::get_private_vars()
    {
        return _private_vars;
    }

    NodeclSet AutoScopedVariables::get_firstprivate_vars()
    {
        return _firstprivate_vars;
    }

    NodeclSet AutoScopedVariables::get_race_vars()
    {
        return _race_vars;
    }

    NodeclSet AutoScopedVariables::get_shared_vars()
    {
        return _shared_vars;
    }

    NodeclSet AutoScopedVariables::get_undef_vars()
    {
        return _undef_vars;
    }

    // ************************** END class for Auto-Scoping purposes ************************* //
    // **************************************************************************************** //
    


    // ******************************************************************************************* //
    // ****************************** Visitor for Top Level nodes ******************************** //

    TopLevelVisitor::TopLevelVisitor( )
            : _main (NBase::null()), _functions(), _analysis_asserted_funcs(), _filename("")
    {}

    NBase TopLevelVisitor::get_main() const
    {
        return _main;
    }

    ObjectList<NBase> TopLevelVisitor::get_functions() const
    {
        return _functions;
    }

    std::map<Symbol, NBase> TopLevelVisitor::get_asserted_funcs() const
    {
        return _analysis_asserted_funcs;
    }

    void TopLevelVisitor::walk_functions(const NBase& n)
    {
        _filename = n.get_filename( );
        walk( n );
    }

    void TopLevelVisitor::unhandled_node(const NBase& n)
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
            _functions.append( n );
        }

    void TopLevelVisitor::visit( const Nodecl::OpenMP::TaskCall& n )
    {
        if( _filename == n.get_filename( ) )
            _functions.append( n );
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

    std::string prettyprint_nodecl_set(const NodeclSet& s, bool print_in_dot)
    {
        std::string result = "";
        int line_size = 0;
        for(NodeclSet::const_iterator it = s.begin(); it != s.end(); ++it)
        {
            std::string it_str = it->prettyprint();
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

    std::string prettyprint_nodecl_map(const NodeclMap& m, bool print_in_dot)
    {
        std::string result = "";
        int line_size = 0;
        for(NodeclMap::const_iterator it = m.begin(); it != m.end(); ++it)
        {
            if( it->second.first.is_null( ) )
            {
                std::string it_str = it->first.prettyprint() + "= UNKNOWN; ";
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
                std::string it_str = it->first.prettyprint() + "=" + it->second.first.prettyprint() + "; ";
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
