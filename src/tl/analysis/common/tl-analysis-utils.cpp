/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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
#include <time.h>

namespace TL {
namespace Analysis {

    double time_nsec()
    {
        struct timespec tp;
        clock_gettime(CLOCK_MONOTONIC, &tp);
        return (tp.tv_sec * 1e9 + tp.tv_nsec);
    }

namespace Utils {

    // ******************************************************************************************* //
    // ************************** Common methods with analysis purposes ************************** //

    std::string generate_hashed_name(NBase ast)
    {
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
        std::stringstream line; line << ast.get_line();
        std::string funcname = (ast.is<Nodecl::FunctionCode>()
                ? std::string("_" + ast.as<Nodecl::FunctionCode>().get_symbol().get_name())
                : ""
        );

        return filename + "_" + line.str() + funcname + "_" + date_str;
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
        else if (n.is<Nodecl::Analysis::RangeUnion>())
        {
            const NBase& lhs_nodecl = get_nodecl_base(n.as<Nodecl::Analysis::RangeUnion>().get_lhs());
            const NBase& rhs_nodecl = get_nodecl_base(n.as<Nodecl::Analysis::RangeUnion>().get_rhs());
            if (Nodecl::Utils::structurally_equal_nodecls(lhs_nodecl, rhs_nodecl))
                nodecl = lhs_nodecl;
            else
                internal_error("The base variable of the expressions in a union must be the same "
                               "for the LHS and the RHS, but it is not for '%s'.\n", n.prettyprint().c_str());
        }
        else if (n.is<Nodecl::Analysis::RangeIntersection>())
        {
            const NBase& lhs_nodecl = get_nodecl_base(n.as<Nodecl::Analysis::RangeIntersection>().get_lhs());
            const NBase& rhs_nodecl = get_nodecl_base(n.as<Nodecl::Analysis::RangeIntersection>().get_rhs());
            if (Nodecl::Utils::structurally_equal_nodecls(lhs_nodecl, rhs_nodecl))
                nodecl = lhs_nodecl;
            else
                internal_error("The base variable of the expressions in an intersection must be the same "
                               "for the LHS and the RHS, but it is not for '%s'.\n", n.prettyprint().c_str());
        }
        else if (n.is<Nodecl::FunctionCall>())
        {
            nodecl = get_nodecl_base(n.as<Nodecl::FunctionCall>().get_called());
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
    
    static NBase nodecl_set_contains_enclosing_nodecl_rec(const NBase& n, const NodeclSet& set)
    {
        const Nodecl::NodeclBase& m = n.no_conv();

        if (nodecl_set_contains_nodecl(m, set) && !m.get_type().no_ref().is_pointer())
            return n;

        if (m.is<Nodecl::ArraySubscript>())
        {
            Nodecl::NodeclBase subscripted = m.as<Nodecl::ArraySubscript>().get_subscripted().no_conv();
            if (!m.get_type().no_ref().is_pointer())
            {
                return nodecl_set_contains_enclosing_nodecl_rec(subscripted, set);
            }
            else if(subscripted.is<Nodecl::ArraySubscript>())
            {
                subscripted = subscripted.as<Nodecl::ArraySubscript>().get_subscripted().no_conv();
                while(subscripted.is<Nodecl::ArraySubscript>())
                {
                    if(!subscripted.get_type().no_ref().is_pointer())
                        return nodecl_set_contains_enclosing_nodecl_rec(subscripted, set);
                    else
                        subscripted = subscripted.as<Nodecl::ArraySubscript>().get_subscripted().no_conv();
                }
            }
        }
        else if (m.is<Nodecl::ClassMemberAccess>())
        {
            return nodecl_set_contains_enclosing_nodecl_rec(m.as<Nodecl::ClassMemberAccess>().get_lhs(), set);
        }

        return NBase::null();
    }

    NBase nodecl_set_contains_enclosing_nodecl(const NBase& n, const NodeclSet& set)
    {
        const NBase& res = nodecl_set_contains_enclosing_nodecl_rec(n.no_conv(), set);
        if(res.is_null() && nodecl_set_contains_nodecl(n.no_conv(), set))
            return n;
        else return res;
    }
    
    Nodecl::List nodecl_set_contains_enclosed_nodecl(const NBase& n, const NodeclSet& set)
    {
        Nodecl::List result;
        
        // Symbols which are pointers are not considered to contain any access to the pointed object
        if (!n.no_conv().is<Nodecl::Symbol>()
                || !n.no_conv().get_symbol().get_type().no_ref().is_pointer())
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
            if(nodecl_set_contains_nodecl(n.no_conv(), set))
                result.append(n.shallow_copy());
        }
        return result;
    }
    
    Nodecl::NodeclBase unflatten_subscripts(Nodecl::NodeclBase n)
    {
        if (n.is<Nodecl::ArraySubscript>())
        {
            const Nodecl::NodeclBase& subscripted = n.as<Nodecl::ArraySubscript>().get_subscripted().no_conv();
            const Nodecl::List& subscripts = n.as<Nodecl::ArraySubscript>().get_subscripts().as<Nodecl::List>();

            Nodecl::NodeclBase new_subscript = unflatten_subscripts(subscripted);
            int i = 1;
            for(Nodecl::List::const_iterator it = subscripts.begin(); it != subscripts.end(); ++it)
            {
                Type t = subscripted.get_type().no_ref();
                int j = i;
                while (j>0)
                {
                    if(t.is_pointer())
                        t = t.points_to();
                    else if(t.is_array())
                        t = t.array_element();
                    else
                        internal_error("Expected array type for dimension %d in nodecl '%s', but found '%s'.\n",
                                       subscripts.size()-j, n.prettyprint().c_str(), t.print_declarator().c_str());
                    j--;
                }
                new_subscript = Nodecl::ArraySubscript::make(new_subscript.shallow_copy(),
                                                             Nodecl::List::make(it->shallow_copy()), t);
                i++;
            }
            return new_subscript;
        }
        
        return n;
    }

    Nodecl::List nodecl_set_contains_pointed_nodecl(const NBase& n, const NodeclSet& set)
    {
        Nodecl::List result;
        // Only a symbol with pointer/array type may point to some object
        if ((n.get_type().no_ref().is_pointer() || n.get_type().no_ref().is_array()))
        {
            for (NodeclSet::const_iterator it = set.begin(); it != set.end(); ++it)
            {
                Nodecl::NodeclBase sub = it->no_conv().shallow_copy();
                if (sub.is<Nodecl::ArraySubscript>())
                {
                    // Skip the less significant dimension because, if it has to be pointed,
                    // then 'n' must be a "container"
                    sub = sub.as<Nodecl::ArraySubscript>().get_subscripted().no_conv();
                    if (Nodecl::Utils::structurally_equal_nodecls(n, sub, /*skip conversions*/true))
                    {
                        result.append(sub.shallow_copy());
                        continue;
                    }

                    Nodecl::NodeclBase sub1 = sub;
                    while (true)
                    {
                        sub1 = sub1.no_conv();
                        if (Nodecl::Utils::structurally_equal_nodecls(n, sub1, /*skip conversions*/true))
                        {
                            result.append(sub1.shallow_copy());
                            goto ptd_nodecl_found;
                        }
                        if (!sub1.is<Nodecl::ArraySubscript>())
                            break;
                        sub1 = sub1.as<Nodecl::ArraySubscript>().get_subscripted();
                    }

                    // If not found, flatten all consecutive dimensions:
                    // in order to find any possible subscript access
                    // we need to check all dimensions accesses, so, we will transform:
                    // A[i][j]  ->  subscripted: A, subscripts: i, j into
                    //              subscripted: A[i], subscript: j
                    {
                        Nodecl::NodeclBase sub2 = sub;
                        sub2 = unflatten_subscripts(sub);

                        // Traverse each subscripted individually comparing it with the nodecl we are looking for
                        while (true)
                        {
                            sub2 = sub2.no_conv();
                            if (Nodecl::Utils::structurally_equal_nodecls(n, sub2, /*skip conversions*/true))
                            {
                                result.append(sub2.shallow_copy());
                                goto ptd_nodecl_found;
                            }
                            if (!sub2.is<Nodecl::ArraySubscript>())
                                break;
                            sub2 = sub2.as<Nodecl::ArraySubscript>().get_subscripted();
                        }
                    }

ptd_nodecl_found:   ;
                }
                // Nothing to do, only ArraySubscript may be pointed
                else {}
            }
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
        if (m1.size() != m2.size())
            return false;
        
        NodeclMap::const_iterator it1 = m1.begin();
        NodeclMap::const_iterator it2 = m2.begin();
        std::pair <NodeclMap::const_iterator, NodeclMap::const_iterator> range1, range2;
        for ( ; it1 != m1.end(); )
        {
            // 1.- If the number of entries for a given key is different in the two sets, the maps are different
            if (m1.count(it1->first) != m2.count(it2->first))
                return false;

            // 2.- Compare all entries regardless of the order
            range1 = m1.equal_range(it1->first);
            range2 = m2.equal_range(it2->first);
            for (NodeclMap::const_iterator itr1 = range1.first; itr1 != range1.second; )
            {
                for (NodeclMap::const_iterator itr2 = range2.first; itr2 != range2.second; ++itr2)
                {
                    if (Nodecl::Utils::structurally_equal_nodecls(itr1->first, itr2->first, /*skip_conversions*/true)
                            && Nodecl::Utils::structurally_equal_nodecls(itr1->second.first, itr2->second.first, /*skip_conversions*/true))
                        goto next_it;
                }
                return false;
next_it:        ++itr1, ++it1, ++it2;
            }
        }
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

    void TopLevelVisitor::visit( const Nodecl::CxxExplicitInstantiationDef& n ) {}

    void TopLevelVisitor::visit( const Nodecl::CxxExplicitInstantiationDecl& n ) {}

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

    void TopLevelVisitor::visit( const Nodecl::OmpSs::TaskCall& n )
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
            if (!print_in_dot
                    || !it->get_symbol().is_valid()
                    || (it->get_symbol().is_valid()
                            && !it->get_symbol().is_saved_expression()))
            {   // Avoid printing in the DOT file those variables generated by the compiler
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
            if (!print_in_dot
                    || !it->first.get_symbol().is_valid()
                    || (it->first.get_symbol().is_valid()
                            && !it->first.get_symbol().is_saved_expression()))
            {   // Avoid printing in the DOT file those variables generated by the compiler
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
