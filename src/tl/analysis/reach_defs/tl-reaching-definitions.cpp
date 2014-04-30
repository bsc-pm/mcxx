/*--------------------------------------------------------------------
(C) Copyright 2006-2009 Barcelona Supercomputing Center
Centro Nacional de Supercomputacion

This file is part of Mercurium C/C++ source-to-source compiler.

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

#include "cxx-cexpr.h"

#include "tl-analysis-utils.hpp"
#include "tl-reaching-definitions.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // ************************** Class implementing reaching definition analysis ************************* //

    ReachingDefinitions::ReachingDefinitions( ExtensibleGraph* graph )
        : _graph( graph ), _undefined_reach_defs()
    {}

    void ReachingDefinitions::compute_reaching_definitions( )
    {
        Node* graph = _graph->get_graph( );

        // Set a fictitious reaching definition for each parameter 
        generate_undefined_reaching_definitions( );
        
        // Compute initial info (liveness only regarding the current node)
        gather_reaching_definitions_initial_information( graph );
        ExtensibleGraph::clear_visits( graph );

        // Common Liveness analysis
        solve_reaching_definition_equations( graph );
        ExtensibleGraph::clear_visits( graph );
    }

    void ReachingDefinitions::generate_undefined_reaching_definitions( )
    {
        // Take care of the function parameters
        Symbol func_sym = _graph->get_function_symbol();
        if(!func_sym.is_valid())
            return;
        
        ObjectList<Symbol> params = func_sym.get_function_parameters();
        for(ObjectList<Symbol>::iterator it = params.begin(); it != params.end(); ++it)
        {
            Nodecl::Symbol s = Nodecl::Symbol::make(*it);
            s.set_type(it->get_type());
            Utils::ExtendedSymbol es(s);
            _undefined_reach_defs.insert(
                    std::pair<Utils::ExtendedSymbol, Utils::NodeclPair>(
                            es, Utils::NodeclPair(Nodecl::Unknown::make(), Nodecl::Unknown::make())));
        }
        
        // Take care of the global variables
        const std::set<Symbol> global_vars = _graph->get_global_variables();
        for(std::set<Symbol>::const_iterator it = global_vars.begin(); it != global_vars.end(); ++it)
        {
            // Compute the reaching definition: the initialization value if the symbol is constant or undefined otherwise
            Nodecl::NodeclBase reach_def = Nodecl::Unknown::make();
            if(it->get_type().is_const())
            {
                Nodecl::NodeclBase sym_val = it->get_value();
                if(!sym_val.is_null())
                    reach_def = sym_val.shallow_copy();
            }
            // Build the nodecl symbol
            Nodecl::Symbol s = Nodecl::Symbol::make(*it);
            s.set_type(it->get_type());
            Utils::ExtendedSymbol es(s);
            _undefined_reach_defs.insert(
                    std::pair<Utils::ExtendedSymbol, Utils::NodeclPair>(
                            es, Utils::NodeclPair(reach_def, Nodecl::NodeclBase::null())));
        }
        
        // Initialize as undefined all variables
        ObjectList<Nodecl::NodeclBase> nodecls;
        Node* pcfg = _graph->get_graph();
        if(pcfg->is_graph_node())
        {
            nodecls.append(pcfg->get_graph_related_ast());
        }
        else if(pcfg->has_statements())
        {
            ObjectList<Nodecl::NodeclBase> stmts = pcfg->get_statements();
            for(ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin(); it != stmts.end(); ++it)
                nodecls.append(*it);
        }
        
        for(ObjectList<Nodecl::NodeclBase>::iterator it = nodecls.begin(); it != nodecls.end(); ++it)
        {
            ObjectList<Nodecl::Symbol> all_syms = Nodecl::Utils::get_all_symbols_first_occurrence(*it);
            for(ObjectList<Nodecl::Symbol>:: iterator itt = all_syms.begin(); itt != all_syms.end(); ++itt)
            {
                Utils::ExtendedSymbol es(*itt);
                if(_undefined_reach_defs.find(es) == _undefined_reach_defs.end())
                {
                    Nodecl::NodeclBase reach_def = Nodecl::Unknown::make();
                    _undefined_reach_defs.insert(
                            std::pair<Utils::ExtendedSymbol, Utils::NodeclPair>(
                                    es, Utils::NodeclPair(reach_def, Nodecl::NodeclBase::null())));
                }
            }
        }
    }
    
    void ReachingDefinitions::gather_reaching_definitions_initial_information( Node* current )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );

            if( !current->is_exit_node( ) )
            {
                if( current->is_graph_node( ) )
                {
                    Node* entry = current->get_graph_entry_node( );
                    gather_reaching_definitions_initial_information( entry );
                    set_graph_node_reaching_definitions( current );
                }
                else if( !current->is_entry_node( ) )
                {
                    GeneratedStatementsVisitor rdv;
                    ObjectList<Nodecl::NodeclBase> stmts = current->get_statements( );
                    for( ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin( ); it != stmts.end( ); ++it )
                    {
                        rdv.walk( *it );
                    }
                    current->set_generated_stmts( rdv.get_gen( ) );
                }

                ObjectList<Edge*> exit_edges = current->get_exit_edges( );
                for( ObjectList<Edge*>::iterator it = exit_edges.begin( ); it != exit_edges.end( ); ++it )
                {
                    gather_reaching_definitions_initial_information( ( *it )->get_target( ) );
                }
            }
        }
    }

    void ReachingDefinitions::solve_reaching_definition_equations( Node* current )
    {
        bool changed = true;
        while( changed )
        {
            changed = false;
            solve_reaching_definition_equations_rec( current, changed );
            ExtensibleGraph::clear_visits( current );
        }
    }

    void ReachingDefinitions::solve_reaching_definition_equations_rec( Node* current, bool& changed )
    {
        if ( !current->is_visited( ) )
        {
            current->set_visited( true );

            if( !current->is_exit_node( ) )
            {
                if( current->is_graph_node( ) )
                {
                    solve_reaching_definition_equations_rec( current->get_graph_entry_node(), changed );
                    set_graph_node_reaching_definitions( current );
                }
                else if( !current->is_entry_node( ) )
                {
                    Utils::ext_sym_map old_rd_in = current->get_reaching_definitions_in();
                    Utils::ext_sym_map old_rd_out = current->get_reaching_definitions_out();
                    Utils::ext_sym_map rd_out, rd_in, pred_rd_out;

                    // Computing Reach Defs In
                    ObjectList<Node*> parents = current->get_parents( );
                    for( ObjectList<Node*>::iterator it = parents.begin( ); it != parents.end( ); ++it )
                    {
                        bool parent_is_entry = ( *it )->is_entry_node( );
                        if( parent_is_entry )
                        {
                            // Iterate over outer parents while we found an ENTRY node
                            Node* entry_outer_node = ( *it )->get_outer_node( );
                            ObjectList<Node*> outer_parents;
                            while( parent_is_entry )
                            {
                                outer_parents = entry_outer_node->get_parents( );
                                parent_is_entry = ( outer_parents.size( ) == 1 ) && outer_parents[0]->is_entry_node( );
                                entry_outer_node = ( parent_is_entry ? outer_parents[0]->get_outer_node( ) : NULL );
                            }
                            // Get the Reach Def Out of the current predecessors
                            for( ObjectList<Node*>::iterator itop = outer_parents.begin( ); itop != outer_parents.end( ); ++itop )
                            {
                                Utils::ext_sym_map outer_rd_out = ( *itop )->get_reaching_definitions_out( );
                                pred_rd_out.insert( outer_rd_out.begin( ), outer_rd_out.end( ) );
                            }
                        }
                        else
                        {
                            pred_rd_out = ( *it )->get_reaching_definitions_out( );
                        }
                        rd_in = Utils::ext_sym_map_union( rd_in, pred_rd_out );
                        if(_graph->is_first_statement_node(current))
                            rd_in = Utils::ext_sym_map_union( rd_in, _undefined_reach_defs );
                    }

                    // Computing Reach Defs Out
                    Utils::ext_sym_map gen = current->get_generated_stmts( );
                    Utils::ext_sym_set killed = current->get_killed_vars( );
                    Utils::ext_sym_map diff = Utils::ext_sym_map_minus_ext_sym_set( rd_in, killed );
                    rd_out = Utils::ext_sym_map_union( gen, diff );

                    if( !Utils::ext_sym_map_equivalence( old_rd_in, rd_in ) || !Utils::ext_sym_map_equivalence( old_rd_out, rd_out ) )
                    {
                        current->set_reaching_definitions_in( rd_in );
                        current->set_reaching_definitions_out( rd_out );
                        changed = true;
                    }
                }

                ObjectList<Node*> children = current->get_children( );
                for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
                {
                    solve_reaching_definition_equations_rec( *it, changed );
                }
            }
        }
    }

    void ReachingDefinitions::set_graph_node_reaching_definitions( Node* current )
    {
        if( current->is_graph_node( ) )
        {
            // RDI(graph) = U RDO (Y), for all Y predecessors of X
            Utils::ext_sym_map graph_rdi;
            ObjectList<Node*> parents = current->get_parents( );
            for( ObjectList<Node*>::iterator it = parents.begin( ); it != parents.end( ); )
            {
                Node* c_it = *it;
                while(c_it->is_entry_node())
                {
                    if(c_it->get_outer_node()->get_id() != 0)
                        c_it = c_it->get_outer_node()->get_parents()[0];
                    else
                        goto iterate;
                }
                graph_rdi = Utils::ext_sym_map_union( graph_rdi, c_it->get_reaching_definitions_out( ) );
iterate:        ++it;
            }
            current->set_reaching_definitions_in( graph_rdi );

            // RDO(graph) = U RDO(inner exits)
            Utils::ext_sym_map graph_rdo;
            ObjectList<Node*> exits = current->get_graph_exit_node( )->get_parents( );
            for( ObjectList<Node*>::iterator it = exits.begin( ); it != exits.end( ); ++it )
            {
                graph_rdo = Utils::ext_sym_map_union( graph_rdo, ( *it )->get_reaching_definitions_out( ) );
            }
            if(graph_rdo.empty())
            {   // This may happen when no Reaching Defintion has been computed inside the graph or
                // when there is no statement inside the task and the information has not been propagated 
                // (Entry and Exit nodes do not contain any analysis information)
                // In this case, we propagate the Reaching Definition Out from the parents
                graph_rdo = graph_rdi;
            }
            current->set_reaching_definitions_out( graph_rdo );
        }
    }

    // *********************** End class implementing reaching definitions analysis *********************** //
    // **************************************************************************************************** //



    // **************************************************************************************************** //
    // ************************ Class implementing a visitor of reaching definition *********************** //

    GeneratedStatementsVisitor::GeneratedStatementsVisitor( )
            : _gen( )
    {}

    Utils::ext_sym_map GeneratedStatementsVisitor::get_gen( )
    {
        return _gen;
    }

    void GeneratedStatementsVisitor::visit_assignment( const Nodecl::NodeclBase& lhs, const Nodecl::NodeclBase& rhs, 
                                                       const Nodecl::NodeclBase& stmt )
    {
        if( _gen.find( lhs ) != _gen.end( ) )
        {   // Generated set only contains downwards exposed definitions
            // So, if there is a previous statement that generated a definition for the same variable
            // we remove it from the list
            _gen.erase( Utils::ExtendedSymbol( lhs ) );
        }
        _gen.insert( std::pair<Utils::ExtendedSymbol, Utils::NodeclPair>( Utils::ExtendedSymbol( lhs ), 
                                                                          Utils::NodeclPair(rhs, stmt) ) );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::AddAssignment& n )
    {
        Nodecl::Add rhs = Nodecl::Add::make( n.get_lhs( ).shallow_copy( ), n.get_rhs( ).shallow_copy( ),
                                             n.get_type( ), n.get_locus() );
        visit_assignment( n.get_lhs( ), rhs, n );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::ArithmeticShrAssignment& n )
    {
        Nodecl::ArithmeticShr rhs = Nodecl::ArithmeticShr::make( n.get_lhs( ).shallow_copy( ), n.get_rhs( ).shallow_copy( ),
                                                                 n.get_type( ), n.get_locus() );
        visit_assignment( n.get_lhs( ), rhs, n );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::Assignment& n )
    {
        visit_assignment( n.get_lhs( ), n.get_rhs( ), Nodecl::NodeclBase::null() );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::BitwiseAndAssignment& n )
    {
        Nodecl::BitwiseAnd rhs = Nodecl::BitwiseAnd::make( n.get_lhs( ).shallow_copy( ), n.get_rhs( ).shallow_copy( ),
                                                           n.get_type( ), n.get_locus() );
        visit_assignment( n.get_lhs( ), rhs, n );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::BitwiseOrAssignment& n )
    {
        Nodecl::BitwiseOr rhs = Nodecl::BitwiseOr::make( n.get_lhs( ).shallow_copy( ), n.get_rhs( ).shallow_copy( ),
                                                         n.get_type( ), n.get_locus() );
        visit_assignment( n.get_lhs( ), rhs, n );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::BitwiseShlAssignment& n )
    {
        Nodecl::BitwiseShl rhs = Nodecl::BitwiseShl::make( n.get_lhs( ).shallow_copy( ), n.get_rhs( ).shallow_copy( ),
                                                           n.get_type( ), n.get_locus() );
        visit_assignment( n.get_lhs( ), rhs, n );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::BitwiseShrAssignment& n )
    {
        Nodecl::BitwiseShr rhs = Nodecl::BitwiseShr::make( n.get_lhs( ).shallow_copy( ), n.get_rhs( ).shallow_copy( ),
                                                           n.get_type( ), n.get_locus() );
        visit_assignment( n.get_lhs( ), rhs, n );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::BitwiseXorAssignment& n )
    {
        Nodecl::BitwiseXor rhs = Nodecl::BitwiseXor::make( n.get_lhs( ).shallow_copy( ), n.get_rhs( ).shallow_copy( ),
                                                           n.get_type( ), n.get_locus() );
        visit_assignment( n.get_lhs( ), rhs, n );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::DivAssignment& n )
    {
        Nodecl::Div rhs = Nodecl::Div::make( n.get_lhs( ).shallow_copy( ), n.get_rhs( ).shallow_copy( ),
                                             n.get_type( ), n.get_locus() );
        visit_assignment( n.get_lhs( ), rhs, n );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::MinusAssignment& n )
    {
        Nodecl::Minus rhs = Nodecl::Minus::make( n.get_lhs( ).shallow_copy( ), n.get_rhs( ).shallow_copy( ),
                                                 n.get_type( ), n.get_locus() );
        visit_assignment( n.get_lhs( ), rhs, n );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::ModAssignment& n )
    {
        Nodecl::Mod rhs = Nodecl::Mod::make( n.get_lhs( ).shallow_copy( ), n.get_rhs( ).shallow_copy( ),
                                             n.get_type( ),  n.get_locus() );
        visit_assignment( n.get_lhs( ), rhs, n );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::MulAssignment& n )
    {
        Nodecl::Mul rhs = Nodecl::Mul::make( n.get_lhs( ).shallow_copy( ), n.get_rhs( ).shallow_copy( ),
                                             n.get_type( ), n.get_locus() );
        visit_assignment( n.get_lhs( ), rhs, n );
    }
    
    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::ObjectInit& n )
    {
        TL::Symbol lhs_sym = n.get_symbol( );
        Nodecl::Symbol lhs = Nodecl::Symbol::make( lhs_sym, n.get_locus( ) );
        Nodecl::NodeclBase rhs = lhs_sym.get_value( );
        
        // check for nested assignments
        walk( rhs );
        
        _gen.insert( std::pair<Utils::ExtendedSymbol, Utils::NodeclPair>( Utils::ExtendedSymbol( lhs ), 
                                                                          Utils::NodeclPair(rhs, Nodecl::NodeclBase::null()) ) );
    }
    
    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::Postdecrement& n )
    {
        Nodecl::IntegerLiteral one = Nodecl::IntegerLiteral::make( n.get_type( ), const_value_get_one( /* bytes */ 4, /* signed */ 1 ),
                                                                   n.get_locus() );
        Nodecl::Minus rhs = Nodecl::Minus::make( n.get_rhs( ).shallow_copy(), one , n.get_type( ),
                                                 n.get_locus() );
        visit_assignment( n.get_rhs( ), rhs, n );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::Postincrement& n )
    {
        Nodecl::IntegerLiteral one = Nodecl::IntegerLiteral::make( n.get_type( ), const_value_get_one( /* bytes */ 4, /* signed */ 1 ),
                                                                   n.get_locus() );
        Nodecl::Add rhs = Nodecl::Add::make( n.get_rhs( ).shallow_copy( ), one, n.get_type( ),
                                             n.get_locus() );
        visit_assignment( n.get_rhs( ), rhs, n );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::Predecrement& n )
    {
        Nodecl::IntegerLiteral one = Nodecl::IntegerLiteral::make( n.get_type( ), const_value_get_one( /* bytes */ 4, /* signed */ 1 ),
                                                                   n.get_locus() );
        Nodecl::Minus rhs = Nodecl::Minus::make( n.get_rhs( ).shallow_copy( ), one, n.get_type( ),
                                                 n.get_locus() );
        visit_assignment( n.get_rhs( ), rhs, n );
    }

    GeneratedStatementsVisitor::Ret GeneratedStatementsVisitor::visit( const Nodecl::Preincrement& n )
    {
        Nodecl::IntegerLiteral one = Nodecl::IntegerLiteral::make( n.get_type( ), const_value_get_one( /* bytes */ 4, /* signed */ 1 ),
                                                                   n.get_locus() );
        Nodecl::Add rhs = Nodecl::Add::make( n.get_rhs( ).shallow_copy( ), one, n.get_type( ),
                                             n.get_locus() );
        visit_assignment( n.get_rhs( ), rhs, n );
    }

    // ********************** END class implementing a visitor of reaching definition ********************* //
    // **************************************************************************************************** //





    // El visitant d'statements de ReachDefs caldrà fer coses de l'estil:
    // - Post increment:
    //         nodecl_t one = const_value_to_nodecl( const_value_get_one( /* bytes */ 4, /* signed*/ 1 ) );
    //         _init_expression = compute_init_expr( Nodecl::Add(n.get_type(), n.get_lhs(), Nodecl::NodeclBase( one ), n.get_locus() ) );
    // - AddAssignment:
    //         _init_expression = compute_init_expr( Nodecl::Add(n.get_type(), n.get_lhs(), n.get_rhs(), n.get_locus() ) );
//     void GeneratedStatementsVisitor::compute_init_expr( Nodecl::NodeclBase init_value )
//     {
//         Nodecl::Calculator calc;
//         const_value_t* const_val = calc.compute_const_value( init_value );
//
//         TL::Type type = init_value.get_type( );
//         std::string filename = init_value.get_filename( );
//         int line = init_value.get_line( );
//         if( const_val != NULL )
//         {
//             Nodecl::NodeclBase const_nodecl = Nodecl::NodeclBase( const_value_to_nodecl( const_val ) );
//             if( const_nodecl.is<Nodecl::BooleanLiteral>( ) )
//             {
//                 _init_expression = Nodecl::BooleanLiteral::make( type, const_val, filename, line );
//             }
//             else if( const_nodecl.is<Nodecl::ComplexLiteral>( ) )
//             {
//                 _init_expression = Nodecl::ComplexLiteral::make( const_value_complex_get_real_part( const_val ),
//                                                                     const_value_complex_get_imag_part( const_val ),
//                                                                     type, filename, line );
//             }
//             else if( const_nodecl.is<Nodecl::FloatingLiteral>( ) )
//             {
//                 _init_expression = Nodecl::FloatingLiteral::make( type, const_val, filename, line );
//             }
//             else if( const_nodecl.is<Nodecl::IntegerLiteral>( ) )
//             {
//                 _init_expression = Nodecl::IntegerLiteral::make( type, const_val, filename, line );
//             }
//             else if( const_nodecl.is<Nodecl::StringLiteral>( ) )
//             {
//                 _init_expression = Nodecl::StringLiteral::make( type, const_val, filename, line );
//             }
//             else
//             {
//                 internal_error( "Unexpected node type '%s' while computing initial value in a constant expression",
//                 ast_print_node_type( kind ) );
//             }
//         }
//         else
//         {
//             _init_expression = val;
//         }
//     }

//     static void make_permanent_auxiliar_values(Node* node)
//     {
//         if (!node->is_visited())
//         {
// //                 std::cerr << "Making permanent values  -->  " << node->get_id() << std::endl;
//             node->set_visited(true);
//
//             ext_sym_map aux_reach_defs = node->get_auxiliar_reaching_definitions();
//             node->set_reaching_definition_list(aux_reach_defs);
//
//             ObjectList<Node*> children = node->get_children();
//             for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
//             {
//                 make_permanent_auxiliar_values(*it);
//             }
//         }
//     }

//     static bool is_range(Nodecl::NodeclBase nodecl)
//     {
//         if (nodecl.is<Nodecl::Symbol>() || nodecl.is<Nodecl::IntegerLiteral>())
//         {
//             return false;
//         }
//         else if (nodecl.is<Nodecl::Range>())
//         {
//             return true;
//         }
//         else if (nodecl.is<Nodecl::List>())
//         {
//             bool result = false;
//             Nodecl::List aux = nodecl.as<Nodecl::List>();
//             for(Nodecl::List::iterator it = aux.begin(); it != aux.end(); ++it)
//             {
//                 result = result || is_range(*it);
//             }
//             return result;
//         }
//         else if (nodecl.is<Nodecl::ArraySubscript>())
//         {
//             Nodecl::ArraySubscript aux = nodecl.as<Nodecl::ArraySubscript>();
//             return (is_range(aux.get_subscripted()) || is_range(aux.get_subscripts()));
//         }
//         else if (nodecl.is<Nodecl::ClassMemberAccess>())
//         {
//             Nodecl::ClassMemberAccess aux = nodecl.as<Nodecl::ClassMemberAccess>();
//             return (is_range(aux.get_lhs()) || is_range(aux.get_member()));
//         }
//         else if (nodecl.is<Nodecl::Reference>())
//         {
//             Nodecl::Reference aux = nodecl.as<Nodecl::Reference>();
//             return (is_range(aux.get_rhs()));
//         }
//         else if (nodecl.is<Nodecl::Dereference>())
//         {
//             Nodecl::Dereference aux = nodecl.as<Nodecl::Dereference>();
//             return (is_range(aux.get_rhs()));
//         }
//         // Many different expression can be built while the renaming period: so, here we can find any kind of expression
//         else if (nodecl.is<Nodecl::Conversion>())
//         {
//             Nodecl::Conversion aux = nodecl.as<Nodecl::Conversion>();
//             return is_range(aux.get_nest());
//         }
//         else if (nodecl.is<Nodecl::Cast>())
//         {
//             Nodecl::Cast aux = nodecl.as<Nodecl::Cast>();
//             return is_range(aux.get_rhs());
//         }
//         else if (nodecl.is<Nodecl::Add>())
//         {
//             Nodecl::Add aux = nodecl.as<Nodecl::Add>();
//             return (is_range(aux.get_lhs()) || is_range(aux.get_rhs()));
//         }
//         else if (nodecl.is<Nodecl::Minus>())
//         {
//             Nodecl::Minus aux = nodecl.as<Nodecl::Minus>();
//             return (is_range(aux.get_lhs()) || is_range(aux.get_rhs()));
//         }
//         else if (nodecl.is<Nodecl::Mul>())
//         {
//             Nodecl::Mul aux = nodecl.as<Nodecl::Mul>();
//             return (is_range(aux.get_lhs()) || is_range(aux.get_rhs()));
//         }
//         else if (nodecl.is<Nodecl::Div>())
//         {
//             Nodecl::Div aux = nodecl.as<Nodecl::Div>();
//             return (is_range(aux.get_lhs()) || is_range(aux.get_rhs()));
//         }
//         else
//         {
//             internal_error("Unexpected node type '%s' while traversing a nodecl embedded in an extensible symbol",
//                         ast_print_node_type(nodecl.get_kind()));
//         }
//     }

    /*!
     * This method merges the reaching definitions of a list of nodes
     * All definitions that exist in all parents and are the same for all of them, are included directly in the list.
     * The rest of definitions are included with an initial value of null.
     */
//     static ext_sym_map intersect_parents_reach_def(ObjectList<ext_sym_map> reach_defs_m_l, ObjectList<Edge*> entry_edges)
//     {
//         ext_sym_map result;
//
//         if (!reach_defs_m_l.empty())
//         {
//             // Get the first reach defs map which do not come from a back edge
//             // Delete the edge from the list and the corresponding entry in the map in the order to do not repeat the search in this node
//             ObjectList<ext_sym_map>::iterator itrd = reach_defs_m_l.begin();
//             ObjectList<Edge*>::iterator ite = entry_edges.begin();
//             while(itrd != reach_defs_m_l.end() && (*ite)->is_back_edge())
//             {
//                 ++itrd; ++ite;
//             }
//             ext_sym_map init_map = *itrd;
//             reach_defs_m_l.erase(itrd);
//             entry_edges.erase(ite);
//
//             // Keep those values that comes from all parents
//             for (ext_sym_map::iterator it = init_map.begin(); it != init_map.end(); ++it)
//             {
//                 Nodecl::NodeclBase first = it->first;
//                 Nodecl::NodeclBase second = it->second;
//                 ite = entry_edges.begin();
//                 ObjectList<ext_sym_map>::iterator itm = reach_defs_m_l.begin();
//                 for (; itm != reach_defs_m_l.end(); ++itm, ++ite)
//                 {
//                     if ( (*itm).find(it->first) != (*itm).end() )
//                     {   // The REACH DEF is defined in 'itm' parent
//                             Nodecl::NodeclBase second_ = (*itm)[it->first];
// //                                 std::cerr << "Combining parents with values: '" << second.prettyprint() << "' and '"
// //                                     << second_.prettyprint() << "'" << std::endl;
//                         if (!Nodecl::Utils::equal_nodecls((*itm)[it->first], it->second))
//                         {   // Values are different
//                             if ((*itm)[it->first].is<Nodecl::Range>() || it->second.is<Nodecl::Range>())
//                             {   // Some value is a range, we try to mix the values
//                                 Nodecl::NodeclBase renamed_value = CfgRenamingVisitor::combine_variable_values((*itm)[it->first], it->second);
//                                 if (renamed_value.is_null())
//                                 {
//                                     result[it->first] = Nodecl::NodeclBase::null();
//                                     break;
//                                 }
//                                 init_map[it->first] = renamed_value;
//                             }
//                             else
//                             {   // We don't know how to mix the values
//                                 result[it->first] = Nodecl::NodeclBase::null();
//                                 break;
//                             }
//                         }
//                     }
//                     else
//                     {
//                         if (!(*ite)->is_back_edge())
//                         {   // When the value is not defined but the REACH DEFS list is from a back edge node, we do not take it into account
//                             result[it->first] = Nodecl::NodeclBase::null();
//                             break;
//                         }
//                     }
//                 }
//
//                 // If we have traversed all the list, then we have to insert the value in the result map
//                 if (itm == reach_defs_m_l.end())
//                 {
//                     result[it->first] = init_map[it->first];
//                 }
//             }
//
//             // The other lists are included when the values don't exist yet in the result map
//             for (ObjectList<ext_sym_map>::iterator it = reach_defs_m_l.begin(); it != reach_defs_m_l.end(); ++it)
//             {
//                 for (ext_sym_map::iterator itm = it->begin(); itm != it->end(); ++itm)
//                 {
//                     if (result.find(itm->first) == result.end())
//                     {
//                         Nodecl::NodeclBase first = itm->first;
//                         result[itm->first] = Nodecl::NodeclBase::null();
//                     }
//                 }
//             }
//         }
//
//         return result;
//     }

//     void StaticAnalysis::extend_reaching_definitions_info(Node* node)
//     {
//         bool changes = true;
//         while (changes)
//         {
//             changes = false;
//             ExtensibleGraph::clear_visits(node);
//             propagate_reach_defs_among_nodes(node, changes);
//         }
//         ExtensibleGraph::clear_visits(node);
//
//         make_permanent_auxiliar_values(node);
//         ExtensibleGraph::clear_visits(node);
//
//         substitute_reaching_definition_known_values(node);
//     }


//     void StaticAnalysis::propagate_reach_defs_among_nodes(Node* node, bool& changes)
//     {
//         if (!node->is_visited())
//         {
//             node->set_visited(true);
//
//             Node_type ntype = node->get_type();
//
//             if (ntype == GRAPH_NODE)
//             {
//                 std::map<Symbol, Nodecl::NodeclBase> induction_vars_m;
//                 if (node->get_graph_type() == LOOP)
//                 {   // FIXME This case is only for FORstatements, not WHILE or DOWHILE
//                     propagate_reach_defs_in_for_loop_special_nodes(node);
//                     induction_vars_m = _loop_analysis->get_induction_vars_mapping(node);
//                 }
//
//                 Node* entry = node->get_graph_entry_node();
//
//                 // Compute recursively info from nodes within the graph node
//                 propagate_reach_defs_among_nodes(entry, changes);
//                 ExtensibleGraph::clear_visits(entry);
//                 make_permanent_auxiliar_values(entry);
//
//                 // Extend info to the graph node
//                 const char* filename;
//                 int line;
//                 if (node->get_graph_type() == LOOP)
//                 {
//                     Nodecl::NodeclBase label = node->get_graph_related_ast();
//                     filename = label.get_filename().c_str();
//                     line = label.get_line();
//                 }
//                 propagate_reaching_definitions_to_graph_node(node, induction_vars_m, filename, line);
//             }
//
//             // Compute reaching parents definitions
//             ext_sym_map combined_parents_reach_defs = compute_parents_reach_defs(node);
//
//             // Propagate parents info to the actual node as auxiliary values
//             ext_sym_map actual_reach_defs = node->get_reaching_definitions();
//             ext_sym_map actual_aux_reach_defs = node->get_auxiliar_reaching_definitions();
//             actual_reach_defs.insert(actual_aux_reach_defs.begin(), actual_aux_reach_defs.end());
//             for(ext_sym_map::iterator it = combined_parents_reach_defs.begin(); it != combined_parents_reach_defs.end(); ++it)
//             {
//                 Nodecl::NodeclBase first = it->first, second = it->second;
//                 if (actual_reach_defs.find(it->first) == actual_reach_defs.end())
//                 {   // Only if the definition is not performed within the node, we store the parents values
//                     node->set_auxiliar_reaching_definition(it->first, it->second);
//                     changes = true;
//                 }
//             }
//
//             // Compute info for the children
//             ObjectList<Node*> children = node->get_children();
//             for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
//             {
//                 propagate_reach_defs_among_nodes(*it, changes);
//             }
//         }
//     }

//     void StaticAnalysis::propagate_reach_defs_in_for_loop_special_nodes(Node* loop_node)
//     {   // Ranges for the Condition and Increment node are different that ranges for the code within the For Statement
//
//         // Propagate reach defs to entry node
//         Node* entry = loop_node->get_graph_entry_node();
//         ext_sym_map combined_parents_reach_defs = StaticAnalysis::compute_parents_reach_defs(loop_node);
//         ext_sym_map actual_reach_defs = entry->get_reaching_definitions();
//         for(ext_sym_map::iterator it = combined_parents_reach_defs.begin(); it != combined_parents_reach_defs.end(); ++it)
//         {
//             if (actual_reach_defs.find(it->first) == actual_reach_defs.end())
//             {   // Only if the definition is not performed within the node, we store the parents values
//                 entry->set_reaching_definition(it->first, it->second);
//             }
//         }
//
//         // Propagate reach defs to conditional node
//         Node* cond = entry->get_children()[0];
//         combined_parents_reach_defs = StaticAnalysis::compute_parents_reach_defs(cond);
//         actual_reach_defs = cond->get_reaching_definitions();
//         for(ext_sym_map::iterator it = combined_parents_reach_defs.begin(); it != combined_parents_reach_defs.end(); ++it)
//         {
//             if (actual_reach_defs.find(it->first) == actual_reach_defs.end())
//             {   // Only if the definition is not performed within the node, we store the parents values
//                 cond->set_reaching_definition(it->first, it->second);
//             }
//         }
//
//         ObjectList<Edge*> cond_exit_edges = cond->get_exit_edges();
//         ObjectList<Edge*>::iterator ite = cond_exit_edges.begin();
//
//         // Nodes through the True edge has a smaller range for the induction variable
//         Node* true_node;
//         if ((*ite)->get_type() == TRUE_EDGE)
//         {
//             true_node = (*ite)->get_target();
//         }
//         else
//         {
//             true_node = (*(ite+1))->get_target();
//         }
//         if( true_node->is_graph_node( ) )
//         {
//             true_node = true_node->get_graph_entry_node( );
//         }
//         combined_parents_reach_defs = StaticAnalysis::compute_parents_reach_defs(true_node);
//         actual_reach_defs = true_node->get_reaching_definitions();
//         std::map<Symbol, Nodecl::NodeclBase> induction_vars_m = _loop_analysis->get_induction_vars_mapping(loop_node);
//         for(ext_sym_map::iterator it = combined_parents_reach_defs.begin(); it != combined_parents_reach_defs.end(); ++it)
//         {
//             if (actual_reach_defs.find(it->first) == actual_reach_defs.end())
//             {   // Only if the definition is not performed within the node, we store the parents values
//                 // If the variable is an induction var, we get here the range of the var within the loop
//                 if (it->first.is<Nodecl::Symbol>()
//                     && induction_vars_m.find(it->first.get_symbol()) != induction_vars_m.end())
//                 {
//                     Nodecl::NodeclBase first = it->first, second = induction_vars_m[it->first.get_symbol()];
//                     true_node->set_reaching_definition(it->first, induction_vars_m[it->first.get_symbol()]);
//                 }
//                 else
//                 {
//                     Nodecl::NodeclBase first = it->first, second = it->second;
//                     true_node->set_reaching_definition(it->first, it->second);
//                 }
//             }
//         }
//     }

//     ext_sym_map StaticAnalysis::compute_parents_reach_defs(Node* node)
//     {
//         ObjectList<Edge*> entry_edges;
//         ObjectList<Node*> parents;
//         ObjectList<Edge_type> entry_edge_types;
//         if (node->get_type() == BASIC_ENTRY_NODE)
//         {   // Get info of the outer_graph parents
//             Node* outer_node = node->get_outer_node();
//             parents = outer_node->get_parents();
//             while(parents.size() == 1 && parents[0]->get_type() == BASIC_ENTRY_NODE)
//             {   // Advance over outer graphs while our parent is the entry node
//                 outer_node = outer_node->get_outer_node();
//                 parents = outer_node->get_parents();
//             }
//             entry_edges = outer_node->get_entry_edges();
//             entry_edge_types = outer_node->get_entry_edge_types();
//         }
//         else
//         {
//             parents = node->get_parents();
//             entry_edges = node->get_entry_edges();
//             entry_edge_types = node->get_entry_edge_types();
//         }
//
//         ObjectList<ext_sym_map> reach_defs;
//         for (ObjectList<Node*>::iterator it = parents.begin(); it != parents.end(); ++it)
//         {
//             ext_sym_map parent_reach_defs = (*it)->get_reaching_definitions();
//             ext_sym_map parent_aux_reach_defs = (*it)->get_auxiliar_reaching_definitions();
//             parent_reach_defs.insert(parent_aux_reach_defs.begin(), parent_aux_reach_defs.end());
//             reach_defs.append(parent_reach_defs);
//         }
//
//         return intersect_parents_reach_def(reach_defs, entry_edges);
//     }

//     void StaticAnalysis::propagate_reaching_definitions_to_graph_node(Node* node, std::map<Symbol, Nodecl::NodeclBase> induct_vars,
//                                                                     const char* filename, int line)
//     {
//         if (node->get_type() == GRAPH_NODE)
//         {   // When node is a LOOP graph, we have to look for the 'next' node of the loop
//             // otherwise, we can keep the value of the exit node
//             Node* exit_node = node->get_graph_exit_node();
//             if (node->get_graph_type() == LOOP)
//             {
//                 Node* stride = node->get_stride_node();
//
//                 ext_sym_map old_reach_defs = node->get_reaching_definitions();
//                 ext_sym_map stride_reach_defs = stride->get_reaching_definitions();
//
//                 if (old_reach_defs.empty())
//                 {   // When the graph node has no reach definition defined, we initially propagate the values of the stride node
//                     node->set_reaching_definition_list(stride_reach_defs);
//                 }
//
//                 for(ext_sym_map::iterator it = stride_reach_defs.begin(); it != stride_reach_defs.end(); ++it)
//                 {
//                     Nodecl::NodeclBase first = it->first, second = it->second;
//                     CfgRenamingVisitor renaming_v(induct_vars, filename, line);
//                     renaming_v.set_computing_range_limits(true);
//
//                     // The rhs only must be renamed when it is not accessing an array
//                     if (!is_range(first))
//                     {
//                         ObjectList<Nodecl::NodeclBase> renamed = renaming_v.walk(it->second);
//                         if (!renamed.empty())
//                         {
//                             node->set_reaching_definition(first, renamed[0]);
//                         }
//                     }
//                 }
//             }
//             else
//             {
//                 node->set_data(_REACH_DEFS, exit_node->get_reaching_definitions());
//             }
//         }
//         else
//         {
//             internal_error("Propagating reaching definitions in node '%d' with type '%s' while "
//                             "here it is mandatory a Graph node.\n",
//                         node->get_id(), node->get_type_as_string().c_str());
//         }
//     }

//     Nodecl::NodeclBase StaticAnalysis::rename_nodecl(Nodecl::NodeclBase nodecl, std::map<Symbol, Nodecl::NodeclBase> rename_map)
//     {
//         CfgRenamingVisitor renaming_v(rename_map, nodecl.get_filename().c_str(), nodecl.get_line());
//         ObjectList<Nodecl::NodeclBase> renamed = renaming_v.walk(nodecl);
//         if (!renamed.empty())
//         {
//             if (renamed.size() == 1)
//             {
// //                     std::cerr << "Renaming performed: " << nodecl.prettyprint() << " --> " << renamed[0].prettyprint() << std::endl;
//                 return renamed[0];
//             }
//             else
//             {
//                 internal_error("More than one nodecl returned while renaming nodecl ", nodecl.prettyprint().c_str());
//             }
//         }
//         else
//         {
//             return Nodecl::NodeclBase::null();
//         }
//     }

    /*!
    * This method substitute those reaching definitions based on other values which are known
    * For example:
    *      - node A Reach Defs: i = n;
    *      - node B (dependent on A) Reach Defs: i = i - 1;
    *      - We can compute value on B as: i = n - 1;
    */
//     void StaticAnalysis::substitute_reaching_definition_known_values(Node* node)
//     {
//         if (node->is_visited())
//         {
//             node->set_visited(true);
//
//             if (node->get_type() == GRAPH_NODE)
//             {
//                 Node* entry = node->get_graph_entry_node();
//                 substitute_reaching_definition_known_values(entry);
//
//                 node->set_graph_node_reaching_definitions();
//             }
//
//             Nodecl::NodeclBase renamed;
//
//             // Create the renaming map
//             ext_sym_map actual_reach_defs = node->get_reaching_definitions();
//             std::map<Symbol, Nodecl::NodeclBase> rename_map;
//             for (ext_sym_map::iterator it = actual_reach_defs.begin(); it != actual_reach_defs.end(); ++it)
//             {
//                 Nodecl::NodeclBase first = it->first;
//                 if (first.is<Nodecl::Symbol>())
//                 {
//                     rename_map.insert(std::map<Symbol, Nodecl::NodeclBase>::value_type(first.get_symbol(), it->second));
//                 }
//                 // FIXME More Nodecls can be renamed here: MemberAccess, Pointer to member
//             }
//             // Rename the reaching definitions which depend in other reaching definitions
//             for (ext_sym_map::iterator it = actual_reach_defs.begin(); it != actual_reach_defs.end(); ++it)
//             {
//                 Nodecl::NodeclBase first = it->first, second = it->second;
//                 Nodecl::NodeclBase new_first, new_second;
//                 if (first.is<Nodecl::Symbol>())
//                 {   // Nothing to be renamed in a symbol located in the left hand side
//                 }
//                 else
//                 {
//                     new_first = rename_nodecl(first, rename_map);
//                     if (!new_first.is_null())
//                     {
//                         node->rename_reaching_defintion_var(first, new_first);
//                         first = new_first;
//                     }
//                 }
//                 new_second = rename_nodecl(second, rename_map);
//                 if (!new_second.is_null())
//                 {
//                     node->set_reaching_definition(first, new_second);
//                 }
//
//             }
//
//             ObjectList<Node*> children = node->get_children();
//             for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
//             {
//                 substitute_reaching_definition_known_values(*it);
//             }
//         }
//     }
}
}
