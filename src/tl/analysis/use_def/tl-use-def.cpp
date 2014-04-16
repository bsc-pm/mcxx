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

#include <iostream>
#include <fstream>

#include "tl-analysis-utils.hpp"
#include "config.h"
#include "tl-node.hpp"
#include "tl-pcfg-visitor.hpp"      // For IPA analysis
#include "tl-rename-visitor.hpp"
#include "tl-use-def.hpp"

namespace TL {
namespace Analysis {

Optimizations::Calculator _calc;
SizeMap _pointer_to_size_map;

    // **************************************************************************************************** //
    // **************************** Class implementing use-definition analysis **************************** //

namespace {
    
    // This method removes from 'container' the sub-object represented by 'contained'
    Nodecl::NodeclBase split_var_depending_on_usage_rec(Nodecl::NodeclBase container, Nodecl::NodeclBase contained)
    {
        Nodecl::NodeclBase result;
        if( contained.is<Nodecl::ArraySubscript>( ) )
        {   // Contained[...]
            // Get the 'container' and 'contained' accesses. If not a Range, transform it into a Range
            Nodecl::NodeclBase ctr_subscript, ctd_subscript;
            Nodecl::NodeclBase subscripted;
            Nodecl::NodeclBase ctr_lb;
            Nodecl::NodeclBase ctr_ub;
            TL::Type ctr_t = container.is<Nodecl::Symbol>() ? container.get_symbol().get_type( ).no_ref() 
                                                            : container.get_type().no_ref();
            ERROR_CONDITION(!ctr_t.is_valid(), 
                            "Invalid type computed for container %s.\n", 
                            container.prettyprint().c_str());
            Nodecl::List ctd_subscripts = contained.as<Nodecl::ArraySubscript>().get_subscripts().as<Nodecl::List>();
            if(container.is<Nodecl::Analysis::RangeUnion>())
            {
                Nodecl::NodeclBase lhs = container.as<Nodecl::Analysis::RangeUnion>().get_lhs();
                Nodecl::NodeclBase rhs = container.as<Nodecl::Analysis::RangeUnion>().get_rhs();
                result = split_var_depending_on_usage_rec(lhs, contained);  // Try whether the LHS contains 'contained'
                if(Nodecl::Utils::structurally_equal_nodecls(result, lhs))  // if not, try with the RHS
                    result = split_var_depending_on_usage_rec(result, rhs);
            }
            else
            {
                if( container.is<Nodecl::ArraySubscript>( ) )
                {   // Container[...]
                    subscripted = container.as<Nodecl::ArraySubscript>( ).get_subscripted( );
                    Nodecl::List ctr_subscripts = container.as<Nodecl::ArraySubscript>( ).get_subscripts( ).as<Nodecl::List>( );
                    unsigned int i = 0;
                    unsigned int size = std::min( ctd_subscripts.size(), ctr_subscripts.size() );
                    while( i < size-1 )
                    {
                        if( !Nodecl::Utils::structurally_equal_nodecls( ctr_subscripts[i], ctd_subscripts[i] ) )
                        {
                            WARNING_MESSAGE( "Splitting usage of multidimensional arrays when dimensions other than "
                                            "the less significant accessed differs is not yet supported\n", 0 );
                            return result;
                        }
                        i++;
                    }
                    
                    ctd_subscript = ctd_subscripts[i];
                    ctr_subscript = ctr_subscripts[i];
                    
                    if( ctr_subscript.is<Nodecl::Range>( ) )
                    {   // Container[ctr_lb:ctr_ub]
                        ctr_lb = ctr_subscript.as<Nodecl::Range>().get_lower( );
                        ctr_ub = ctr_subscript.as<Nodecl::Range>().get_upper( );
                    }
                    else
                    {   // Container[ctr_index]  ->  Container[ctr_index:ctr_index]
                        ctr_lb = ctr_subscript;
                        ctr_ub = ctr_subscript;
                    }
                }
                else
                {
                    if( ctd_subscripts.size( ) > 1 )
                    {
                        WARNING_MESSAGE("Cannot split the multidimensional array access %s "
                                        "when its container expression other than an array access to the less significant dimension\n",
                                        contained.prettyprint().c_str(), container.prettyprint().c_str() );
                        return result;
                    }
                    
                    ctd_subscript = ctd_subscripts[0];
                    subscripted = container;
                    
                    if( ctr_t.is_array( ) )
                    {
                        ctr_t.array_get_bounds( ctr_lb, ctr_ub );
                    }
                    else if(ctr_t.is_pointer())
                    {
                        if(_pointer_to_size_map.find(subscripted.no_conv()) != _pointer_to_size_map.end())
                        {
                            ctr_lb = const_value_to_nodecl(const_value_get_zero(/* bytes */ 4, /* signed*/ 1));
                            ctr_ub = _pointer_to_size_map[subscripted];
                        }
                        else
                        {
                            WARNING_MESSAGE("We cannot split the usage of a pointer type because we do not know the size of the object\n", 0);
                            return result;
                        }
                    }
                }
            
                // Get the 'contained' access. If not a Range, transform it into a Range
                Nodecl::NodeclBase ctd_lb;
                Nodecl::NodeclBase ctd_ub;
                if(ctd_subscript.is<Nodecl::Range>())
                {   // Contained[ctd_lb:ctd_ub]
                    ctd_lb = ctd_subscript.as<Nodecl::Range>().get_lower();
                    ctd_ub = ctd_subscript.as<Nodecl::Range>().get_upper();
                }
                else
                {   // Contained[ctd_index]  ->  Contained[ctd_index:ctd_index]
                    ctd_lb = ctd_subscript;
                    ctd_ub = ctd_subscript;
                }
                Type ctd_subscript_t = ctd_subscript.get_type();
            
                // This case creates two ranges: [ctr_lb:ctd_lb] and [ctd_ub:ctr_ub]
                Nodecl::NodeclBase lb, ub;
                const_value_t* one = const_value_get_one(/*bytes*/ 4, /*signed*/ 1);
                Nodecl::NodeclBase one_nodecl = Nodecl::NodeclBase(const_value_to_nodecl(one));
                Nodecl::Range ctr_analysis = Nodecl::Range::make(ctr_lb.shallow_copy(), ctr_ub.shallow_copy(), 
                                                                 one_nodecl.shallow_copy(), ctd_subscript_t);
                Nodecl::Range ctd_analysis = Nodecl::Range::make(ctd_lb.shallow_copy(), ctd_ub.shallow_copy(), 
                                                                 one_nodecl.shallow_copy(), ctd_subscript_t);
                Nodecl::NodeclBase range_subtraction = Utils::range_sub(ctr_analysis, ctd_analysis);
                Nodecl::ArraySubscript new_arr_subscript;
                if(range_subtraction.is<Nodecl::Analysis::EmptyRange>())
                    return result;
                else if(range_subtraction.is<Nodecl::Range>())
                {
                    result = Nodecl::ArraySubscript::make(subscripted.shallow_copy(),
                                                          Nodecl::List::make(range_subtraction), ctr_t);
                }
                else if(range_subtraction.is<Nodecl::Analysis::RangeUnion>())
                {
                    Nodecl::NodeclBase lhs = range_subtraction.as<Nodecl::Analysis::RangeUnion>().get_lhs();
                    Nodecl::NodeclBase rhs = range_subtraction.as<Nodecl::Analysis::RangeUnion>().get_rhs();
                    result = Nodecl::Analysis::RangeUnion::make(
                        Nodecl::ArraySubscript::make(subscripted.shallow_copy(), Nodecl::List::make(lhs), ctr_t), 
                        Nodecl::ArraySubscript::make(subscripted.shallow_copy(), Nodecl::List::make(rhs), ctr_t),
                        ctr_t
                    );
                }
                else if(range_subtraction.is<Nodecl::Analysis::RangeSub>())
                {   // Assume 'contained' is actually contained in 'container', so ctr_lb<ctd_lb and ctd_ub>ctr_ub
                    if(VERBOSE && !container.is<Nodecl::Symbol>())
                    {   // If 'container' is a symbol, then any subscripted 'contained' is for sure contained in 'container'
                        WARNING_MESSAGE("Analysis is assuming %s contains %s. This may not be correct.\n", 
                                        container.prettyprint().c_str(), contained.prettyprint().c_str());
                    }
                    result = Nodecl::Analysis::RangeUnion::make(
                        Nodecl::ArraySubscript::make(subscripted.shallow_copy(), 
                                                    Nodecl::List::make(Nodecl::Range::make(ctr_lb.shallow_copy(), ctd_lb.shallow_copy(), 
                                                                                           one_nodecl.shallow_copy(), ctr_t)), 
                                                    ctr_t), 
                        Nodecl::ArraySubscript::make(subscripted.shallow_copy(),
                                                    Nodecl::List::make(Nodecl::Range::make(ctd_ub.shallow_copy(), ctr_ub.shallow_copy(), 
                                                                                           one_nodecl.shallow_copy(), ctr_t)), 
                                                    ctr_t),
                        ctr_t
                    );
                }
                else
                {
                    internal_error("Unexpected nodecl type %s when removing sub-object %s from object %s "
                                "by using Analysis::Utils::range_sub method.\n", 
                                ast_print_node_type(range_subtraction.get_kind()), contained.prettyprint().c_str(), 
                                                    container.prettyprint().c_str());
                }
            }
        }
        else if( contained.is<Nodecl::ClassMemberAccess>( ) )
        {
            TL::Type ctr_t = container.get_type( );
            if( ctr_t.is_lvalue_reference( ) )
                ctr_t = ctr_t.references_to( );
            if( ctr_t.is_class( ) )
            {   // struct t { ... }
                // Compute the nest of members accessed by 'contained'
                ObjectList<Symbol> ctd_nested_syms;
                ctd_nested_syms.append( contained.as<Nodecl::ClassMemberAccess>( ).get_member( ).get_symbol( ) );
                Nodecl::NodeclBase tmp = contained.as<Nodecl::ClassMemberAccess>( ).get_lhs( );
                while( tmp.is<Nodecl::ClassMemberAccess>( ) )
                {
                    ctd_nested_syms.append( tmp.as<Nodecl::ClassMemberAccess>( ).get_member( ).get_symbol( ) );
                    tmp = tmp.as<Nodecl::ClassMemberAccess>( ).get_lhs( );
                }

                // Travers the 'Container' adding all members that are not in the list just computed
                ObjectList<Symbol> ctr_members = ctr_t.get_all_data_members( );
                Nodecl::NodeclBase current_lhs = container;
                Nodecl::NodeclBase next_cma_lhs;
                while( !ctr_members.empty( ) )
                {
                    Symbol current_member_sym;      // We want this variable to be new at each iteration
                    for( ObjectList<Symbol>::iterator it = ctr_members.begin(); it != ctr_members.end(); ++it )
                    {
                        Nodecl::Symbol sym_n = Nodecl::Symbol::make( *it );
                        Nodecl::ClassMemberAccess new_cma = Nodecl::ClassMemberAccess::make( current_lhs.shallow_copy(), sym_n,
                                                                                             /* member-form */ Nodecl::NodeclBase::null( ), it->get_type( ) );
                        if( !ctd_nested_syms.contains( *it ) )
                        {
                            // Include the whole member, since 'Contained' is not a part of it
                            result = new_cma;
                        }
                        else
                        {
                            next_cma_lhs = new_cma;
                            current_member_sym = *it;
                        }
                    }
                    ctr_members.clear();

                    // Compute the type of the next nesting level member
                    if( current_member_sym.is_valid( ) )
                    {
                        // Get the next data members set
                        TL::Type mem_t = current_member_sym.get_type();
                        if( mem_t.is_lvalue_reference( ) )
                            mem_t = mem_t.references_to( );
                        if( mem_t.is_class()  )
                            ctr_members = mem_t.get_all_data_members();
                        // Set the new lhs for the next members
                        current_lhs = next_cma_lhs;
                    }
                }
            }
            else
            {
                WARNING_MESSAGE( "Container %s, of contained %s, has no class type. Instead it is %s\n",
                                 container.prettyprint().c_str(), contained.prettyprint().c_str(),
                                 print_declarator(ctr_t.get_internal_type()) );
                return result;
            }
        }
        else
        {
            WARNING_MESSAGE( "Unexpected type of nodecl '%s' when splitting an object into different subobjects.\n"
                             "ArraySubscript or ClassMemberAccess expected\n",
            ast_print_node_type( contained.get_kind( ) ) );
        }
        return result;
    }

    Nodecl::NodeclBase split_var_depending_on_usage(Nodecl::NodeclBase container, Nodecl::NodeclBase contained)
    {
        Nodecl::NodeclBase result = container;
        if(contained.is<Nodecl::List>())
        {
            Nodecl::List contained_list = contained.as<Nodecl::List>();
            for(Nodecl::List::iterator it = contained_list.begin(); it != contained_list.end(); ++it)
                result = split_var_depending_on_usage_rec(result, *it);
        }
        else
            result = split_var_depending_on_usage_rec(result, contained);
        return result;
    }
    
    /*!This method computes the usage of a node in two different cases:
     * - we are merging the usage of the children nodes with a parent node to set the usage of the enclosing graph node
     * - we are computing the usage of a node: since there may be more than one statement within the same node,
     *                                         we need to take into account the usage computed for the already treated statements
     */
    void propagate_usage_to_ancestors( Utils::ext_sym_set& ue_vars, Utils::ext_sym_set& killed_vars,
                                       Utils::ext_sym_set& undef_vars, const Utils::ext_sym_set& ue_children,
                                       const Utils::ext_sym_set& killed_children, const Utils::ext_sym_set& undef_children )
    {
        // Propagate the upwards exposed variables
        Nodecl::NodeclBase non_ue_vars1, non_ue_vars2;
        for( Utils::ext_sym_set::iterator it = ue_children.begin( ); it != ue_children.end( ); ++it )
        {
            Nodecl::NodeclBase n_it = it->get_nodecl( );
            // UE vars can only be upwards propagated if the are not KILLED or UNDEF in the parent
            // or they (or an enclosing nodecl) are not yet in the result set
            if( !Utils::ext_sym_set_contains_enclosing_nodecl( n_it, killed_vars ).is_null( ) ||
                !Utils::ext_sym_set_contains_enclosing_nodecl( n_it, undef_vars ).is_null( ) || 
                !Utils::ext_sym_set_contains_enclosing_nodecl( n_it, ue_vars ).is_null( ) )
                continue;

            non_ue_vars1 = Utils::ext_sym_set_contains_enclosed_nodecl( n_it, killed_vars );
            non_ue_vars2 = Utils::ext_sym_set_contains_enclosed_nodecl( n_it, undef_vars );
            if( non_ue_vars1.is_null( ) && non_ue_vars2.is_null( ) )
            {   // Neither killed nor undef var sets contain the current ue var
                Nodecl::NodeclBase tmp = Utils::ext_sym_set_contains_enclosed_nodecl( n_it, ue_vars );
                if( !tmp.is_null( ) )
                    ue_vars.erase( tmp );   // Delete the enclosed var from the list
                ue_vars.insert( *it );      // Insert the new containing var
            }
            else
            {   // Here a part of the nodecl is KILLED|UNDEF and a part is UE
                Nodecl::NodeclBase new_ue_vars1, new_ue_vars2;
                if( !non_ue_vars1.is_null( ) )
                    new_ue_vars1 = split_var_depending_on_usage( n_it, non_ue_vars1 );
                if( !non_ue_vars2.is_null( ) )
                    new_ue_vars2 = split_var_depending_on_usage( n_it, non_ue_vars2 );
                
                if( (!non_ue_vars1.is_null( ) && new_ue_vars1.is_null( )) || 
                    (!non_ue_vars2.is_null( ) && new_ue_vars2.is_null( )) )
                {   // When the two sets are empty is because the separation has not been possible
                    // Then, we set to undef the whole object and remove the partial object from the corresponding list(s)
                    if( !non_ue_vars1.is_null( ) )
                        delete_enclosed_var_from_list( non_ue_vars1, killed_vars );
                    if( !non_ue_vars2.is_null( ) )
                        delete_enclosed_var_from_list( non_ue_vars2, undef_vars );
                    undef_vars.insert( n_it );
                }
                else
                {   // new_ue_varsX may be the union of different array ranges. We may want to split the union into separated nodecls
                    if(!new_ue_vars1.is_null())
                        ue_vars.insert( Utils::ExtendedSymbol( new_ue_vars1 ) );
                    if(!new_ue_vars2.is_null())
                        ue_vars.insert( Utils::ExtendedSymbol( new_ue_vars2 ) );
                }
            }
        }

        // Propagate the killed variables
        Nodecl::NodeclBase non_killed_var;
        for( Utils::ext_sym_set::iterator it = killed_children.begin( ); it != killed_children.end( ); ++it )
        {
            Nodecl::NodeclBase n_it = it->get_nodecl( );
            if( !Utils::ext_sym_set_contains_enclosing_nodecl( n_it, undef_vars ).is_null( ) ||
                !Utils::ext_sym_set_contains_enclosing_nodecl( n_it, killed_vars ).is_null( ) )
                continue;

            non_killed_var = Utils::ext_sym_set_contains_enclosed_nodecl( n_it, undef_vars );
            if( non_killed_var.is_null( ) )
            {   // Undef var set does not contain the current killed var
                Nodecl::NodeclBase already_killed_var = Utils::ext_sym_set_contains_enclosed_nodecl( n_it, killed_vars );
                if( !already_killed_var.is_null( ) )
                    killed_vars.erase( already_killed_var );    // A part of the variable was already killed: remove the subobject
                killed_vars.insert( *it );                      // Insert the whole enclosing object
            }
            else
            {   // Here a part of the nodecl has already been marked as undefined
                Nodecl::NodeclBase new_killed_vars;
                new_killed_vars = split_var_depending_on_usage( n_it, non_killed_var );
                if( new_killed_vars.is_null( ) )
                {   // When the set is null is because the separation has not been possible
                    // Then, we set to undefined the whole object and remove the partial object from the killed list
                    Nodecl::NodeclBase already_killed_var = Utils::ext_sym_set_contains_enclosed_nodecl( n_it, killed_vars );
                    if( !already_killed_var.is_null( ) )
                        killed_vars.erase( already_killed_var );    // A part of the variable was already killed: remove the subobject
                    undef_vars.insert( *it );                       // Insert the whole enclosing object
                }
                else
                {   // Insert the computed killed parts in the corresponding list
                    // new_killed_vars may be the union of different array ranges. We may want to split the union into separated nodecls
                    killed_vars.insert( new_killed_vars );
                }
            }
        }

        // Propagate the undefined behavior variables of the children
        Nodecl::NodeclBase non_undef_var1, non_undef_var2;
        for( Utils::ext_sym_set::iterator it = undef_children.begin( ); it != undef_children.end( ); ++it )
        {
            Nodecl::NodeclBase n_it = it->get_nodecl( );
            if( !Utils::ext_sym_set_contains_enclosing_nodecl( n_it, ue_vars ).is_null( ) ||
                !Utils::ext_sym_set_contains_enclosing_nodecl( n_it, killed_vars ).is_null( ) )
                continue;

            non_undef_var1 = Utils::ext_sym_set_contains_enclosed_nodecl( n_it, ue_vars );
            non_undef_var2 = Utils::ext_sym_set_contains_enclosed_nodecl( n_it, killed_vars );
            if( non_undef_var1.is_null( ) && non_undef_var2.is_null( ) )
            {   // Neither ue nor killed  var sets contain the current ue var
                if( !Utils::ext_sym_set_contains_enclosing_nodecl( n_it, undef_vars ).is_null( ) )
                    continue;
                else
                {
                    Nodecl::NodeclBase tmp = Utils::ext_sym_set_contains_enclosed_nodecl( n_it, undef_vars );
                    if( !tmp.is_null( ) )
                        undef_vars.erase( tmp );        // Delete the enclosed var from the list
                    undef_vars.insert( *it );           // Insert the new containing var
                }
            }
            else
            {
                Nodecl::NodeclBase new_undef_vars1, new_undef_vars2;
                if( !non_undef_var1.is_null( ) )
                    new_undef_vars1 = split_var_depending_on_usage( n_it, non_undef_var1 );
                if( !non_undef_var2.is_null( ) )
                    new_undef_vars2 = split_var_depending_on_usage( n_it, non_undef_var2 );

                if( new_undef_vars1.is_null( ) && new_undef_vars2.is_null( ) )
                {   // When the two sets are null is because the separation has not been possible
                    // Then, we set to undef the whole object and remove the partial object from the corresponding list(s)
                    if( !non_undef_var1.is_null( ) )
                        delete_enclosed_var_from_list( non_undef_var1, ue_vars );
                    if( !non_undef_var2.is_null( ) )
                        delete_enclosed_var_from_list( non_undef_var2, killed_vars );
                    undef_vars.insert( n_it );
                }
                else
                {   // new_undef_varsX may be the union of different array ranges. We may want to split the union into separated nodecls
                    if(!new_undef_vars1.is_null())
                        undef_vars.insert(new_undef_vars1);
                    if(!new_undef_vars2.is_null())
                        undef_vars.insert(new_undef_vars2);
                }
            }
        }
    }

}


    UseDef::UseDef( ExtensibleGraph* graph, ObjectList<ExtensibleGraph*>* pcfgs )
        : _graph( graph ), _pcfgs( pcfgs ), _global_vars( ), _reference_params( )
    {
        // Initialized Global Variables usage to NONE
        std::set<Symbol> global_vars = _graph->get_global_variables( );
        for( std::set<Symbol>::iterator it = global_vars.begin( ); it != global_vars.end( ); ++it )
            _global_vars[*it] = Utils::UsageKind::NONE;

        // Initialized Reference parameters usage to NONE
        Symbol func_sym = graph->get_function_symbol( );
        if( func_sym.is_valid( ) )
        {   // The PCFG contains a FunctionCode
            ObjectList<TL::Symbol> params = func_sym.get_function_parameters( );
            for( ObjectList<TL::Symbol>::iterator it = params.begin( ); it != params.end( ); ++it )
            {
                Type param_type = it->get_type( );
                if( param_type.is_any_reference( ) || param_type.is_pointer( ) )
                    _reference_params[*it] = Utils::UsageKind::NONE;
            }
        }
        
        _pointer_to_size_map = graph->get_pointer_n_elements_map();
    }

    void UseDef::compute_usage( )
    {
        Node* graph = _graph->get_graph( );
        compute_usage_rec( graph );
        ExtensibleGraph::clear_visits( graph );
    }

    // Top bottom traversal
    void UseDef::compute_usage_rec( Node* current )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );
            if( current->is_exit_node( ) )
                return;

            if( current->is_graph_node( )
                && !current->is_asm_def_node( ) && !current->is_asm_op_node( ) )
            {
                // Use-def info is computed from inner nodes to outer nodes
                compute_usage_rec( current->get_graph_entry_node( ) );

                // We need to do this here because in order to propagate the tasks usage
                // to the outer nodes where they are created,
                // all children of the task_creation node must have the use-def computed
                ObjectList<Node*> inner_tasks;
                if( ExtensibleGraph::node_contains_tasks( current, current, inner_tasks ) )
                    // This set is traversed from end to start because the tasks are ordered from top to bottom and
                    // we need later tasks to be analyzed before its ancestor tasks are analyzed
                    for( ObjectList<Node*>::reverse_iterator it = inner_tasks.rbegin(); it != inner_tasks.rend(); ++it )
                        propagate_task_usage_to_task_creation_node(*it);

                // Propagate usage info from inner to outer nodes
                current->set_visited( false );
                ExtensibleGraph::clear_visits_in_level( current->get_graph_entry_node(), current );
                set_graph_node_use_def( current );
            }
            else
            {
                // Treat statements in the current node
                ObjectList<Nodecl::NodeclBase> stmts = current->get_statements( );
                UsageVisitor uv( current, _graph, _pcfgs, &_global_vars, &_reference_params );
                for( ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin( ); it != stmts.end( ); ++it )
                {
                    uv.compute_statement_usage( *it );
                }
            }

            // Compute usage form children
            ObjectList<Node*> children = current->get_children( );
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
                compute_usage_rec( *it );
        }
    }

    void UseDef::propagate_task_usage_to_task_creation_node( Node* task_creation )
    {
        // Task creation children may be: created task, task synchronization, another task creation
        Utils::ext_sym_set ue_vars = task_creation->get_ue_vars( );
        Utils::ext_sym_set killed_vars = task_creation->get_killed_vars( );
        Utils::ext_sym_set undef_vars = task_creation->get_undefined_behaviour_vars( );
        Utils::ext_sym_set child_ue_vars, child_killed_vars, child_undef_vars;

        ObjectList<Node*> children = task_creation->get_children( );
        for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
        {
            child_ue_vars = (*it)->get_ue_vars( );
            child_killed_vars = (*it)->get_killed_vars( );
            child_undef_vars = (*it)->get_undefined_behaviour_vars( );

            ue_vars.insert( child_ue_vars.begin(), child_ue_vars.end() );
            killed_vars.insert( child_killed_vars.begin(), child_killed_vars.end() );
            undef_vars.insert( child_undef_vars.begin(), child_undef_vars.end() );
        }

        // Purge the sets:
        // 1.- When the same variable appears in all three sets UE, KILLED, UNDEF
        Utils::ext_sym_set::iterator it = undef_vars.begin();
        while( it != undef_vars.end( ) )
        {
            if( !Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl(), ue_vars ).is_null() &&
                !Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl(), killed_vars ).is_null() )
            {
                undef_vars.erase( it++ );
            }
            else
                ++it;
        }
        // 2.- When a variable is UNDEF and it is either UE or KILLED, it must remain as UNDEF only
        it = ue_vars.begin();
        while( it != ue_vars.end( ) )
        {
            if( !Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl(), undef_vars ).is_null() &&
                Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl(), killed_vars ).is_null() )
                ue_vars.erase( it++ );
            else
                ++it;
        }
        it = killed_vars.begin();
        while( it != killed_vars.end( ) )
        {
            if( !Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl(), undef_vars ).is_null() &&
                Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl(), ue_vars ).is_null() )
                killed_vars.erase( it++ );
            else
                ++it;
        }
        // 3.- Set the purged sets as usage information of the task creation node
        task_creation->set_ue_var( ue_vars );
        task_creation->set_killed_var( killed_vars );
        task_creation->set_undefined_behaviour_var( undef_vars );
    }

    ObjectList<Utils::ext_sym_set> UseDef::get_use_def_over_nodes( Node* current )
    {
        ObjectList<Utils::ext_sym_set> use_def, use_def_aux;

        if( !current->is_visited( ) )
        {
            current->set_visited( true );

            // Task nodes information has already been propagated to its corresponding task_creation node
            if( !current->is_omp_task_node( ) )
            {
                // Use-Def in current node
                Utils::ext_sym_set ue_vars = current->get_ue_vars( );
                Utils::ext_sym_set killed_vars = current->get_killed_vars( );
                Utils::ext_sym_set undef_vars = current->get_undefined_behaviour_vars( );

                // Concatenate info from children nodes
                ObjectList<Node*> children = current->get_children( );
                Utils::ext_sym_set ue_children, killed_children, undef_children;
                Utils::ext_sym_set ue_task_children, killed_task_children, undef_task_children;
                for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
                {
                    use_def_aux = get_use_def_over_nodes( *it );
                    if( !use_def_aux.empty( ) )
                    {
                        ue_children = ext_sym_set_union( ue_children, use_def_aux[0] );
                        killed_children = ext_sym_set_union( killed_children, use_def_aux[1] );
                        undef_children = ext_sym_set_union( undef_children, use_def_aux[2] );
                    }
                }

                // Merge children
                merge_children_usage( ue_children, killed_children, undef_children, current->get_id( ) );

                // Merge current node and its children usage information
                propagate_usage_to_ancestors( ue_vars, killed_vars, undef_vars,
                                              ue_children, killed_children, undef_children );

                // Set the new usage information to the current node
                if( !ue_vars.empty( ) || !killed_vars.empty( ) || !undef_vars.empty( ) )
                {
                    use_def.append( ue_vars );
                    use_def.append( killed_vars );
                    use_def.append( undef_vars );
                }
            }
        }

        return use_def;
    }

    void UseDef::merge_children_usage( Utils::ext_sym_set& ue_vars, Utils::ext_sym_set& killed_vars,
                                       Utils::ext_sym_set& undef_vars, int node_id )
    {
        // Purge UNDEF vars from those vars that are in both UE and KILLED lists
        for( Utils::ext_sym_set::iterator it = ue_vars.begin( ); it != ue_vars.end( ); ++it )
        {
            if( ( !Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl( ), killed_vars ).is_null( ) ||
                  !Utils::ext_sym_set_contains_enclosed_nodecl( it->get_nodecl( ), killed_vars ).is_null( ) ) &&
                !Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl( ), undef_vars ).is_null( ) )
            {
                undef_vars.erase( *it );
            }
        }
    }

    void UseDef::set_graph_node_use_def( Node* current )
    {
        if( current->is_graph_node( ) )
        {
            if( !current->is_visited( ) )
            {
                current->set_visited( true );
                Utils::ext_sym_set ue_vars, killed_vars, undef_vars;
                ObjectList<Utils::ext_sym_set> usage = get_use_def_over_nodes( current->get_graph_entry_node( ) );
                if( !usage.empty( ) )
                {
                    ue_vars = usage[0];
                    killed_vars = usage[1];
                    undef_vars = usage[2];
                }

                Utils::ext_sym_set private_ue_vars, private_killed_vars, private_undef_vars;

                if( current->is_omp_loop_node( ) || current->is_omp_sections_node( ) || current->is_omp_single_node( ) ||
                    current->is_omp_parallel_node( ) || current->is_omp_task_node( ) )
                {   // Take into account data-sharing clauses in Use-Def Task node computation
                    Nodecl::List environ =
                            current->get_graph_related_ast( ).as<Nodecl::OpenMP::Task>( ).get_environment( ).as<Nodecl::List>( );
                    for( Nodecl::List::iterator it = environ.begin( ); it != environ.end( ); ++it )
                    {
                        if( it->is<Nodecl::OpenMP::Private>( ) )
                        {   // Remove any usage computed in the inner nodes,
                            // because is the usage of a copy of this variable
                            Nodecl::List private_syms = it->as<Nodecl::OpenMP::Private>( ).get_symbols( ).as<Nodecl::List>( );
                            for( Nodecl::List::iterator it_p = private_syms.begin( ); it_p != private_syms.end( ); ++it_p )
                            {
                                if( Utils::ext_sym_set_contains_nodecl( *it_p, undef_vars ) )
                                {
                                    undef_vars.erase( Utils::ExtendedSymbol( *it_p ) );
                                    private_undef_vars.insert( Utils::ExtendedSymbol( *it_p ) );
                                }
                                else
                                {
                                    if( Utils::ext_sym_set_contains_nodecl( *it_p, ue_vars ) )
                                    {
                                        ue_vars.erase( Utils::ExtendedSymbol( *it_p ) );
                                        private_ue_vars.insert( Utils::ExtendedSymbol( *it_p ) );
                                    }
                                    if( Utils::ext_sym_set_contains_nodecl( *it_p, killed_vars ) )
                                    {
                                        killed_vars.erase( Utils::ExtendedSymbol( *it_p ) );
                                        private_killed_vars.insert( Utils::ExtendedSymbol( *it_p ) );
                                    }
                                }
                            }
                        }
                        if( it->is<Nodecl::OpenMP::Firstprivate>( ) )
                        {   // This variable is Upper Exposed in the task
                            Nodecl::List firstprivate_syms = it->as<Nodecl::OpenMP::Firstprivate>( ).get_symbols( ).as<Nodecl::List>( );
                            for( Nodecl::List::iterator it_fp = firstprivate_syms.begin( ); it_fp != firstprivate_syms.end( ); ++it_fp )
                            {
                                if( Utils::ext_sym_set_contains_nodecl( *it_fp, undef_vars ) )
                                {
                                    undef_vars.erase( Utils::ExtendedSymbol( *it_fp ) );
                                    private_undef_vars.insert( Utils::ExtendedSymbol( *it_fp ) );
                                }
                                else if( Utils::ext_sym_set_contains_nodecl( *it_fp, killed_vars ) )
                                {
                                    killed_vars.erase( Utils::ExtendedSymbol( *it_fp ) );
                                    private_killed_vars.insert( Utils::ExtendedSymbol( *it_fp ) );
                                }
                            }
                        }
                    }
                }

                current->add_ue_var( ue_vars );
                current->add_killed_var( killed_vars );
                current->add_undefined_behaviour_var( undef_vars );

                current->add_private_ue_var( private_ue_vars );
                current->add_private_killed_var( private_killed_vars );
                current->add_private_undefined_behaviour_var( private_undef_vars );
            }
        }
        else
        {
            internal_error( "Cannot propagate use-def info from inner nodes to outer nodes "\
                            "in node '%d' with type '%s'. GRAPH_NODE expected\n",
                            current->get_id( ), current->get_type_as_string( ).c_str( ) );
        }
    }

    // ************************** End class implementing use-definition analysis ************************** //
    // **************************************************************************************************** //



    // **************************************************************************************************** //
    // ***************************** Class implementing use-definition visitor **************************** //

namespace {
     void get_use_def_variables( Node* actual, int id_target_node,
                                 Utils::ext_sym_set &ue_vars, Utils::ext_sym_set &killed_vars,
                                 Utils::ext_sym_set &undef_vars )
    {
        ObjectList<Node*> children = actual->get_children( );
        for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
        {
            if( ( *it )->get_id( ) != id_target_node )
            {
                ue_vars = ext_sym_set_union( ue_vars, ( *it )->get_ue_vars( ) );
                killed_vars = ext_sym_set_union( killed_vars, ( *it )->get_killed_vars( ) );
                undef_vars = ext_sym_set_union( undef_vars, ( *it )->get_undefined_behaviour_vars( ) );

                get_use_def_variables( *it, id_target_node, ue_vars, killed_vars, undef_vars );
            }
        }
    }

    sym_to_nodecl_map map_reference_params_to_args( ObjectList<TL::Symbol> parameters,
                                                    Nodecl::List arguments )
    {
        sym_to_nodecl_map ref_params_to_args;

        ObjectList<TL::Symbol>::iterator itp = parameters.begin( );
        Nodecl::List::iterator ita = arguments.begin( );

        //TODO: parameters.size() must be == to arguments.size()
        for( ; ( ita != arguments.end( ) ) && ( itp != parameters.end( ) ); ++itp, ++ita )
        {
            Type param_type = itp->get_type( );
            if( ( param_type.is_any_reference( ) || param_type.is_pointer( ) ) )
                ref_params_to_args[*itp] = *ita;
        }

        return ref_params_to_args;
    }

    sym_to_nodecl_map map_non_reference_params_to_args( ObjectList<TL::Symbol> parameters,
                                                        Nodecl::List arguments )
    {
        sym_to_nodecl_map non_ref_params_to_args;

        ObjectList<TL::Symbol>::iterator itp = parameters.begin( );
        Nodecl::List::iterator ita = arguments.begin( );

        //TODO: parameters.size() must be == to arguments.size()
        for( ; ( ita != arguments.end( ) ) && ( itp != parameters.end( ) ); ++itp, ++ita )
        {
            Type param_type = itp->get_type( );
            if( !param_type.is_any_reference( ) && !param_type.is_pointer( ) )
            {
                // If some memory access in the argument is a symbol, then we add the tuple to the map
                ObjectList<Nodecl::NodeclBase> obj = Nodecl::Utils::get_all_memory_accesses( *ita );
                for( ObjectList<Nodecl::NodeclBase>::iterator it = obj.begin( ); it != obj.end( ); ++it )
                {
                    if( !it->is_constant( ) )
                    {
                        non_ref_params_to_args[*itp] = *ita;
                        break;
                    }
                }
            }
        }

        return non_ref_params_to_args;
    }
}

    UsageVisitor::UsageVisitor( Node* fake_node )
        : _node( fake_node ), _define( false ), _current_nodecl( Nodecl::NodeclBase::null( ) ),
          _global_vars( ), _reference_params( ), _avoid_func_calls( false ), _pcfg( NULL ), _pcfgs( )
    {}

    UsageVisitor::UsageVisitor( Node* n,
                                ExtensibleGraph* pcfg,
                                ObjectList<ExtensibleGraph*>* pcfgs,
                                std::map<Symbol, Utils::UsageKind>* global_vars,
                                std::map<Symbol, Utils::UsageKind>* reference_params )
        : _node( n ), _define( false ), _current_nodecl( Nodecl::NodeclBase::null( ) ),
          _global_vars( global_vars ), _reference_params( reference_params ),
          _avoid_func_calls( false ), _pcfg( pcfg ), _pcfgs( pcfgs )
    {}

    void UsageVisitor::set_var_usage_to_node( const Utils::ExtendedSymbol& var, Utils::UsageKind usage_kind )
    {
        Utils::ext_sym_set ue_vars = _node->get_ue_vars( );
        Utils::ext_sym_set killed_vars = _node->get_killed_vars( );
        Utils::ext_sym_set undef_vars = _node->get_undefined_behaviour_vars( );
        Utils::ext_sym_set empty_set;
        if( usage_kind._usage_type & Utils::UsageKind::USED )
        {
            Utils::ext_sym_set ue_tmp; ue_tmp.insert( var );
            propagate_usage_to_ancestors( ue_vars, killed_vars, undef_vars, ue_tmp, empty_set, empty_set );
            _node->set_ue_var( ue_vars );                   // Replace the set of upwards exposed variables associated to the node
        }
        else if( usage_kind._usage_type & Utils::UsageKind::DEFINED )
        {
            Utils::ext_sym_set killed_tmp; killed_tmp.insert( var );
            propagate_usage_to_ancestors( ue_vars, killed_vars, undef_vars, empty_set, killed_tmp, empty_set );
            _node->set_killed_var( killed_vars );               // Replace the set of killed vars associated to the node
        }
        else
        {
            Utils::ext_sym_set undef_tmp; undef_tmp.insert( var );
            propagate_usage_to_ancestors( ue_vars, killed_vars, undef_vars, empty_set, empty_set, undef_tmp );
            _node->set_undefined_behaviour_var( undef_vars );   // Replace the set of undefined behavior vars associated to the node
        }
    }

    void UsageVisitor::set_var_usage_to_node( const Utils::ext_sym_set& var_set, Utils::UsageKind usage_kind )
    {
        for( Utils::ext_sym_set::const_iterator it = var_set.begin( ); it != var_set.end( ); ++it )
            set_var_usage_to_node( *it, usage_kind );
    }

    void UsageVisitor::compute_statement_usage( Nodecl::NodeclBase st )
    {
        Node* outer_node = _node->get_outer_node( );
        if( outer_node->is_split_statement( ) && !_node->is_function_call_node( ) )
        {   // The function calls that can appear in the split statement have already been analyzed
            // We want to avoid computing the usage again. In exchange, we want to propagate the previously compute usage
            // F.i.:   int c = foo(a, b)
            //         PCFG:
            //           ______________________________________________
            //          |  [SPLIT_STMT]                                |
            //          |  __________________________________________  |
            //          | | [FUNC_CALL]                              | |
            //          | |  _______       ___________       ______  | |
            //          | | |       |     |           |     |      | | |
            //          | | | ENTRY |---->| foo(a, b) |---->| EXIT | | |
            //          | | |_______|     |___________|     |______| | |
            //          | |__________________________________________| |
            //          |               _______|_______                |
            //          |              |               |               |
            //          |              | c = foo(a, b) |               |
            //          |              |_______________|               |
            //          |______________________________________________|
            //
            //         When computing Use-Def of "c = foo(a, b)", we want to propagate
            //             the info calculated for "b=foo(a, b)" regarding to the function call
            ObjectList<Node*> parents = _node->get_parents( );
            while( !parents.empty( ) && !parents[0]->is_entry_node( ) )
            {
                ERROR_CONDITION( parents.size( ) != 1,
                                 "Ancestors of a non function call node which are inside the enclosing split statement "\
                                 "must not have any sibling, but we have found %d siblings", parents.size( ) );

                _node->set_ue_var( parents[0]->get_ue_vars( ) );
                _node->set_killed_var( parents[0]->get_killed_vars( ) );
                _node->set_undefined_behaviour_var( parents[0]->get_undefined_behaviour_vars( ) );

                parents = parents[0]->get_parents( );
            }

            _avoid_func_calls = true;
        }

        walk( st );
    }

    void UsageVisitor::unhandled_node( const Nodecl::NodeclBase& n )
    {
        nodecl_t internal_n = n.get_internal_nodecl( );
        WARNING_MESSAGE( "Unhandled node '%s' with type '%s' during Use-Def Analysis",
                         codegen_to_str( internal_n, nodecl_retrieve_context( internal_n ) ),
                         ast_print_node_type( n.get_kind( ) ) );
    }

    template<typename T>
    void UsageVisitor::visit_assignment( const T& n )
    {
        _define = false;
        walk( n.get_rhs( ) );
        _define = true;
        walk( n.get_lhs( ) );
        _define = false;
    }

    template<typename T>
    void UsageVisitor::visit_binary_assignment( const T& n )
    {
        // Traverse the use of both the lhs and the rhs
        walk( n.get_rhs( ) );
        walk( n.get_lhs( ) );

        // Traverse the definition of the lhs
        _define = true;
        walk( n.get_lhs( ) );
        _define = false;
    }

    void UsageVisitor::parse_parameter( std::string current_param, const Nodecl::NodeclBase& arg )
    {
        size_t first_slash_pos = current_param.find( "#" );
        if( first_slash_pos != std::string::npos )
        {   // Parameter is pointer
            // The address is used
            set_var_usage_to_node( Utils::ExtendedSymbol( arg ), Utils::UsageKind::USED );
            size_t second_slash_pos = current_param.find( "#", first_slash_pos );
            std::string pointed_param_usage = current_param.substr( first_slash_pos, second_slash_pos - first_slash_pos );
            // TODO: What do we want to do with the pointed value??
        }
        else
        {
            ObjectList<Nodecl::NodeclBase> obj = Nodecl::Utils::get_all_memory_accesses( arg );
            for( ObjectList<Nodecl::NodeclBase>::iterator it_o = obj.begin( ); it_o != obj.end( ); ++it_o )
            {
                // Set all arguments as upper exposed
                Symbol s( it_o->get_symbol( ) );
                if( !s.is_valid( ) )
                {   // ArraySubscript and ClassMemberAccess are memory accesses but do not have a symbol associated
                    // In these cases, we need to get the nodecl base
                    s = Utils::get_nodecl_base( *it_o ).get_symbol( );
                    ERROR_CONDITION( !s.is_valid( ),
                                     "A memory access must have a symbol associated, but %s does not have", it_o->prettyprint( ).c_str( ) );
                }
                set_var_usage_to_node( Utils::ExtendedSymbol( *it_o ), Utils::UsageKind::USED );
                if( ( _global_vars->find( s ) != _global_vars->end( ) ) &&
                    (*_global_vars)[s]._usage_type & Utils::UsageKind::NONE )
                {
                    (*_global_vars)[s] = Utils::UsageKind::USED;
                }
                else if( ( _reference_params->find( s ) != _reference_params->end( ) ) &&
                         ( (*_reference_params)[s]._usage_type & Utils::UsageKind::NONE ) )
                {
                    (*_reference_params)[s] = Utils::UsageKind::USED;
                }
            }
        }
    }

    bool UsageVisitor::parse_c_functions_file( Symbol func_sym, const Nodecl::List& args )
    {
        bool side_effects = true;

        std::string cLibFuncsPath = std::string( MCXX_ANALYSIS_DATA_PATH ) + "/cLibraryFunctionList" ;
        std::ifstream cLibFuncs( cLibFuncsPath.c_str( ) );
        if( cLibFuncs.is_open( ) )
        {
            std::string func_decl;
            while( cLibFuncs.good( ) )
            {
                getline( cLibFuncs, func_decl );
                if( func_decl.substr( 0, 2 ) != "//" )
                {
                    size_t open_parenth_pos = func_decl.find( "(" );
                    std::string func_name = func_decl.substr( 0, open_parenth_pos - 1 );
                    if( func_sym.get_name( ) == func_name )
                    {   // No global variable is read / written
                        // Check for parameters usage
                        side_effects = false;

                        size_t comma_pos = func_decl.find( "," );
                        if( comma_pos == std::string::npos )
                        {
                            comma_pos = func_decl.find( ")" );
                        }
                        size_t last_comma_pos = open_parenth_pos + 1;
                        std::string current_param;
                        Nodecl::List::iterator it = args.begin( );
                        while( comma_pos != std::string::npos && /* not a default parameter*/ it != args.end( ) )
                        {
                            current_param = func_decl.substr( last_comma_pos, comma_pos - last_comma_pos );
                            parse_parameter( current_param, *it );
                            it++;
                            last_comma_pos = comma_pos + 1;
                            comma_pos = func_decl.find( ",", last_comma_pos );
                        }
                        // Last parameter
                        if( it != args.end( ) )
                        {
                            current_param = func_decl.substr( last_comma_pos, func_decl.find( ")", last_comma_pos ) - last_comma_pos );
                            if( current_param == "..." )
                            {   // Arguments are supposed to be only used
                                ObjectList<Nodecl::NodeclBase> obj;
                                while( it != args.end( ) )
                                {
                                    obj = Nodecl::Utils::get_all_memory_accesses( *it );
                                    for( ObjectList<Nodecl::NodeclBase>::iterator it_o = obj.begin( ); it_o  != obj.end( ); ++it_o )
                                        set_var_usage_to_node( Utils::ExtendedSymbol( *it_o ), Utils::UsageKind::USED );
                                    ++it;
                                }
                            }
                            else
                            {
                                parse_parameter( current_param, *it );
                            }
                        }
                    }
                }
            }

            if( side_effects && VERBOSE )
            {
                WARNING_MESSAGE( "Function's '%s' code not reached. \nUsage of global variables and "\
                                  "reference parameters will be limited. \nIf you know the side effects of this function, "\
                                  "add it to the file and recompile your code. \n(If you recompile the compiler, "\
                                  "you want to add the function in $MCC_HOME/src/tl/analysis/use_def/cLibraryFunctionList instead).",
                                  func_sym.get_name( ).c_str( ), cLibFuncsPath.c_str( ) );
            }
            cLibFuncs.close();
        }
        else
        {
            WARNING_MESSAGE( "File containing C library calls Usage info cannot be opened. \n"\
                             "Path tried: '%s'", cLibFuncsPath.c_str( ) );
        }

        return side_effects;
    }

    static ExtensibleGraph* find_graph_in_list_from_function_symbol( Symbol func_sym, ObjectList<ExtensibleGraph*>* pcfgs )
    {
        ExtensibleGraph* result = NULL;
        for( ObjectList<ExtensibleGraph*>::iterator it = pcfgs->begin( ); it != pcfgs->end( ); ++it )
        {
            Symbol s( ( *it )->get_function_symbol( ) );
            if( s.is_valid( ) && ( s == func_sym ) )
            {
                result = *it;
                break;
            }
        }
        return result;
    }

    static bool nodecl_is_dereference( const Nodecl::NodeclBase& n )
    {
        bool result = false;
        if( n.is<Nodecl::Dereference>( ) )
            result = true;
        else if( n.is<Nodecl::Cast>( ) )
            result = nodecl_is_dereference( n.as<Nodecl::Cast>( ).get_rhs( ) );
        else if( n.is<Nodecl::Conversion>( ) )
            result = nodecl_is_dereference( n.as<Nodecl::Conversion>( ).get_nest( ) );
        return result;
    }

    static sym_to_nodecl_map map_params_to_args( const ObjectList<TL::Symbol>& parameters,
                                                 const Nodecl::List& arguments )
    {
        sym_to_nodecl_map result;
        int n_iters = std::min( arguments.size( ), parameters.size( ) );
        if( n_iters > 0 )
        {
            Nodecl::List::const_iterator ita = arguments.begin( );
            ObjectList<TL::Symbol>::const_iterator itp = parameters.begin( );
            int i;
            for( i = 0; i < n_iters; ++i  )
            {
                result[*itp] = *ita;
                ita++; itp++;
            }
        }
        return result;
    }

    static Nodecl::NodeclBase rename_param_usage_to_argument( const Nodecl::NodeclBase& n,
                                                              const ObjectList<TL::Symbol>& parameters,
                                                              const Nodecl::List& arguments )
    {
        sym_to_nodecl_map param_to_arg_map = map_params_to_args( parameters, arguments );
        Nodecl::NodeclBase var_copy = n.shallow_copy( );
        RenameVisitor rv( param_to_arg_map );
        rv.rename_expressions( var_copy );
        return var_copy;
    }

    Utils::ext_sym_set UsageVisitor::get_ipa_usage( Utils::UsageKind usage_kind, const Utils::ext_sym_set& list,
                                                    const Nodecl::List& arguments, const TL::Symbol& func_sym )
    {
        Utils::ext_sym_set result;
        ObjectList<TL::Symbol> parameters = func_sym.get_function_parameters( );
        for( Utils::ext_sym_set::iterator it = list.begin( ); it != list.end( ); ++it )
        {
            Nodecl::NodeclBase it_nodecl = it->get_nodecl( );
            Nodecl::NodeclBase var = Utils::get_nodecl_base( it_nodecl );
            Symbol s( var.get_symbol( ) );
            ERROR_CONDITION( !s.is_valid( ),
                             "The base nodecl of an extended symbol must have a symbol associated, but %s does not have one",
                             it_nodecl.prettyprint( ).c_str( ) );
            if( _global_vars->find( s ) != _global_vars->end( ) )
            {
                Nodecl::NodeclBase var_copy = rename_param_usage_to_argument( it_nodecl, parameters, arguments );
                result.insert( var_copy );
                (*_global_vars)[s] = usage_kind;
            }
            else
            {
                ObjectList<TL::Symbol> var_param = parameters.find( s );
                if( !var_param.empty( ) )
                {
                    Type var_param_type = var_param[0].get_type( );
                    if( var_param_type.is_any_reference( ) || var_param_type.is_pointer( ) )
                    {
                        // Find the matching between arguments and parameters
                        int n_iters = std::min( arguments.size( ), parameters.size( ) );
                        if( n_iters > 0 )
                        {
                            Nodecl::List::const_iterator ita = arguments.begin( );
                            ObjectList<TL::Symbol>::const_iterator itp = parameters.begin( );
                            int i;
                            for( i = 0; i < n_iters; ++i  )
                            {
                                if( *itp == var_param[0] )
                                    break;
                                ita++; itp++;
                            }
                            ERROR_CONDITION( i > n_iters, "Parameter %s not found in function %s\n",
                                             itp->get_name( ).c_str( ), func_sym.get_name( ).c_str( ) );

                            Type param_type = itp->get_type( );
                            if( param_type.is_any_reference( ) )
                            {   // type & variable
                                ERROR_CONDITION( !ita->get_symbol( ).is_valid( ), "Invalid argument %s passed by reference",
                                                                               ita->prettyprint( ).c_str( ) );
                                Nodecl::NodeclBase var_copy = rename_param_usage_to_argument( it_nodecl, parameters, arguments );
                                result.insert( Utils::ExtendedSymbol( var_copy ) );
                                (*_reference_params)[s] = usage_kind;
                            }
                            else if( param_type.is_pointer( ) )
                            {   // type* variable
                                // Replace the used var by the argument used in the call
                                Nodecl::NodeclBase var_copy = rename_param_usage_to_argument( it_nodecl, parameters, arguments );
                                if( usage_kind._usage_type & Utils::UsageKind::DEFINED ) // The list being treated is a Kill list
                                {
                                    if( nodecl_is_dereference( it_nodecl ) )   // *variable = ...
                                    {
                                        result.insert( Utils::ExtendedSymbol( var_copy ) );
                                        (*_reference_params)[s] = usage_kind;
                                    }
                                    else
                                    {}  // variable = ...
                                        // Nothing to be done because the address cannot be changed
                                }
                                else
                                {
                                    result.insert( Utils::ExtendedSymbol( var_copy ) );
                                    (*_reference_params)[s] = usage_kind;
                                }
                            }
                            else
                            {} // Nothing tobe done because the function uses a copy of the argument
                        }
                    }
                }
            }
        }
        return result;
    }

    static std::map<Symbol, Utils::UsageKind> set_of_symbols_to_usage_map( const std::set<Symbol>& s )
    {
        std::map<Symbol, Utils::UsageKind> result;
        for( std::set<Symbol>::iterator it = s.begin( ); it != s.end( ); ++it )
            result[*it] = Utils::UsageKind::NONE;
        return result;
    }

    void UsageVisitor::function_visit( const Nodecl::NodeclBase& called_sym, const Nodecl::List& arguments )
    {
        if( _avoid_func_calls )
            return;

        // The function called must be analyzed only in case it has not been analyzed previously
        TL::Symbol func_sym = called_sym.get_symbol( );
        if( func_sym.is_valid( ) )
        {   // The called function is not a pointer to function
            ObjectList<TL::Symbol> params = func_sym.get_function_parameters( );
            ExtensibleGraph* called_pcfg = find_graph_in_list_from_function_symbol( func_sym, _pcfgs );
            if( called_pcfg != NULL )
            {   // Called function code is reachable
                if( called_pcfg->usage_is_computed( ) )
                {
                    // Propagate values that have been computed in the called graph
                    // mapping parameters usage into arguments
                    Node* pcfg_node = called_pcfg->get_graph( );
                    std::set<Symbol> call_graph_global_vars_set = called_pcfg->get_global_variables( );
                    _pcfg->set_global_vars( call_graph_global_vars_set );
                    std::map<Symbol, Utils::UsageKind> call_graph_global_vars_map = set_of_symbols_to_usage_map( call_graph_global_vars_set );
                    _global_vars->insert( call_graph_global_vars_map.begin( ), call_graph_global_vars_map.end( ) );
                    Utils::ext_sym_set ue_vars = get_ipa_usage( Utils::UsageKind::USED, pcfg_node->get_ue_vars( ),
                                                                arguments, func_sym );
                    Utils::ext_sym_set killed_vars = get_ipa_usage( Utils::UsageKind::DEFINED, pcfg_node->get_killed_vars( ),
                                                                    arguments, func_sym );
                    Utils::ext_sym_set undef_vars = get_ipa_usage( Utils::UsageKind::UNDEFINED, pcfg_node->get_undefined_behaviour_vars( ),
                                                                   arguments, func_sym );

                    // Add the usage of the arguments, since they are, at least, read
                    UsageVisitor uv( _node, _pcfg, _pcfgs, _global_vars, _reference_params );
                    for( Nodecl::List::const_iterator it = arguments.begin( ); it != arguments.end( ); ++it )
                    {
                        Nodecl::NodeclBase it_nodecl = it->no_conv();
                        while( it_nodecl.is<Nodecl::Cast>( ) )
                            it_nodecl = it_nodecl.as<Nodecl::Cast>( ).get_rhs( );
                        if( !it_nodecl.is<Nodecl::Symbol>( ) || 
                            !it_nodecl.get_symbol( ).is_valid( ) || 
                            !it_nodecl.get_symbol( ).is_function( ) )
                        {
                            uv.compute_statement_usage( *it );
                        }
                    }

                    set_var_usage_to_node( ue_vars, Utils::UsageKind::USED );
                    set_var_usage_to_node( killed_vars, Utils::UsageKind::DEFINED );
                    set_var_usage_to_node( undef_vars, Utils::UsageKind::UNDEFINED );
                }
                else
                {   // We are arguments are used when calling the function
                    for( Nodecl::List::const_iterator it = arguments.begin( ); it != arguments.end( ); ++it )
                    {
                        ObjectList<Nodecl::NodeclBase> obj = Nodecl::Utils::get_all_memory_accesses( *it );
                        for( ObjectList<Nodecl::NodeclBase>::iterator it_o = obj.begin( ); it_o != obj.end( ); ++it_o )
                            _node->add_ue_var( Utils::ExtendedSymbol( *it_o ) );
                    }

                    // Check for the usage in the graph of the function to propagate Usage (Global variables and reference parameters)
                    // until the point we are currently
                    for( std::map<Symbol, Utils::UsageKind>::iterator it = _global_vars->begin( ); it != _global_vars->end( ); ++it )
                    {
                        Nodecl::NodeclBase sym = Nodecl::Symbol::make( it->first );
                        if( it->second._usage_type & Utils::UsageKind::UNDEFINED )
                            _node->add_undefined_behaviour_var( Utils::ExtendedSymbol( sym ) );
                        else
                        {
                            if( it->second._usage_type & Utils::UsageKind::USED )
                                _node->add_ue_var( Utils::ExtendedSymbol( sym ) );
                            if( it->second._usage_type & Utils::UsageKind::DEFINED )
                                _node->add_killed_var( Utils::ExtendedSymbol( sym ) );
                        }
                    }
                    for( std::map<Symbol, Utils::UsageKind>::iterator it = _reference_params->begin( );
                         it != _reference_params->end( ); ++it )
                    {
                        Nodecl::NodeclBase sym = Nodecl::Symbol::make( it->first );
                        if( it->second._usage_type & Utils::UsageKind::UNDEFINED )
                            _node->set_undefined_behaviour_var( Utils::ExtendedSymbol( sym ) );
                        else
                        {
                            if( it->second._usage_type & Utils::UsageKind::USED )
                                _node->add_ue_var( Utils::ExtendedSymbol( sym ) );
                            if( it->second._usage_type & Utils::UsageKind::DEFINED )
                                _node->add_killed_var( Utils::ExtendedSymbol( sym ) );
                        }
                    }
                }
            }
            else
            {   // Called function code is not reachable
                // Check whether we have enough attributes in the function symbol
                // to determine the function side effects
                bool side_effects = true;

                if( func_sym.has_gcc_attributes( ) )
                {   // Check for information synthesized by gcc
                    ObjectList<GCCAttribute> gcc_attrs = func_sym.get_gcc_attributes( );
                    for( ObjectList<GCCAttribute>::iterator it = gcc_attrs.begin( );
                            it != gcc_attrs.end( ); ++it )
                    {
                        std::string attr_name = it->get_attribute_name( );
                        if( attr_name == "const" || attr_name == "pure" )
                        {   // No side effects except the return value.
                            // Only examine the arguments ( and global variables in 'pure' case)
                            side_effects = false;

                            Utils::ext_sym_set ue_vars;
                            // Set all parameters as used ( if not previously killed or undefined )
                            for( Nodecl::List::iterator it_arg = arguments.begin( ); it_arg != arguments.end( ); ++it_arg )
                            {
                                Utils::ExtendedSymbol es( *it_arg );
                                if( _node->get_killed_vars( ).find( es ) == _node->get_killed_vars( ).end( )
                                    && _node->get_undefined_behaviour_vars( ).find( es ) == _node->get_undefined_behaviour_vars( ).end( ) )
                                {
                                    ue_vars.insert( es );
                                }
                            }

                            if( attr_name == "pure" )
                            {   // Set all global variables variables as upper exposed ( if not previously killed or undefined )
                                for( std::map<Symbol, Utils::UsageKind>::iterator it_g = _global_vars->begin( );
                                     it_g != _global_vars->end( ); ++it_g )
                                {
                                    if( it_g->second._usage_type & Utils::UsageKind::NONE )
                                    {
                                        ue_vars.insert( Utils::ExtendedSymbol( Nodecl::Symbol::make( it_g->first ) ) );
                                        it_g->second._usage_type = Utils::UsageKind::USED;
                                    }
                                }
                            }
                            _node->add_ue_var( ue_vars );
                            if( attr_name == "pure" )
                                break;
                        }
                    }
                }

                if( side_effects )
                {
                    // Check in Mercurium function attributes data-base
                    side_effects = parse_c_functions_file( func_sym, arguments );

                    // Still cannot determine which are the side effects of the function...
                    if( side_effects )
                    {
                        if( func_sym.get_type( ).lacks_prototype( ) )
                        {   // All parameters are passed by value
                            for( Nodecl::List::iterator it = arguments.begin( ); it != arguments.end( ); ++it )
                                if( !it->is_constant( ) )
                                {
                                    ObjectList<Nodecl::NodeclBase> mem_access = Nodecl::Utils::get_all_memory_accesses( *it );
                                    for( ObjectList<Nodecl::NodeclBase>::iterator ita = mem_access.begin( );
                                        ita != mem_access.end( ); ++ita )
                                    {
                                        _node->add_ue_var( Utils::ExtendedSymbol( *ita ) );
                                        if( ita->get_type( ).is_pointer( ) )
                                        {
                                            Nodecl::Dereference pointed_var = Nodecl::Dereference::make( *ita, ita->get_type( ) );
                                            _node->set_undefined_behaviour_var( Utils::ExtendedSymbol( pointed_var ) );
                                        }
                                    }
                                }
                        }
                        else
                        {
                            Utils::ext_sym_set killed = _node->get_killed_vars( );
                            Utils::ext_sym_set undef = _node->get_undefined_behaviour_vars( );

                            // Set all reference parameters to undefined
                            sym_to_nodecl_map ref_params = map_reference_params_to_args( params, arguments );
                            for( sym_to_nodecl_map::iterator it = ref_params.begin( ); it != ref_params.end( ); ++it )
                            {
                                if( Nodecl::Utils::nodecl_is_modifiable_lvalue( it->second ) &&
                                    Utils::ext_sym_set_contains_enclosing_nodecl( it->second, killed ).is_null( ) &&
                                    Utils::ext_sym_set_contains_enclosed_nodecl( it->second, killed ).is_null( ) )
                                {
                                    _node->add_undefined_behaviour_var_and_recompute_use_and_killed_sets(
                                        Utils::ExtendedSymbol( it->second ) );
                                }
                            }

                            // Set the value passed parameters as upper exposed
                            sym_to_nodecl_map non_ref_params = map_non_reference_params_to_args( params, arguments );
                            for( sym_to_nodecl_map::iterator it = non_ref_params.begin( ); it != non_ref_params.end( ); ++it )
                            {
                                ObjectList<Nodecl::NodeclBase> obj = Nodecl::Utils::get_all_memory_accesses( it->second );
                                for( ObjectList<Nodecl::NodeclBase>::iterator it_o = obj.begin( ); it_o != obj.end( ); ++it_o )
                                {
                                    if( Utils::ext_sym_set_contains_enclosing_nodecl( it->second, killed ).is_null( ) &&
                                        Utils::ext_sym_set_contains_enclosed_nodecl( it->second, killed ).is_null( ) &&
                                        Utils::ext_sym_set_contains_enclosing_nodecl( it->second, undef ).is_null( ) &&
                                        Utils::ext_sym_set_contains_enclosed_nodecl( it->second, undef ).is_null( ) )
                                    {
                                        _node->add_ue_var( Utils::ExtendedSymbol( *it_o ) );
                                    }
                                }

                            }

                            // Set all global variables to undefined
                            for( std::map<Symbol, Utils::UsageKind>::iterator it = _global_vars->begin( );
                                 it != _global_vars->end( ); ++it )
                            {
                                Nodecl::NodeclBase sym = Nodecl::Symbol::make( it->first );
                                if( Utils::ext_sym_set_contains_enclosing_nodecl( sym, killed ).is_null( ) &&
                                    Utils::ext_sym_set_contains_enclosed_nodecl( sym, killed ).is_null( ) )
                                {
                                    _node->add_undefined_behaviour_var_and_recompute_use_and_killed_sets( Utils::ExtendedSymbol( sym ) );
                                    it->second._usage_type = Utils::UsageKind::UNDEFINED;
                                }
                            }
                        }
                    }
                }
            }
        }
        else
        {   // Calling a pointer to function: neither code nor prototype are reachable, thus:
            // - all parameters as undefined behavior, we do not know whether they are passed by value or by reference
            // - all global variables as undefined behavior
            Utils::ext_sym_set killed = _node->get_killed_vars( );
            for( Nodecl::List::iterator it = arguments.begin( ); it != arguments.end( ); ++it )
            {
                if( Utils::ext_sym_set_contains_enclosing_nodecl( *it, killed ).is_null( ) &&
                    Utils::ext_sym_set_contains_enclosed_nodecl( *it, killed ).is_null( ) )
                {
                    _node->set_undefined_behaviour_var( Utils::ExtendedSymbol( *it ) );
                }
            }

            for( std::map<Symbol, Utils::UsageKind>::iterator it = _global_vars->begin( ); it != _global_vars->end( ); ++it )
            {
                Nodecl::NodeclBase sym = Nodecl::Symbol::make( it->first );
                if( Utils::ext_sym_set_contains_enclosing_nodecl( sym, killed ).is_null( ) &&
                    Utils::ext_sym_set_contains_enclosed_nodecl( sym, killed ).is_null( ) )
                {
                    _node->add_undefined_behaviour_var_and_recompute_use_and_killed_sets( Utils::ExtendedSymbol( sym ) );
                    it->second._usage_type = Utils::UsageKind::UNDEFINED;
                }
            }
        }
    }

    template<typename T>
    void UsageVisitor::visit_increment( const T& n )
    {
        // Use of the rhs
        walk( n.get_rhs( ) );

        // Definition of the rhs
        _define = true;
        Nodecl::NodeclBase current_nodecl = _current_nodecl;
        _current_nodecl = Nodecl::NodeclBase::null( );
        walk( n.get_rhs( ) );
        _current_nodecl = current_nodecl;
        _define = false;

    }

    void UsageVisitor::visit( const Nodecl::AddAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::ArithmeticShrAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        Nodecl::NodeclBase current_nodecl = _current_nodecl;
        bool define = _define;

        // Use of the subscripts
        _define = false;
        _current_nodecl = Nodecl::NodeclBase::null( );
        walk( n.get_subscripts( ) );

        // Use of the ArraySubscript
        _define = define;   // Just in case
        if( current_nodecl.is_null( ) )
            _current_nodecl = n;
        else
            _current_nodecl = current_nodecl;
        walk( n.get_subscripted( ) );
        _current_nodecl = Nodecl::NodeclBase::null( );
    }

    void UsageVisitor::visit( const Nodecl::Assignment& n )
    {
        visit_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::BitwiseAndAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::BitwiseOrAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::BitwiseShlAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::BitwiseShrAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::BitwiseXorAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::ClassMemberAccess& n )
    {
        if( _current_nodecl.is_null( ) )
            _current_nodecl = n;

        // walk( n.get_lhs( ) );  // In a member access, the use/definition is always of the member, not the base
        walk( n.get_member( ) );

        _current_nodecl = Nodecl::NodeclBase::null( );
    }

    void UsageVisitor::visit( const Nodecl::Dereference& n )
    {
        Nodecl::NodeclBase current_nodecl = _current_nodecl;
        bool define = _define;

        // Use of the Dereferenced variable
        _define = false;
        _current_nodecl = Nodecl::NodeclBase::null( );
        walk( n.get_rhs( ) );

        // Use of the Dereference
        if( current_nodecl.is_null( ) )
        {
            _define = define;
            _current_nodecl = n;
        }

        walk( n.get_rhs( ) );

        // If we were traversing some object, then the use of that access
        if( !current_nodecl.is_null( ) )
        {
            _define = define;       // Just in case
            _current_nodecl = current_nodecl;
            walk( n.get_rhs( ) );
        }

        _current_nodecl = Nodecl::NodeclBase::null( );
    }

    void UsageVisitor::visit( const Nodecl::DivAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::FunctionCall& n )
    {
        function_visit( n.get_called( ), n.get_arguments( ).as<Nodecl::List>( ) );
    }

    void UsageVisitor::visit( const Nodecl::MinusAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::ModAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::MulAssignment& n )
    {
        visit_binary_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::ObjectInit& n )
    {
        Nodecl::Symbol n_sym = Nodecl::Symbol::make( n.get_symbol( ), n.get_locus() );
        set_var_usage_to_node( Utils::ExtendedSymbol( n_sym ), Utils::UsageKind::DEFINED );

        // Value of initialization, in case it exists
        walk( n.get_symbol( ).get_value( ) );
    }

    void UsageVisitor::visit( const Nodecl::Postdecrement& n )
    {
        visit_increment( n );
    }

    void UsageVisitor::visit( const Nodecl::Postincrement& n )
    {
        visit_increment( n );
    }

    void UsageVisitor::visit( const Nodecl::Predecrement& n )
    {
        visit_increment( n );
    }

    void UsageVisitor::visit( const Nodecl::Preincrement& n )
    {
        visit_increment( n );
    }

    void UsageVisitor::visit( const Nodecl::Range& n )
    {
        walk( n.get_lower() );
        walk( n.get_upper() );
        walk( n.get_stride() );
    }

    void UsageVisitor::visit( const Nodecl::Reference& n )
    {
        Nodecl::NodeclBase rhs = n.get_rhs( );
        if( !_current_nodecl.is_null( ) )
        {
            walk( rhs );
        }
        else
        {   // Only pointers to member are really used
            ReferenceUsageVisitor ruv;
            ruv.walk( rhs );
            _node->add_ue_var( ruv.get_ue_vars( ) );
        }
    }

    void UsageVisitor::visit( const Nodecl::Symbol& n )
    {
        Nodecl::NodeclBase var_in_use = n;
        if( !_current_nodecl.is_null( ) )
            var_in_use = _current_nodecl;

        Symbol sym( Utils::get_nodecl_base( var_in_use ).get_symbol( ) );
        if( _define )
        {
            set_var_usage_to_node( Utils::ExtendedSymbol( var_in_use ), Utils::UsageKind::DEFINED );

            if( _global_vars->find( sym ) != _global_vars->end( ) )
            {
                if( (*_global_vars)[sym]._usage_type & Utils::UsageKind::NONE )
                    (*_global_vars)[sym] = Utils::UsageKind::DEFINED;
                else if( (*_global_vars)[sym]._usage_type & Utils::UsageKind::USED )
                    (*_global_vars)[sym] = Utils::UsageKind::USED | Utils::UsageKind::DEFINED;
            }
            else if( ( _reference_params->find( sym ) != _reference_params->end( ) ) &&
                ( (*_reference_params)[sym]._usage_type & ( Utils::UsageKind::USED | Utils::UsageKind::NONE ) ) )
            {
                if( (*_reference_params)[sym]._usage_type & Utils::UsageKind::NONE )
                    (*_reference_params)[sym] = Utils::UsageKind::DEFINED;
                else if( (*_reference_params)[sym]._usage_type & Utils::UsageKind::USED )
                    (*_reference_params)[sym] = Utils::UsageKind::USED | Utils::UsageKind::DEFINED;
            }
        }
        else
        {
            if( !Utils::ext_sym_set_contains_nodecl( var_in_use, _node->get_killed_vars( ) ) )
            {
                set_var_usage_to_node( Utils::ExtendedSymbol( var_in_use ), Utils::UsageKind::USED );

                if( ( _global_vars->find( sym ) != _global_vars->end( ) ) &&
                    ( (*_global_vars)[sym]._usage_type & Utils::UsageKind::NONE ) )
                {
                    (*_global_vars)[sym] = Utils::UsageKind::USED;
                }
                else if( ( _reference_params->find( sym ) != _reference_params->end( ) ) &&
                    ( (*_reference_params)[sym]._usage_type & Utils::UsageKind::NONE ) )
                {
                    (*_reference_params)[sym] = Utils::UsageKind::USED;
                }
            }
        }
    }

    void UsageVisitor::visit( const Nodecl::UnalignedVectorStore& n )
    {
        visit_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::VectorAssignment& n )
    {
        visit_assignment( n );
    }

    // It is used: the base, the strides (if variables) and the memory positions formed by base+stride_i
    void UsageVisitor::visit( const Nodecl::VectorGather& n )
    {
        Nodecl::NodeclBase base = n.get_base( );
        Nodecl::NodeclBase strides = n.get_strides( );

        // Usage of the base
        walk( base );

        if( strides.is<Nodecl::VectorLiteral>( ) )
        {
            Nodecl::List stride_list = strides.as<Nodecl::VectorLiteral>().get_scalar_values().as<Nodecl::List>();
            for( Nodecl::List::iterator it = stride_list.begin( ); it != stride_list.end( ); ++it )
            {
                // Usage of base+stride_i
                Nodecl::Add current_access = Nodecl::Add::make( base.shallow_copy( ), it->shallow_copy( ), base.get_type( ), it->get_locus( ) );
                if( !Utils::ext_sym_set_contains_nodecl( current_access, _node->get_killed_vars( ) ) )
                    set_var_usage_to_node( Utils::ExtendedSymbol( current_access ), Utils::UsageKind::USED );
            }
        }
        else
        {
            // Usage of the stride
            walk( strides );

            Nodecl::Add current_access = Nodecl::Add::make( base.shallow_copy( ), strides.shallow_copy( ), base.get_type( ), strides.get_locus( ) );
            if( !Utils::ext_sym_set_contains_nodecl( current_access, _node->get_killed_vars( ) ) )
                set_var_usage_to_node( Utils::ExtendedSymbol( current_access ), Utils::UsageKind::USED );
        }
    }

    void UsageVisitor::visit( const Nodecl::VectorMaskAssignment& n )
    {
        visit_assignment( n );
    }

    // It is used: the strides (if variables). It is defined the memory positions formed by base+stride_i
    void UsageVisitor::visit( const Nodecl::VectorScatter& n )
    {
        Nodecl::NodeclBase base = n.get_base( );
        Nodecl::NodeclBase strides = n.get_strides( );
        Nodecl::NodeclBase source = n.get_source( );

        // Usage of source and base
        walk( source );
        walk( base );

        if( strides.is<Nodecl::VectorLiteral>( ) )
        {
            Nodecl::List stride_list = strides.as<Nodecl::VectorLiteral>().get_scalar_values().as<Nodecl::List>();
            for( Nodecl::List::iterator it = stride_list.begin( ); it != stride_list.end( ); ++it )
            {
                // Usage of base+stride_i
                Nodecl::Add current_access = Nodecl::Add::make( base.shallow_copy( ), it->shallow_copy( ), base.get_type( ), it->get_locus( ) );
                if( !Utils::ext_sym_set_contains_nodecl( current_access, _node->get_killed_vars( ) ) )
                    set_var_usage_to_node( Utils::ExtendedSymbol( current_access ), Utils::UsageKind::DEFINED );
            }
        }
        else
        {
            // Usage of strides
            walk( strides );

            Nodecl::Add current_access = Nodecl::Add::make( base.shallow_copy( ), strides.shallow_copy( ), base.get_type( ), strides.get_locus( ) );
            if( !Utils::ext_sym_set_contains_nodecl( current_access, _node->get_killed_vars( ) ) )
                set_var_usage_to_node( Utils::ExtendedSymbol( current_access ), Utils::UsageKind::DEFINED );
        }

    }

    void UsageVisitor::visit( const Nodecl::VectorSincos& n )
    {
        Nodecl::NodeclBase source = n.get_source( );
        Nodecl::NodeclBase sin_ptr = n.get_sin_pointer( );
        Nodecl::NodeclBase cos_ptr = n.get_cos_pointer( );

        walk( source );
        walk( sin_ptr );
        walk( cos_ptr );

        // Uses the source of the operation
        if( !Utils::ext_sym_set_contains_nodecl( source, _node->get_killed_vars( ) ) )
            set_var_usage_to_node( Utils::ExtendedSymbol( source ), Utils::UsageKind::USED );

        // Defines the memory pointed by the second and third parameters
        Nodecl::NodeclBase sin_ptd = Nodecl::Dereference::make( sin_ptr.shallow_copy( ), sin_ptr.get_type( ) );
        if( !Utils::ext_sym_set_contains_nodecl( sin_ptd, _node->get_killed_vars( ) ) )
            set_var_usage_to_node( Utils::ExtendedSymbol( sin_ptd ), Utils::UsageKind::DEFINED );
        Nodecl::NodeclBase cos_ptd = Nodecl::Dereference::make( cos_ptr.shallow_copy( ), cos_ptr.get_type( ) );
        if( !Utils::ext_sym_set_contains_nodecl( cos_ptd, _node->get_killed_vars( ) ) )
            set_var_usage_to_node( Utils::ExtendedSymbol( cos_ptd ), Utils::UsageKind::DEFINED );
    }

    void UsageVisitor::visit( const Nodecl::VectorStore& n )
    {
        visit_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::VectorStreamStore& n )
    {
        visit_assignment( n );
    }

    void UsageVisitor::visit( const Nodecl::VirtualFunctionCall& n )
    {
        function_visit( n.get_called( ), n.get_arguments( ).as<Nodecl::List>( ) );
    }

    ReferenceUsageVisitor::ReferenceUsageVisitor( )
        : _current_nodecl( Nodecl::NodeclBase::null( ) ), _store_symbol( false ), _used_ext_syms( )
    {}

    Utils::ext_sym_set ReferenceUsageVisitor::get_ue_vars( )
    {
        return _used_ext_syms;
    }

    void ReferenceUsageVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        // Walk the base
        Nodecl::NodeclBase subscripted = n.get_subscripted( );
        if( subscripted.get_type( ).is_pointer( ) )
        {   // lhs is used only when it has pointer type
            _store_symbol = true;
            _current_nodecl = n;
            walk( subscripted );
            _current_nodecl = Nodecl::NodeclBase::null( );
            _store_symbol = false;
        }

        // Walk the subscripts
        _store_symbol = true;
        walk( n.get_subscripts( ) );
        _store_symbol = false;
    }

    void ReferenceUsageVisitor::visit( const Nodecl::ClassMemberAccess& n )
    {
        if( _current_nodecl.is_null( ) )
            _current_nodecl = n;
        walk( n.get_member( ) );
        _current_nodecl = Nodecl::NodeclBase::null( );
    }

    void ReferenceUsageVisitor::visit( const Nodecl::Reference& n )
    {
        if( _current_nodecl.is_null( ) )
            _current_nodecl = n;
        walk( n.get_rhs( ) );
        _current_nodecl = Nodecl::NodeclBase::null( );
    }

    void ReferenceUsageVisitor::visit( const Nodecl::Symbol& n )
    {
        if( _store_symbol )
        {
            Nodecl::NodeclBase var_in_use = n;
            if( !_current_nodecl.is_null( ) )
                var_in_use = _current_nodecl;

            _used_ext_syms.insert( var_in_use );
        }
    }

    // *************************** End class implementing use-definition visitor ************************** //
    // **************************************************************************************************** //

}
}
