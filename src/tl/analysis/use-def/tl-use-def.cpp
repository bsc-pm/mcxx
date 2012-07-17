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

#include "tl-use-def.hpp"
    
namespace TL
namespace Analysis {

    void UseDef::fill_use_def_sets(Nodecl::NodeclBase n, bool defined)
    {
        ExtendedSymbol s(n);
        if (defined)
        {
            set_killed_var(ExtendedSymbol(n));
        }
        else
        {
            ext_sym_set killed_vars = get_killed_vars();
            if (!killed_vars.contains(s))
            {
                set_ue_var(s);
            }
        }
    }   

    void UseDef::fill_use_def_sets(Nodecl::List n_l, bool defined)
    {
        for(Nodecl::List::iterator it = n_l.begin(); it != n_l.end(); ++it)
        {
            fill_use_def_sets(*it, defined);
        }
    }

    /*!Try to insert a new variable in a list
     * If an englobing variable of the current variable already exists, then we don't include the variable
     * If any variable englobed by the current variable exists, then we delete the variable
     * If the variable is an array en we can form a range with the to access, we do that deleting the existing element of the list and
     * including the new ranged access
     */
    static Utils::ext_sym_set insert_var_in_list(Nodecl::NodeclBase var, Utils::ext_sym_set list)
    {
        Utils::ext_sym_set new_list;
        if (!Utils::ext_sym_set_contains_englobing_nodecl(var, list))
        {
            // Create a new list with the elements of 'list' that are not englobed by 'var'
            Utils::ext_sym_set aux_list(1, Utils::ExtendedSymbol(var));
            for(Utils::ext_sym_set::iterator it = list.begin(); it != list.end(); ++it)
            {
                if (!Utils::ext_sym_set_contains_englobing_nodecl(it->get_nodecl(), aux_list))
                {
                    new_list.insert(*it);
                }
            }
            
            // Insert the new variable
            new_list.insert(var);
        }
        else
        {   // No need to insert the variable, its englobing symbol is already there
        // FIXME We can create ranges for array accesses here
        new_list = list;
        }
        return new_list;
    }
    
    /*!
     * Inserts the elements in 'l' to the list 'in_l' when they are not in the list 'killed' nor in 'undef'
     * When avoiding lists, it take cares of elements englobing the current variable and of elements englobed by the current variable
     */
    static Utils::ext_sym_set compute_use_def_with_children(Utils::ext_sym_set l, Utils::ext_sym_set in_l, Utils::ext_sym_set& killed, Utils::ext_sym_set& undef, char compute_undef)
    {
        Utils::ext_sym_set new_l = in_l;
        for (Utils::ext_sym_set::iterator it = l.begin(); it != l.end(); ++it)
        {
            Nodecl::NodeclBase var = it->get_nodecl();
            if (!Utils::ext_sym_set_contains_englobing_nodecl(var, killed))
            {   // No englobing variable in the avoiding list 1
                // Look for variables in avoiding list 1 englobed by 'var'
                Utils::ext_sym_set aux_set(1, var);
                Utils::ext_sym_set::iterator itk = killed.begin();
                for (; itk != killed.end(); ++itk)
                {
                    if (Utils::ext_sym_set_contains_englobing_nodecl(itk->get_nodecl(), aux_set))
                    {   // Delete from 'var' the englobed part of (*itk) and put the result in 'var'
                    // TODO
                    std::cerr << "warning: Part of nodecl " << itk->get_nodecl().prettyprint() << " founded in the current var "
                    << var.prettyprint() << " must be avoided. A subpart is killed." << std::endl;
                    //                             var = nodecl_subtract(var, ita->get_nodecl());
                    killed.erase(itk);
                    if (compute_undef == '1')
                        new_l = insert_var_in_list(var, new_l);
                    else
                        undef.insert(var);
                    break;
                    }
                }
                
                if (!Utils::ext_sym_set_contains_englobing_nodecl(var, undef))
                {   // No englobing variable in the avoiding list 2
                    // Look for variables in avoiding list 2 englobed by 'var'
                    Utils::ext_sym_set aux_set; aux_set.insert(*it);
                    Utils::ext_sym_set::iterator itu = undef.begin();
                    for (; itu != undef.end(); ++itu)
                    {
                        if (Utils::ext_sym_set_contains_englobing_nodecl(itu->get_nodecl(), aux_set))
                        {   // Delete from var the englobed part of (*itu) and put the result in 'var'
                            // TODO
                            std::cerr << "warning: Part of nodecl " << itu->get_nodecl().prettyprint() << " founded in the current var "
                            << var.prettyprint() << " must be avoided. A subpart is undefined." << std::endl;
                            undef.erase(itu);
                            if (compute_undef == '1')
                                new_l = insert_var_in_list(var, new_l);
                            else
                                undef.insert(var);
                            break;
                        }
                    }
                    if (itk == killed.end() && itu == undef.end())
                    {
                        new_l = insert_var_in_list(var, new_l);
                    }
                }
            }
        }
        return new_l;
    }
    
    ObjectList<Utils::ext_sym_set> UseDef::get_use_def_over_nodes( Node* current )
    {
        ObjectList<Utils::ext_sym_set> use_def, use_def_aux;
        
        if ( !current->is_visited( ) )
        {
            current->set_visited( true );
            
            // Use-Def in current node
            Utils::ext_sym_set ue_vars = get_ue_vars( );
            Utils::ext_sym_set killed_vars = get_killed_vars( );
            Utils::ext_sym_set undef_vars = get_undefined_behaviour_vars( );
            
            // Concatenate info from children nodes
            ObjectList<Node*> children = get_children( );
            Utils::ext_sym_set ue_children, killed_children, undef_children;
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                use_def_aux = get_use_def_over_nodes( *it );
                if ( !use_def_aux.empty() )
                {
                    ue_children.insert( use_def_aux[0] );
                    killed_children.insert( use_def_aux[1] );
                    undef_children.insert( use_def_aux[2] );
                }
            }
            
            // Append to current node info from children
            Utils::ext_sym_set null_list;
            undef_vars = compute_use_def_with_children( undef_children, undef_vars, 
                                                        killed_vars, undef_vars, /*compute_undef*/ '1' );
            ue_vars = compute_use_def_with_children( ue_children, ue_vars, 
                                                     killed_vars, undef_vars, /*compute_undef*/ '0' );
            killed_vars = compute_use_def_with_children( killed_children, killed_vars, 
                                                         killed_vars, undef_vars, /*compute_undef*/ '0' );
            
            use_def.append( ue_vars ); 
            use_def.append( killed_vars ); 
            use_def.append( undef_vars );
        }
        
        return use_def;
    }
    
    void UseDef::set_graph_node_use_def( Node* outer_node )
    {
        if ( outer_node->is_graph_node( ) )
        {
            if ( !outer_node->is_visited( ) )
            {
                outer_node->set_visited( true );
                
                Node* entry_node = get_data<Node*>(_ENTRY_NODE);
                
                ObjectList<Utils::ext_sym_set> use_def = get_use_def_over_nodes( entry_node );
                outer_node->set_ue_var( use_def[0] );
                outer_node->set_killed_var( use_def[1] );
                outer_node->set_undefined_behaviour_var( use_def[2] );
            }
        }
        else
        {
            internal_error( "Can't propagate use-def info from inner nodes to outer nodes in node '%d' with type '%s'.\n",
                            _id, get_type_as_string().c_str( ) );
        }
    }
    
}
}