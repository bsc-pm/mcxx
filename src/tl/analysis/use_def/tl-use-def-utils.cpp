/*--------------------------------------------------------------------
(C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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

#include "tl-use-def.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // ************************* Class implementing pointer simplification visitor ************************ //
    
    class LIBTL_CLASS PointersSimplifierVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        void unhandled_node(const NBase& n)
        {
            WARNING_MESSAGE("Unhandled node of type '%s' while PointersSimplifierVisitor.\n", ast_print_node_type(n.get_kind()));
        }
        
        void visit(const Nodecl::Dereference& n)
        {
            NBase rhs = n.get_rhs().no_conv();
            if(rhs.is<Nodecl::Reference>())
            {   // *&v  ->  v
                n.replace(rhs.as<Nodecl::Reference>().get_rhs().no_conv().shallow_copy());
                walk(n);
            }
            else
            {
                walk(rhs);
            }
        }
        
        void visit(const Nodecl::Reference& n)
        {
            NBase rhs = n.get_rhs().no_conv();
            if(rhs.is<Nodecl::Dereference>())
            {   // &*v  ->  v
                n.replace(rhs.as<Nodecl::Dereference>().get_rhs().no_conv().shallow_copy());
                walk(n);
            }
            else if(rhs.is<Nodecl::ArraySubscript>())
            {
                Nodecl::List subscripts = rhs.as<Nodecl::ArraySubscript>().get_subscripts().as<Nodecl::List>();
                if(subscripts.size()==1 && 
                    subscripts.front().is_constant() && 
                    const_value_is_zero(subscripts.front().get_constant()))
                {
                    n.replace(rhs.as<Nodecl::ArraySubscript>().get_subscripted().shallow_copy());
                }
            }
            else
            {
                walk(rhs);
            }
        }
    };
    
    NBase simplify_pointer(const NBase& original_variable)
    {
        NBase simplified_variable = original_variable.no_conv().shallow_copy();
        PointersSimplifierVisitor psv;
        psv.walk(simplified_variable);
        return simplified_variable;
    }
    
    // This method simplifies arguments in the way "*&v and &*v -> v"
    Nodecl::List simplify_pointers(const Nodecl::List& original_args)
    {
        Nodecl::List simplified_args;
        for(Nodecl::List::iterator it = original_args.begin(); it != original_args.end(); ++it)
        {
            simplified_args.append(simplify_pointer(*it));
        }
        return simplified_args;
    }

    class LIBTL_CLASS ArgumentSimplifierVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        void unhandled_node(const NBase& n)
        {
            WARNING_MESSAGE("Unhandled node of type '%s' while ArgumentSimplifierVisitor.\n",
                            ast_print_node_type(n.get_kind()));
        }

        void visit(const Nodecl::DefaultArgument& n)
        {
            n.replace(n.get_argument());
        }
    };

    Nodecl::List simplify_arguments(const Nodecl::List& original_args)
    {
        Nodecl::List simplified_args;
        ArgumentSimplifierVisitor asv;
        for (Nodecl::List::iterator it = original_args.begin(); it != original_args.end(); ++it)
        {
            Nodecl::NodeclBase arg = it->no_conv().shallow_copy();
            asv.walk(arg);
            simplified_args.append(arg);
        }
        return simplified_args;
    }

    // *********************** End class implementing pointer simplification visitor ********************** //
    // **************************************************************************************************** //
    
    
    // This method returns the part of 'container' which is not the 'contained'.
    // If the separation cannot be done, then it returns NBase::null()
    NBase split_var_depending_on_usage_rec(NBase container, NBase contained)
    {
        NBase result;
        if(contained.is<Nodecl::ArraySubscript>())
        {   // Contained[...]
            // Get the 'container' and 'contained' accesses. If not a Range, transform it into a Range
            NBase ctr_subscript, ctd_subscript;
            NBase subscripted;
            NBase ctr_lb;
            NBase ctr_ub;
            TL::Type ctr_t = container.is<Nodecl::Symbol>() ? container.get_symbol().get_type().no_ref() 
                                                            : container.get_type().no_ref();
            ERROR_CONDITION(!ctr_t.is_valid(), 
                            "Invalid type computed for container %s.\n", 
                            container.prettyprint().c_str());
            Nodecl::List ctd_subscripts = contained.as<Nodecl::ArraySubscript>().get_subscripts().as<Nodecl::List>();
            if(container.is<Nodecl::Analysis::RangeUnion>())
            {
                NBase lhs = container.as<Nodecl::Analysis::RangeUnion>().get_lhs();
                NBase rhs = container.as<Nodecl::Analysis::RangeUnion>().get_rhs();
                result = split_var_depending_on_usage_rec(lhs, contained);  // Try whether the LHS contains 'contained'
                if(Nodecl::Utils::structurally_equal_nodecls(result, lhs))  // if not, try with the RHS
                    result = split_var_depending_on_usage_rec(result, rhs);
            }
            else
            {
                if(container.is<Nodecl::ArraySubscript>())
                {   // Container[...]
                    subscripted = container.as<Nodecl::ArraySubscript>().get_subscripted();
                    Nodecl::List ctr_subscripts = container.as<Nodecl::ArraySubscript>().get_subscripts().as<Nodecl::List>();
                    unsigned int i = 0;
                    unsigned int size = std::min(ctd_subscripts.size(), ctr_subscripts.size());
                    while(i < size-1)
                    {
                        if(!Nodecl::Utils::structurally_equal_nodecls(ctr_subscripts[i], ctd_subscripts[i]))
                        {
                            if(VERBOSE)
                            {
                                WARNING_MESSAGE("Splitting usage of multidimensional arrays when dimensions other than "
                                                "the less significant accessed differs is not yet supported\n", 0);
                            }
                            return result;
                        }
                        i++;
                    }
                    
                    ctd_subscript = ctd_subscripts[i];
                    ctr_subscript = ctr_subscripts[i];
                    
                    if(ctr_subscript.is<Nodecl::Range>())
                    {   // Container[ctr_lb:ctr_ub]
                        ctr_lb = ctr_subscript.as<Nodecl::Range>().get_lower();
                        ctr_ub = ctr_subscript.as<Nodecl::Range>().get_upper();
                    }
                    else
                    {   // Container[ctr_index]  ->  Container[ctr_index:ctr_index]
                        ctr_lb = ctr_subscript;
                        ctr_ub = ctr_subscript;
                    }
                }
                else
                {
                    if(ctd_subscripts.size() > 1)
                    {
                        if(VERBOSE)
                        {
                            WARNING_MESSAGE("Cannot split the multidimensional array access %s "
                                            "when its container expression is other than an array access to the less significant dimension\n",
                                            contained.prettyprint().c_str(), container.prettyprint().c_str());
                        }
                        return result;
                    }
                    
                    ctd_subscript = ctd_subscripts[0];
                    subscripted = container;
                    
                    if(ctr_t.is_array())
                    {
                        ctr_t.array_get_bounds(ctr_lb, ctr_ub);
                    }
                    else if(ctr_t.is_pointer())
                    {
                        if(_pointer_to_size_map.find(subscripted.no_conv()) != _pointer_to_size_map.end())
                        {
                            ctr_lb = const_value_to_nodecl_with_basic_type(const_value_get_zero(/* bytes */ type_get_size(get_ptrdiff_t_type()), /* signed*/ 1), get_ptrdiff_t_type());
                            ctr_ub = _pointer_to_size_map[subscripted];
                        }
                        else
                        {
                            if(VERBOSE)
                            {
                                WARNING_MESSAGE("We cannot split the usage of pointer '%s' because we do not know the size of the object\n", 
                                                container.prettyprint().c_str());
                            }
                            return result;
                        }
                    }
                }
            
                // Get the 'contained' access. If not a Range, transform it into a Range
                NBase ctd_lb;
                NBase ctd_ub;
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
                NBase lb, ub;
                const_value_t* one = const_value_get_one(/*bytes*/ type_get_size(get_ptrdiff_t_type()), /*signed*/ 1);
                NBase one_nodecl = NBase(const_value_to_nodecl_with_basic_type(one, get_ptrdiff_t_type()));
                Nodecl::Range ctr_analysis = Nodecl::Range::make(ctr_lb.shallow_copy(), ctr_ub.shallow_copy(), 
                                                                 one_nodecl.shallow_copy(), ctd_subscript_t);
                Nodecl::Range ctd_analysis = Nodecl::Range::make(ctd_lb.shallow_copy(), ctd_ub.shallow_copy(), 
                                                                 one_nodecl.shallow_copy(), ctd_subscript_t);
                NBase range_subtraction = Utils::range_sub(ctr_analysis, ctd_analysis);
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
                    NBase lhs = range_subtraction.as<Nodecl::Analysis::RangeUnion>().get_lhs();
                    NBase rhs = range_subtraction.as<Nodecl::Analysis::RangeUnion>().get_rhs();
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
        else if(contained.is<Nodecl::ClassMemberAccess>())
        {
            TL::Type ctr_t = container.get_type().no_ref();
            // We may have here a class or a pointer to a class
            if(ctr_t.is_pointer())
                ctr_t = ctr_t.points_to();
            
            if(ctr_t.is_class())
            {   // struct t { ... }
                // Compute the nest of members accessed by 'contained'
                ObjectList<Symbol> ctd_nested_syms;
                ctd_nested_syms.append(contained.as<Nodecl::ClassMemberAccess>().get_member().get_symbol());
                NBase tmp = contained.as<Nodecl::ClassMemberAccess>().get_lhs();
                while(tmp.is<Nodecl::ClassMemberAccess>())
                {
                    ctd_nested_syms.append(tmp.as<Nodecl::ClassMemberAccess>().get_member().get_symbol());
                    tmp = tmp.as<Nodecl::ClassMemberAccess>().get_lhs();
                }

                // Travers the 'Container' adding all members that are not in the list just computed
                ObjectList<Symbol> ctr_members = ctr_t.get_all_data_members();
                NBase current_lhs = container;
                NBase next_cma_lhs;
                while(!ctr_members.empty())
                {
                    Symbol current_member_sym;      // We want this variable to be new at each iteration
                    for(ObjectList<Symbol>::iterator it = ctr_members.begin(); it != ctr_members.end(); ++it)
                    {
                        Nodecl::Symbol sym_n = Nodecl::Symbol::make(*it);
                        Nodecl::ClassMemberAccess new_cma = Nodecl::ClassMemberAccess::make(current_lhs.shallow_copy(), sym_n,
                                                                                            /*member-form*/ NBase::null(), it->get_type());
                        if(!ctd_nested_syms.contains(*it))
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
                    if(current_member_sym.is_valid())
                    {
                        // Get the next data members set
                        TL::Type mem_t = current_member_sym.get_type();
                        if(mem_t.is_lvalue_reference())
                            mem_t = mem_t.references_to();
                        if(mem_t.is_class())
                            ctr_members = mem_t.get_all_data_members();
                        // Set the new lhs for the next members
                        current_lhs = next_cma_lhs;
                    }
                }
            }
            else
            {
                if(VERBOSE)
                {
                    WARNING_MESSAGE("Container %s, of contained %s, has no class type. Instead it is %s\n",
                                    container.prettyprint().c_str(), contained.prettyprint().c_str(),
                                    print_declarator(ctr_t.get_internal_type()));
                }
                return result;
            }
        }
        else
        {
            if(VERBOSE)
            {
                WARNING_MESSAGE("Unexpected type of nodecl '%s' when splitting object '%s' into different subobjects.\n"
                                "ArraySubscript or ClassMemberAccess expected\n",
                                ast_print_node_type(contained.get_kind()), contained.prettyprint().c_str());
            }
        }
        return result;
    }
    
    // Overloading method for the case when the 'contained' is a Nodecl::List
    NBase split_var_depending_on_usage(NBase container, NBase contained)
    {
        NBase result = container.no_conv();
        if(contained.is<Nodecl::List>())
        {
            Nodecl::List contained_list = contained.as<Nodecl::List>();
            for(Nodecl::List::iterator it = contained_list.begin(); it != contained_list.end(); ++it)
            {
                result = split_var_depending_on_usage_rec(result, it->no_conv());
                if(result.is_null())    // If we are not able to split one element in 'contained' list, stop splitting the rest
                    break;
            }
        }
        else
            result = split_var_depending_on_usage_rec(result, contained);
        return result;
    }
    
    bool any_parameter_is_pointer(const ObjectList<Symbol>& params)
    {
        for (ObjectList<Symbol>::const_iterator it = params.begin();
             it != params.end(); ++it)
        {
            if (it->get_type().is_pointer()
                    || (it->get_type().is_any_reference()
                            && it->get_type().references_to().is_pointer()))
            {
                return true;
            }
        }
        return false;
    }

    bool any_parameter_is_reference(const ObjectList<Symbol>& params)
    {
        for (ObjectList<Symbol>::const_iterator it = params.begin();
             it != params.end(); ++it)
        {
            if (it->get_type().is_any_reference())
                return true;
        }
        return false;
    }
    
    SymToNodeclMap get_parameters_to_arguments_map(
            const ObjectList<Symbol>& params, 
            const Nodecl::List& args)
    {
        SymToNodeclMap param_to_arg_map;
        int n_iters = std::min(params.size(), args.size());
        if(n_iters > 0)
        {
            Nodecl::List::const_iterator ita = args.begin();
            ObjectList<Symbol>::const_iterator itp = params.begin();
            for(int i = 0; i<n_iters; ++i)
            {
                param_to_arg_map[*itp] = *ita;
                ita++; itp++;
            }
        }
        return param_to_arg_map;
    }


    /*!This method computes the usage of a node in two different cases:
     * - we are merging the usage of the children nodes with a parent node to set the usage of the enclosing graph node
     * - we are computing the usage of a node: since there may be more than one statement within the same node,
     *                                         we need to take into account the usage computed for the already treated statements
     */
    void propagate_usage_to_ancestor(
            Node* ancestor,
            NodeclSet& ue_vars, NodeclSet& killed_vars,
            NodeclSet& undef_vars, NodeclSet& used_addresses,
            const NodeclSet& ue_children, const NodeclSet& killed_children,
            const NodeclSet& undef_children, const NodeclSet& used_addresses_children)
    {
        // Propagate the upwards exposed variables
        NBase ue_previously_killed_subobject, ue_previously_undef_subobject;
        for (NodeclSet::iterator it = ue_children.begin(); it != ue_children.end(); ++it)
        {
            NBase n_it = *it;

            // There is a special case when dealing with Tasks: even though a task may kill a variable
            // (and this usage is propagated to its task creation node), we still have to propagate the usage of the children,
            // because we do not know what is happening first
            bool is_task_creation_context = (ancestor->is_context_node()
                    && ancestor->get_graph_entry_node()->get_children().size()==1
                    && ancestor->get_graph_entry_node()->get_children()[0]->is_omp_task_creation_node());

            // UE vars can only be upwards propagated if the are not already KILLED in the parent
            // or they (or an enclosing nodecl) are not yet in the result set
            if (!is_task_creation_context
                    && (!Utils::nodecl_set_contains_enclosing_nodecl(n_it, killed_vars).is_null()
                            || !Utils::nodecl_set_contains_enclosing_nodecl(n_it, ue_vars).is_null()))
                continue;

            ue_previously_killed_subobject = Utils::nodecl_set_contains_enclosed_nodecl(n_it, killed_vars);
            ue_previously_undef_subobject = Utils::nodecl_set_contains_enclosed_nodecl(n_it, undef_vars);
            if (is_task_creation_context
                || (ue_previously_killed_subobject.is_null()
                    && ue_previously_undef_subobject.is_null()))
            {   // Neither killed nor undef var sets contain the current ue var
                // Or the ancestor is a task creation node, in which case we have to be conservative and propagate its children usage
                NBase tmp = Utils::nodecl_set_contains_enclosed_nodecl(n_it, ue_vars);
                if (!tmp.is_null())
                    ue_vars.erase(tmp);   // Delete the enclosed var from the list
                ue_vars.insert(*it);      // Insert the new containing var
            }
            else
            {   // Here a part of the nodecl is KILLED|UNDEF and a part is UE
                NBase non_killed_ue_vars, non_undef_ue_vars;
                if (!ue_previously_killed_subobject.is_null())
                    non_killed_ue_vars = split_var_depending_on_usage(n_it, ue_previously_killed_subobject);
                if (!ue_previously_undef_subobject.is_null())
                {   // A variable marked as UNDEF can still be UE
                    if (Utils::nodecl_set_contains_nodecl(n_it, undef_vars))
                        non_undef_ue_vars = n_it;
                    else
                        non_undef_ue_vars = split_var_depending_on_usage(n_it, ue_previously_undef_subobject);
                }

                if ((!ue_previously_killed_subobject.is_null() && non_killed_ue_vars.is_null())
                        || (!ue_previously_undef_subobject.is_null() && non_undef_ue_vars.is_null()))
                {   // When the two sets are empty is because the separation has not been possible
                    // Then, we set to undef the whole object and remove the partial object from the corresponding list(s)
                    if (!ue_previously_killed_subobject.is_null())
                        killed_vars.erase(ue_previously_killed_subobject);
                    if (!ue_previously_undef_subobject.is_null())
                        undef_vars.erase(ue_previously_undef_subobject);
                    undef_vars.insert(n_it);
                }
                else
                {   // new_ue_varsX may be the union of different array ranges. We may want to split the union into separated nodecls
                    if (!non_killed_ue_vars.is_null())
                        ue_vars.insert(non_killed_ue_vars);
                    if (!non_undef_ue_vars.is_null())
                        ue_vars.insert(non_undef_ue_vars);
                }
            }
        }

        // Propagate the killed variables
        NBase non_killed_var;
        for (NodeclSet::iterator it = killed_children.begin(); it != killed_children.end(); ++it)
        {
            NBase n_it = *it;
            if (!Utils::nodecl_set_contains_enclosing_nodecl(n_it, undef_vars).is_null()
                    || !Utils::nodecl_set_contains_enclosing_nodecl(n_it, killed_vars).is_null())
                continue;

            non_killed_var = Utils::nodecl_set_contains_enclosed_nodecl(n_it, undef_vars);
            if (non_killed_var.is_null())
            {   // Undef var set does not contain the current killed var
                NBase already_killed_var = Utils::nodecl_set_contains_enclosed_nodecl(n_it, killed_vars);
                if (!already_killed_var.is_null())
                    killed_vars.erase(already_killed_var);    // A part of the variable was already killed: remove the subobject
                killed_vars.insert(*it);                      // Insert the whole enclosing object
            }
            else
            {   // Here a part of the nodecl has already been marked as undefined
                NBase new_killed_vars;
                new_killed_vars = split_var_depending_on_usage(n_it, non_killed_var);
                if (new_killed_vars.is_null())
                {   // When the set is null is because the separation has not been possible
                    // Then, we set to undefined the whole object and remove the partial object from the killed list
                    NBase already_killed_var = Utils::nodecl_set_contains_enclosed_nodecl(n_it, killed_vars);
                    if (!already_killed_var.is_null())
                        killed_vars.erase(already_killed_var);    // A part of the variable was already killed: remove the subobject
                    undef_vars.insert(*it);                       // Insert the whole enclosing object
                }
                else
                {   // Insert the computed killed parts in the corresponding list
                    // new_killed_vars may be the union of different array ranges. We may want to split the union into separated nodecls
                    killed_vars.insert(new_killed_vars);
                }
            }
        }

        // Propagate the undefined behavior variables of the children
        NBase undef_previously_ue_subobject, undef_previously_killed_subobject;
        for (NodeclSet::iterator it = undef_children.begin(); it != undef_children.end(); ++it)
        {
            NBase n_it = *it;
            // Variables marked as KILLED cannot be UNDEF
            if (!Utils::nodecl_set_contains_enclosing_nodecl(n_it, killed_vars).is_null())
                continue;

            undef_previously_ue_subobject = Utils::nodecl_set_contains_enclosed_nodecl(n_it, ue_vars);
            undef_previously_killed_subobject = Utils::nodecl_set_contains_enclosed_nodecl(n_it, killed_vars);
            if (undef_previously_ue_subobject.is_null() && undef_previously_killed_subobject.is_null())
            {   // Neither ue nor killed var sets contain the current ue var
                if (!Utils::nodecl_set_contains_enclosing_nodecl(n_it, undef_vars).is_null())
                    continue;
                else
                {
                    NBase tmp = Utils::nodecl_set_contains_enclosed_nodecl(n_it, undef_vars);
                    if (!tmp.is_null())
                        undef_vars.erase(tmp);        // Delete the enclosed var from the list
                    undef_vars.insert(*it);           // Insert the new containing var
                }
            }
            else
            {
                NBase non_ue_undef_vars, non_killed_undef_vars;
                if (!undef_previously_ue_subobject.is_null())
                {
                    if (Utils::nodecl_set_contains_nodecl(n_it, ue_vars))
                        non_ue_undef_vars = n_it;
                    else
                        non_ue_undef_vars = split_var_depending_on_usage(n_it, undef_previously_ue_subobject);
                }
                if (!undef_previously_killed_subobject.is_null())
                    non_killed_undef_vars = split_var_depending_on_usage(n_it, undef_previously_killed_subobject);

                if (non_ue_undef_vars.is_null() && non_killed_undef_vars.is_null())
                {   // When the two sets are null is because the separation has not been possible
                    // Then, we set to undef the whole object and remove the partial object from the corresponding list(s)
                    if (!undef_previously_ue_subobject.is_null())
                        ue_vars.erase(undef_previously_ue_subobject);
                    if (!undef_previously_killed_subobject.is_null())
                        killed_vars.erase(undef_previously_killed_subobject);
                    undef_vars.insert(n_it);
                }
                else
                {   // new_undef_varsX may be the union of different array ranges. We may want to split the union into separated nodecls
                    if (!non_ue_undef_vars.is_null())
                        undef_vars.insert(non_ue_undef_vars);
                    if (!non_killed_undef_vars.is_null())
                        undef_vars.insert(non_killed_undef_vars);
                }
            }
        }

        // Propagate the used addresses of the children
        used_addresses.insert(used_addresses_children.begin(), used_addresses_children.end());
    }

    // **************************************************************************************************** //
    // ******************************** Usage computing methods for graphs ******************************** //

    void merge_children_usage(
            NodeclSet& ue_vars, NodeclSet& killed_vars,
            NodeclSet& undef_vars, int node_id)
    {
        // Purge UNDEF vars from those vars that are in both UE and KILLED lists
        for (NodeclSet::iterator it = ue_vars.begin(); it != ue_vars.end(); ++it)
        {
            if ((!Utils::nodecl_set_contains_enclosing_nodecl(*it, killed_vars).is_null()
                    || !Utils::nodecl_set_contains_enclosed_nodecl(*it, killed_vars).is_null())
                && !Utils::nodecl_set_contains_enclosing_nodecl(*it, undef_vars).is_null())
            {
                undef_vars.erase(*it);
            }
        }
    }

    /*!Recursive method that returns a list with three elements:
     * - The first is the list of upper exposed variables of the graph node;
     * - The second is the list of killed variables of the graph node
     * - The third is the list of undefined variables of the graph
     */
    void get_use_def_over_nodes(Node* n, ObjectList<NodeclSet>& use_def)
    {
        // 1.- Base case: the node has already been visited
        if (n->is_visited_extgraph())
            return;

        n->set_visited_extgraph(true);

        // 2.- Task nodes information has already been propagated to its corresponding task_creation node
        if (n->is_omp_task_node() || n->is_omp_async_target_node())
            return;

        // 3.- Compute the information from the children
        // 3.1.- Concatenate info from children nodes
        ObjectList<NodeclSet> use_def_aux;
        const ObjectList<Node*>& children = n->get_children();
        NodeclSet ue_children, killed_children, undef_children, used_addresses_children;
        for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
        {
            get_use_def_over_nodes(*it, use_def_aux);
            if (!use_def_aux.empty())
            {
                ue_children.insert(use_def_aux[0].begin(), use_def_aux[0].end());
                killed_children.insert(use_def_aux[1].begin(), use_def_aux[1].end());
                undef_children.insert(use_def_aux[2].begin(), use_def_aux[2].end());
                used_addresses_children.insert(use_def_aux[3].begin(), use_def_aux[3].end());
            }
        }
        // 3.2.- Merge children (make the different sets to be consistent)
        merge_children_usage(ue_children, killed_children, undef_children, n->get_id());

        // 4.- Gather the Use-Def info of the current node
        const NodeclSet& ue_vars = n->get_ue_vars();
        const NodeclSet& killed_vars = n->get_killed_vars();
        const NodeclSet& undef_vars = n->get_undefined_behaviour_vars();
        const NodeclSet& used_addresses = n->get_used_addresses();

        // 5.- Merge current node and its children usage information
        NodeclSet new_ue_vars(ue_vars.begin(), ue_vars.end());
        NodeclSet new_killed_vars(killed_vars.begin(), killed_vars.end());
        NodeclSet new_undef_vars(undef_vars.begin(), undef_vars.end());
        NodeclSet new_used_addresses(used_addresses.begin(), used_addresses.end());
        propagate_usage_to_ancestor(
                n,
                new_ue_vars, new_killed_vars, new_undef_vars, new_used_addresses,
                ue_children, killed_children, undef_children, used_addresses_children);

        // 6.- Append the new usage information to the output parameter
        if (!new_ue_vars.empty() || !new_killed_vars.empty()
                || !new_undef_vars.empty() || !new_used_addresses.empty())
        {
            use_def.append(new_ue_vars);
            use_def.append(new_killed_vars);
            use_def.append(new_undef_vars);
            use_def.append(new_used_addresses);
        }
    }

    void purge_local_variables(Scope graph_sc, NodeclSet& vars_set)
    {
        NodeclSet::iterator it = vars_set.begin();
        while (it != vars_set.end())
        {
            const NBase& n = it->no_conv();
            const NBase& n_base = Utils::get_nodecl_base(n);
            if (!n_base.is_null())
            {
//                 const ObjectList<Symbol>& func_params = _graph->get_function_parameters();
                Scope var_sc(n_base.get_symbol().get_scope());
                if (!n_base.get_symbol().get_type().is_pointer()
                        && !n_base.get_symbol().get_type().is_array()
                        && !n_base.get_symbol().get_type().is_any_reference()
                        /* && !func_params.contains(n_base.get_symbol()) */     // Do we really need to purge this?
                        && var_sc.scope_is_enclosed_by(graph_sc))
                {
                    vars_set.erase(it++);
                }
                else
                {
                    ++it;
                }
            }
            else
            {
                ++it;
            }
        }
    }

    void set_graph_node_use_def(Node* n)
    {
        // 1.- Base cases: the node is not a graph or the node has already been visited
        if (!n->is_graph_node())
            return;
        if (n->is_visited_extgraph())
            return;

        n->set_visited_extgraph(true);

        // 2.- Get the information from the inner nodes of the graph
        NodeclSet ue_vars, killed_vars, undef_vars, used_addresses;
        ObjectList<NodeclSet> usage;
        get_use_def_over_nodes(n->get_graph_entry_node(), usage);
        if (!usage.empty())
        {
            ue_vars = usage[0];
            killed_vars = usage[1];
            undef_vars = usage[2];
            used_addresses = usage[3];
        }

        NodeclSet private_ue_vars, private_killed_vars, private_undef_vars;
        if (n->is_omp_loop_node() || n->is_omp_sections_node() || n->is_omp_single_node()
            || n->is_omp_parallel_node() || n->is_omp_task_node()
            || n->is_omp_async_target_node() || n->is_omp_sync_target_node())
        {   // Take into account data-sharing clauses in Use-Def Task node computation
            const Nodecl::List& environ =
            n->get_graph_related_ast().as<Nodecl::OpenMP::Task>().get_environment().as<Nodecl::List>();
            for (Nodecl::List::iterator it = environ.begin(); it != environ.end(); ++it)
            {
                if (it->is<Nodecl::OpenMP::Private>())
                {   // Remove any usage computed in the inner nodes,
                    // because is the usage of a copy of this variable
                    const Nodecl::List& private_syms =
                    it->as<Nodecl::OpenMP::Private>().get_symbols().as<Nodecl::List>();
                    for (Nodecl::List::const_iterator itp = private_syms.begin();
                         itp != private_syms.end(); ++itp)
                    {
                        const NBase& n_itp = *itp;
                        if (Utils::nodecl_set_contains_nodecl(n_itp, undef_vars))
                        {
                            undef_vars.erase(n_itp);
                            private_undef_vars.insert(n_itp);
                        }
                        else
                        {
                            if (Utils::nodecl_set_contains_nodecl(n_itp, ue_vars))
                            {
                                ue_vars.erase(n_itp);
                                private_ue_vars.insert(n_itp);
                            }
                            if (Utils::nodecl_set_contains_nodecl(n_itp, killed_vars))
                            {
                                killed_vars.erase(n_itp);
                                private_killed_vars.insert(n_itp);
                            }
                        }
                    }
                }
                if (it->is<Nodecl::OpenMP::Firstprivate>())
                {   // This variable is Upper Exposed in the task
                    const Nodecl::List& firstprivate_syms =
                    it->as<Nodecl::OpenMP::Firstprivate>().get_symbols().as<Nodecl::List>();
                    for (Nodecl::List::const_iterator itfp = firstprivate_syms.begin();
                         itfp != firstprivate_syms.end(); ++itfp)
                    {
                        const NBase& n_itfp = *itfp;
                        if (Utils::nodecl_set_contains_nodecl(n_itfp, undef_vars))
                        {
                            undef_vars.erase(n_itfp);
                            private_undef_vars.insert(n_itfp);
                        }
                        else if (Utils::nodecl_set_contains_nodecl(n_itfp, killed_vars))
                        {
                            killed_vars.erase(n_itfp);
                            private_killed_vars.insert(n_itfp);
                        }
                    }
                }
            }
        }
        else
        {   // Purge variables local to the current graph
            const Nodecl::NodeclBase& ast = n->get_graph_related_ast();
            Scope graph_sc(ast.retrieve_context());
            purge_local_variables(graph_sc, ue_vars);
            purge_local_variables(graph_sc, killed_vars);
            purge_local_variables(graph_sc, undef_vars);
            purge_local_variables(graph_sc, used_addresses);
            purge_local_variables(graph_sc, private_ue_vars);
            purge_local_variables(graph_sc, private_killed_vars);
            purge_local_variables(graph_sc, private_undef_vars);
        }

        // 3.- Set the info to the node
        n->set_ue_var(ue_vars);
        n->set_killed_var(killed_vars);
        n->set_undefined_behaviour_var(undef_vars);
        n->set_used_addresses(used_addresses);

        n->set_private_ue_var(private_ue_vars);
        n->set_private_killed_var(private_killed_vars);
        n->set_private_undefined_behaviour_var(private_undef_vars);
    }

    // ****************************** END Usage computing methods for graphs ****************************** //
    // **************************************************************************************************** //
}    
}
