/*--------------------------------------------------------------------
(C) Copyright 2006-2013 Barcelona Supercomputing Center             *
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
                    n.replace(Nodecl::Reference::make(rhs.as<Nodecl::ArraySubscript>().get_subscripted().shallow_copy(), n.get_type()));
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
    Nodecl::List simplify_pointers(const Nodecl::List& original_variables)
    {
        Nodecl::List simplified_variables;
        for(Nodecl::List::iterator it = original_variables.begin(); it != original_variables.end(); ++it)
        {
            simplified_variables.append(simplify_pointer(*it));
        }
        return simplified_variables;
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
                            ctr_lb = const_value_to_nodecl(const_value_get_zero(/* bytes */ 4, /* signed*/ 1));
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
                const_value_t* one = const_value_get_one(/*bytes*/ 4, /*signed*/ 1);
                NBase one_nodecl = NBase(const_value_to_nodecl(one));
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
    
    void get_modifiable_parameters_to_arguments_map(
        const ObjectList<Symbol>& params, 
        const Nodecl::List& args,
        sym_to_nodecl_map& ptr_params, 
        sym_to_nodecl_map& ref_params)
    {
        int n_iters = std::min(params.size(), args.size());
        if(n_iters > 0)
        {
            Nodecl::List::const_iterator ita = args.begin();
            ObjectList<Symbol>::const_iterator itp = params.begin();
            for(int i = 0; i<n_iters; ++i)
            {
                // Skip conversions and castings in the current argument
                Nodecl::NodeclBase arg = ita->no_conv();
                if(arg.is<Nodecl::Cast>())
                    arg = arg.as<Nodecl::Cast>().get_rhs();

                // Reference parameters
                if(itp->get_type().is_any_reference())
                {
                    ref_params[*itp] = arg;
                }
                // Pointer parameters
                if(itp->get_type().is_pointer() || 
                    (itp->get_type().is_any_reference() && itp->get_type().references_to().is_pointer()))
                {
                    ptr_params[*itp] = arg;
                }
                
                ita++; itp++;
            }
        }
    }
    
    sym_to_nodecl_map get_parameters_to_arguments_map(
            const ObjectList<Symbol>& params, 
            const Nodecl::List& args)
    {
        sym_to_nodecl_map param_to_arg_map;
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
    
}    
}