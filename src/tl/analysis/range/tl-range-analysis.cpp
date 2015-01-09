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

#include "cxx-cexpr.h"
#include "cxx-process.h"
#include "tl-nodecl-calc.hpp"
#include "tl-range-analysis.hpp"

#include <algorithm>
#include <fstream>
#include <list>
#include <queue>
#include <set>
#include <unistd.h>
#include <sys/stat.h>
#include <unistd.h>

namespace TL {
namespace Analysis {
    
    #define RANGES_DEBUG
    
    // **************************************************************************************************** //
    // **************************** Visitor implementing constraint building ****************************** //
    
namespace {
    
    // *** Variables and methods to simulate SSA during the Constraint Graph construction *** //
    unsigned int non_sym_constraint_id = 0;
    
    std::map<Symbol, NBase> ssa_to_original_var;
    
    //! This maps stores the relationship between each variable in a given node and 
    //! the last identifier used to create a constraint for that variable
    std::map<NBase, unsigned int, Nodecl::Utils::Nodecl_structural_less> var_to_last_constraint_id;
    
    unsigned int get_next_id(const NBase& n)
    {
        unsigned int next_id = 0;
        if(!n.is_null())
        {
            if(var_to_last_constraint_id.find(n) != var_to_last_constraint_id.end())
                next_id = var_to_last_constraint_id[n] + 1;
            var_to_last_constraint_id[n] = next_id;
        }
        else
        {
            next_id = ++non_sym_constraint_id;
        }
        return next_id;
    }
    
    // *** Convenient global constants to create the ranges *** //
    const_value_t* zero = const_value_get_zero(/*num_bytes*/ 4, /*sign*/1);
    const_value_t* one = const_value_get_one(/*num_bytes*/ 4, /*sign*/1);
    const_value_t* minus_one = const_value_get_minus_one(/*num_bytes*/ 4, /*sign*/1);
    
    Optimizations::Calculator calc;
}
    
    ConstraintReplacement::ConstraintReplacement(Utils::VarToConstraintMap constraints_map)
        : _constraints_map(constraints_map)
    {}
    
    void ConstraintReplacement::visit(const Nodecl::ArraySubscript& n)
    {
        if(_constraints_map.find(n) != _constraints_map.end())
            n.replace(_constraints_map[n].get_symbol().make_nodecl(/*set_ref_type*/false));
        else
        {
            walk(n.get_subscripted());
            walk(n.get_subscripts());
        }
    }
    
    void ConstraintReplacement::visit(const Nodecl::ClassMemberAccess& n)
    {
        if(_constraints_map.find(n) != _constraints_map.end())
            n.replace(_constraints_map[n].get_symbol().make_nodecl(/*set_ref_type*/false));
        else
        {
            walk(n.get_lhs());
            walk(n.get_member());
        }
    }
    
    void ConstraintReplacement::visit(const Nodecl::Symbol& n)
    {
        ERROR_CONDITION(_constraints_map.find(n) == _constraints_map.end(),
                        "No constraints found for symbol %s in locus %s. "
                        "We should replace the variable with the corresponding constraint.", 
                        n.prettyprint().c_str(), n.get_locus_str().c_str());
        
        n.replace(_constraints_map[n].get_symbol().make_nodecl(/*set_ref_type*/false));
    }
    
    ConstraintBuilderVisitor::ConstraintBuilderVisitor(
            Node* n,
            Utils::VarToConstraintMap input_constraints_map, 
            Utils::VarToConstraintMap current_constraints, 
            SSAVarToValue_map *constraints,
            NodeclList *ordered_constraints)
        : _n(n), _input_constraints_map(input_constraints_map), _output_constraints_map(current_constraints), 
          _output_true_constraints_map(), _output_false_constraints_map(), 
          _constraints(constraints), _ordered_constraints(ordered_constraints)
    {}
    
    ConstraintBuilderVisitor::ConstraintBuilderVisitor(
            Node* n,
            SSAVarToValue_map *constraints,
            NodeclList *ordered_constraints)
        : _n(n), _input_constraints_map(), _output_constraints_map(), 
          _output_true_constraints_map(), _output_false_constraints_map(), 
          _constraints(constraints), _ordered_constraints(ordered_constraints)
    {}
    
    Utils::Constraint ConstraintBuilderVisitor::build_constraint(
            const Symbol& s, 
            const NBase& val, 
            const Type& t, 
            std::string c_name)
    {
        // Create the constraint
        Utils::Constraint c = Utils::Constraint(s, val);
        
        // Insert the constraint in the global structures that will allow us building the Constraint Graph
        Nodecl::Symbol s_n = s.make_nodecl(/*set_ref_type*/false);
        if(_constraints->find(s_n) == _constraints->end())
            _ordered_constraints->push_back(s_n);
        (*_constraints)[s_n] = val.no_conv();
        
        // Print the constraint in the standard error
        print_constraint(c_name, s, val, t);
        
        return c;
    }
    
    void ConstraintBuilderVisitor::compute_stmt_constraints(const NBase& n)
    {
        walk(n);
    }
    
    void ConstraintBuilderVisitor::compute_parameters_constraints(const ObjectList<Symbol>& params)
    {
        for(ObjectList<Symbol>::const_iterator it = params.begin(); it != params.end(); ++it)
        {
            Nodecl::Symbol param_s = it->make_nodecl(/*set_ref_type*/false);
            Type t = it->get_type();
            
            // Build a symbol for the new constraint based on the name of the original variable
            std::stringstream ss; ss << get_next_id(param_s);
            std::string constr_name = it->get_name() + "_" + ss.str();
            Symbol s(it->get_scope().new_symbol(constr_name));
            s.set_type(t);
            ssa_to_original_var[s] = param_s;
            
            // Get the value for the constraint
            NBase val = Nodecl::Range::make(Nodecl::Analysis::MinusInfinity::make(t), 
                                            Nodecl::Analysis::PlusInfinity::make(t), 
                                            const_value_to_nodecl(zero), t);
            
            // Build the constraint and insert it in the constraints map
            Utils::Constraint c = build_constraint(s, val, t, "Parameter");
            _output_constraints_map[param_s] = c;
        }
    }
    
    Utils::VarToConstraintMap ConstraintBuilderVisitor::get_output_constraints_map()
    {
        return _output_constraints_map;
    }

    Utils::VarToConstraintMap ConstraintBuilderVisitor::get_output_true_constraints_map()
    {
        return _output_true_constraints_map;
    }
    
    Utils::VarToConstraintMap ConstraintBuilderVisitor::get_output_false_constraints_map()
    {
        return _output_false_constraints_map;
    }
    
    void ConstraintBuilderVisitor::join_list(TL::ObjectList<Utils::Constraint>& list)
    {
        WARNING_MESSAGE("join_list of a list of constraint is not yet supported. Doing nothing.", 0);
    }
    
    Symbol ConstraintBuilderVisitor::get_condition_node_constraints(
            const NBase& lhs, const Type& t,
            std::string s_str, std::string nodecl_str)
    {
        Utils::Constraint c;
        NBase val;
        
        // 1. Compute the first constraint that corresponds to the current node: x COMP_OP c
        // -->    X1 = [0, 1]
        // 1.1 Get a new symbol for the constraint
//         std::stringstream ss; ss << get_next_id(NBase::null());
//         Symbol s_x(lhs.retrieve_context().new_symbol("_x_" + ss.str()));
//         s_x.set_type(t);
//         // 1.2 Build the value of the constraints
//         val = Nodecl::Range::make(const_value_to_nodecl(zero), const_value_to_nodecl(one), const_value_to_nodecl(one), t);
//         // 1.3 Build the actual constraint and insert it in the corresponding map
//         c = build_constraint(s_x, val, t, nodecl_str);
//         Symbol var_s(lhs.retrieve_context().new_symbol("_x"));
//         var_s.set_type(t);
//         Nodecl::Symbol var = var_s.make_nodecl(/*set_ref_type*/false);
//         _output_constraints_map[var] = c;
        
        // 2. Compute the second constraint that corresponds to the current node: x COMP_OP c
        // -->    X1 = X0
        // 2.1 Get a new symbol for the constraint
        std::stringstream ss_tmp; ss_tmp << get_next_id(lhs);
        Symbol s(lhs.retrieve_context().new_symbol(s_str + "_" + ss_tmp.str()));
        s.set_type(t);
        ssa_to_original_var[s] = lhs;
        // 2.2 Build the value of the constraints
        val = _input_constraints_map[lhs].get_symbol().make_nodecl(/*set_ref_type*/false);
        
        // 2.3 Build the actual constraint and insert it in the corresponding map
        c = build_constraint(s, val, t, nodecl_str);
        _input_constraints_map[lhs] = c;
        _output_constraints_map[lhs] = c;
        
        return s;
    }

    void ConstraintBuilderVisitor::visit_assignment(const NBase& lhs, const NBase& rhs)
    {
        // Build a symbol for the new constraint based on the name of the original variable
        std::stringstream ss; ss << get_next_id(lhs);
        Symbol orig_s(Utils::get_nodecl_base(lhs).get_symbol());
        std::string constr_name = orig_s.get_name() + "_" + ss.str();
        Symbol s(lhs.retrieve_context().new_symbol(constr_name));
        Type t = orig_s.get_type();
        s.set_type(t);
        ssa_to_original_var[s] = lhs;
        
        // Build the value of the constraint
        NBase val;
        if(rhs.is_constant())       // x = c;    -->    X1 = c
            val = Nodecl::Range::make(rhs.shallow_copy(), rhs.shallow_copy(), const_value_to_nodecl(zero), t);
        else 
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            ConstraintReplacement cr(_input_constraints_map);
            val = rhs.shallow_copy();
            cr.walk(val);
        }
        
        // Build the constraint and insert it in the corresponding maps
        Utils::Constraint c = build_constraint(s, val, t, "Assignment");
        _input_constraints_map[lhs] = c;
        _output_constraints_map[lhs] = c;
    }
    
    
    void ConstraintBuilderVisitor::visit_increment(const NBase& rhs, bool positive)
    {
        ERROR_CONDITION(_input_constraints_map.find(rhs) == _input_constraints_map.end(), 
                        "Some input constraint required for the increment's RHS %s.\n", 
                        ast_print_node_type(rhs.get_kind()));
        
        // Build a symbol for the new constraint based on the name of the original variable
        std::stringstream ss; ss << get_next_id(rhs);
        Symbol orig_s(Utils::get_nodecl_base(rhs).get_symbol());
        Type t(orig_s.get_type());
        std::string constr_name = orig_s.get_name() + "_" + ss.str();
        Symbol s(rhs.retrieve_context().new_symbol(constr_name));
        s.set_type(t);
        ssa_to_original_var[s] = rhs;
        
        NBase val;
        if(positive)
        {
            val = Nodecl::Add::make(_input_constraints_map[rhs].get_symbol().make_nodecl(/*set_ref_type*/false),
                                    const_value_to_nodecl(one), t);
        }
        else
        {
            val = Nodecl::Minus::make(_input_constraints_map[rhs].get_symbol().make_nodecl(/*set_ref_type*/false),
                                      const_value_to_nodecl(one), t);
        }
        Utils::Constraint c = build_constraint(s, val, t, "Pre/Post-in/decrement");
        _input_constraints_map[rhs] = c;
        _output_constraints_map[rhs] = c;
    }
    
    void ConstraintBuilderVisitor::visit(const Nodecl::AddAssignment& n)
    {
        NBase lhs = n.get_lhs();
        NBase rhs = n.get_rhs();
        NBase new_rhs = Nodecl::Assignment::make(lhs.shallow_copy(), 
                                                 Nodecl::Add::make(lhs.shallow_copy(), rhs.shallow_copy(), rhs.get_type()), 
                                                 lhs.get_type());
        n.replace(new_rhs);
        visit_assignment(n.get_lhs().no_conv(), n.get_rhs().no_conv());
    }
    
    void ConstraintBuilderVisitor::visit(const Nodecl::Assignment& n)
    {
        visit_assignment(n.get_lhs().no_conv(), n.get_rhs().no_conv());
    }
    
    // x == c;   ---TRUE-->    X1 = X0 ∩ [c, c]
    //           --FALSE-->    X1 = X0 ∩ ([-∞, c-1] U [c+1, -∞])
    void ConstraintBuilderVisitor::visit(const Nodecl::Equal& n)
    {
        NBase lhs = n.get_lhs().no_conv();
        NBase rhs = n.get_rhs().no_conv();
        
        // Check the input is something we expect: LHS has a constraint or is a parameter
        ERROR_CONDITION(_input_constraints_map.find(lhs) == _input_constraints_map.end(),
                        "Some input constraint required for the LHS when parsing a %s nodecl", 
                        ast_print_node_type(n.get_kind()));
        
        Symbol orig_s(Utils::get_nodecl_base(lhs).get_symbol());
        Type t = orig_s.get_type();
        std::string orig_s_str = orig_s.get_name();
        
        // 1.- Compute the conditions associated with the current node
        Symbol s = get_condition_node_constraints(lhs, t, orig_s_str, "Equal");
        
        // 2.- Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        NBase val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            ConstraintReplacement cr(_input_constraints_map);
            cr.walk(val);
        }
        // 2.1.- Compute the constraint that corresponds to the true branch taken from this node
        // x < x;       --TRUE-->       X1 = X0 ∩ [c, c]
        // 2.1.1.- Build the TRUE constraint symbol
        std::stringstream ss_true; ss_true << get_next_id(lhs);
        Symbol s_true(n.retrieve_context().new_symbol(orig_s_str + "_" + ss_true.str()));
        s_true.set_type(t);
        ssa_to_original_var[s_true] = lhs;
        // 2.1.2.- Build the TRUE constraint value
        NBase val_true = 
            Nodecl::Analysis::RangeIntersection::make(
                s.make_nodecl(/*set_ref_type*/false), 
                Nodecl::Range::make(val.shallow_copy(),
                                    val.shallow_copy(), 
                                    const_value_to_nodecl(one), t),
                t);
        // 2.1.3.- Build the TRUE constraint and store it
        Utils::Constraint c_true = build_constraint(s_true, val_true, t, "Equal TRUE");
        _output_true_constraints_map[lhs] = c_true;
        // 2.2.- Compute the constraint that corresponds to the false branch taken from this node
        // x < c;       --FALSE-->      X1 = X0 ∩ ([-∞, c-1] U [c+1, -∞])
        // 2.2.1.- Build the FALSE constraint symbol
        std::stringstream ss_false; ss_false << get_next_id(lhs);
        Symbol s_false(n.retrieve_context().new_symbol(orig_s_str + "_" + ss_false.str()));
        s_false.set_type(t);
        ssa_to_original_var[s_false] = lhs;
        // 2.2.2.- Build the FALSE constraint value
        NBase lb, ub;
        if(rhs.is_constant())
        {
            lb = const_value_to_nodecl(const_value_add(rhs.get_constant(), one));
            ub = const_value_to_nodecl(const_value_sub(rhs.get_constant(), one));
        }
        else
        {
            lb = Nodecl::Add::make(val.shallow_copy(), const_value_to_nodecl(one), t);
            ub = Nodecl::Minus::make(val.shallow_copy(), const_value_to_nodecl(one), t);
        }
        NBase val_false = 
            Nodecl::Analysis::RangeIntersection::make(
                s.make_nodecl(/*set_ref_type*/false),
                Nodecl::Analysis::RangeUnion::make(Nodecl::Range::make(Nodecl::Analysis::MinusInfinity::make(t), ub, 
                                                                       const_value_to_nodecl(one), t), 
                                                   Nodecl::Range::make(lb, Nodecl::Analysis::PlusInfinity::make(t), 
                                                                       const_value_to_nodecl(one), t), 
                                                   t),
                t);
        // 2.2.3.- Build the FALSE constraint and store it
        Utils::Constraint c_false = build_constraint(s_false, val_false, t, "Equal FALSE");
        _output_false_constraints_map[lhs] = c_false;
    }
    
    // x > c;   ---TRUE-->    X1 = X0 ∩ [ c+1, +∞ ]
    //          --FALSE-->    X1 = X0 ∩ [-∞, c]
    void ConstraintBuilderVisitor::visit(const Nodecl::GreaterThan& n)
    {
        NBase lhs = n.get_lhs().no_conv();
        NBase rhs = n.get_rhs().no_conv();
        
        // Check the input is something we expect: LHS has a constraint or is a parameter
        ERROR_CONDITION(_input_constraints_map.find(lhs) == _input_constraints_map.end(),
                        "Some input constraint required for the LHS when parsing a %s nodecl", 
                        ast_print_node_type(n.get_kind()));
        
        Symbol orig_s(Utils::get_nodecl_base(lhs).get_symbol());
        Type t = orig_s.get_type();
        std::string orig_s_str = orig_s.get_name();
        
        // 1.- Compute the conditions associated with the current node
        Symbol s = get_condition_node_constraints(lhs, t, orig_s_str, "GreaterThan");
        
        // 2.- Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        NBase val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            ConstraintReplacement cr(_input_constraints_map);
            cr.walk(val);
        }
        // 2.1.- Compute the constraint that corresponds to the true branch taken from this node
        // x < x;       --TRUE-->       X1 = X0 ∩ [ c+1, +∞ ]
        // 2.1.1.- Build the TRUE constraint symbol
        std::stringstream ss_true; ss_true << get_next_id(lhs);
        Symbol s_true(n.retrieve_context().new_symbol(orig_s_str + "_" + ss_true.str()));
        s_true.set_type(t);
        ssa_to_original_var[s_true] = lhs;
        // 2.1.2.- Build the TRUE constraint value
        NBase lb = (rhs.is_constant() ? const_value_to_nodecl(const_value_add(rhs.get_constant(), one)) 
                                      : Nodecl::Add::make(val.shallow_copy(), const_value_to_nodecl(one), t));
        NBase val_true = 
            Nodecl::Analysis::RangeIntersection::make(
                s.make_nodecl(/*set_ref_type*/false), 
                Nodecl::Range::make(lb,
                                    Nodecl::Analysis::PlusInfinity::make(t), 
                                    const_value_to_nodecl(one), t),
                t);
        // 2.1.3.- Build the TRUE constraint and store it
        Utils::Constraint c_true = build_constraint(s_true, val_true, t, "GreaterThan TRUE");
        _output_true_constraints_map[lhs] = c_true;
        // 2.2.- Compute the constraint that corresponds to the false branch taken from this node
        // x < c;       --FALSE-->      X1 = X0 ∩ [-∞, c]
        // 2.2.1.- Build the FALSE constraint symbol
        std::stringstream ss_false; ss_false << get_next_id(lhs);
        Symbol s_false(n.retrieve_context().new_symbol(orig_s_str + "_" + ss_false.str()));
        s_false.set_type(t);
        ssa_to_original_var[s_false] = lhs;
        // 2.2.2.- Build the FALSE constraint value
        NBase val_false = 
            Nodecl::Analysis::RangeIntersection::make(
                s.make_nodecl(/*set_ref_type*/false),
                Nodecl::Range::make(Nodecl::Analysis::MinusInfinity::make(t), 
                                    val.shallow_copy(), 
                                    const_value_to_nodecl(one), t),
                t);
        // 2.2.3.- Build the FALSE constraint and store it
        Utils::Constraint c_false = build_constraint(s_false, val_false, t, "GreaterThan FALSE");
        _output_false_constraints_map[lhs] = c_false;
    }
    
    // x >= c;   ---TRUE-->    X1 = X0 ∩ [ c, +∞ ]
    //           --FALSE-->    X1 = X0 ∩ [-∞, c-1]
    void ConstraintBuilderVisitor::visit(const Nodecl::GreaterOrEqualThan& n)
    {
        NBase lhs = n.get_lhs().no_conv();
        NBase rhs = n.get_rhs().no_conv();
        
        // Check the input is something we expect: LHS has a constraint or is a parameter
        ERROR_CONDITION(_input_constraints_map.find(lhs) == _input_constraints_map.end(),
                        "Some input constraint required for the LHS when parsing a %s nodecl", 
                        ast_print_node_type(n.get_kind()));
        
        Symbol orig_s(Utils::get_nodecl_base(lhs).get_symbol());
        Type t = orig_s.get_type();
        std::string orig_s_str = orig_s.get_name();
        
        // 1.- Compute the conditions associated with the current node
        Symbol s = get_condition_node_constraints(lhs, t, orig_s_str, "GreaterOrEqualThan");
        
        // 2.- Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        NBase val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            ConstraintReplacement cr(_input_constraints_map);
            cr.walk(val);
        }
        // 2.1.- Compute the constraint that corresponds to the true branch taken from this node
        // x < x;       --TRUE-->       X1 = X0 ∩ [ c, +∞ ]
        // 2.1.1.- Build the TRUE constraint symbol
        std::stringstream ss_true; ss_true << get_next_id(lhs);
        Symbol s_true(n.retrieve_context().new_symbol(orig_s_str + "_" + ss_true.str()));
        s_true.set_type(t);
        ssa_to_original_var[s_true] = lhs;
        // 2.1.2.- Build the TRUE constraint value
        NBase val_true = 
            Nodecl::Analysis::RangeIntersection::make(
                s.make_nodecl(/*set_ref_type*/false), 
                Nodecl::Range::make(val.shallow_copy(),
                                    Nodecl::Analysis::PlusInfinity::make(t), 
                                    const_value_to_nodecl(one), t),
                t);
        // 2.1.3.- Build the TRUE constraint and store it
        Utils::Constraint c_true = build_constraint(s_true, val_true, t, "GreaterOrEqualThan TRUE");
        _output_true_constraints_map[lhs] = c_true;
        // 2.2.- Compute the constraint that corresponds to the false branch taken from this node
        // x < c;       --FALSE-->      X1 = X0 ∩ [-∞, c-1]
        // 2.2.1.- Build the FALSE constraint symbol
        std::stringstream ss_false; ss_false << get_next_id(lhs);
        Symbol s_false(n.retrieve_context().new_symbol(orig_s_str + "_" + ss_false.str()));
        s_false.set_type(t);
        ssa_to_original_var[s_false] = lhs;
        // 2.2.2.- Build the FALSE constraint value
        NBase ub = (rhs.is_constant() ? const_value_to_nodecl(const_value_sub(rhs.get_constant(), one)) 
                                      : Nodecl::Minus::make(val.shallow_copy(), const_value_to_nodecl(one), t));
        NBase val_false = 
            Nodecl::Analysis::RangeIntersection::make(
                s.make_nodecl(/*set_ref_type*/false),
                Nodecl::Range::make(Nodecl::Analysis::MinusInfinity::make(t), 
                                    ub, 
                                    const_value_to_nodecl(one), t),
                t);
        // 2.2.3.- Build the FALSE constraint and store it
        Utils::Constraint c_false = build_constraint(s_false, val_false, t, "GreaterOrEqualThan FALSE");
        _output_false_constraints_map[lhs] = c_false;
    }
    
    void ConstraintBuilderVisitor::visit(const Nodecl::LogicalAnd& n)
    {
        walk(n.get_lhs());
        walk(n.get_rhs());
    }
    
    // x <= c;    ---TRUE-->    X1 = X0 ∩ [-∞, c]
    //            --FALSE-->    X1 = X0 ∩ [ c+1,  +∞]
    void ConstraintBuilderVisitor::visit(const Nodecl::LowerOrEqualThan& n)
    {
        NBase lhs = n.get_lhs().no_conv();
        NBase rhs = n.get_rhs().no_conv();
        
        // Check the input is something we expect: LHS has a constraint or is a parameter
        ERROR_CONDITION(_input_constraints_map.find(lhs) == _input_constraints_map.end(),
                        "Some input constraint required for the LHS when parsing a %s nodecl", 
                        ast_print_node_type(n.get_kind()));
        
        Symbol orig_s(Utils::get_nodecl_base(lhs).get_symbol());
        Type t = orig_s.get_type();
        std::string orig_s_str = orig_s.get_name();
        
        // 1.- Compute the conditions associated with the current node
        Symbol s = get_condition_node_constraints(lhs, t, orig_s_str, "LowerOrEqualThan");
        
        // 2.- Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        NBase val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            ConstraintReplacement cr(_input_constraints_map);
            cr.walk(val);
        }
        // 2.1.- Compute the constraint that corresponds to the true branch taken from this node
        // x < c;       --TRUE-->       X1 = X0 ∩ [-∞, c]
        // 2.1.1.- Build the TRUE constraint symbol
        std::stringstream ss_true; ss_true << get_next_id(lhs);
        Symbol s_true(n.retrieve_context().new_symbol(orig_s.get_name() + "_" + ss_true.str()));
        s_true.set_type(t);
        ssa_to_original_var[s_true] = lhs;
        // 2.1.2.- Build the TRUE constraint value
        NBase val_true = 
            Nodecl::Analysis::RangeIntersection::make(
                s.make_nodecl(/*set_ref_type*/false), 
                Nodecl::Range::make(Nodecl::Analysis::MinusInfinity::make(t), 
                                    val.shallow_copy(), 
                                    const_value_to_nodecl(zero), t),
                t);
        // 2.1.3.- Build the TRUE constraint and store it
        Utils::Constraint c_true = build_constraint(s_true, val_true, t, "LowerOrEqualThan TRUE");
        _output_true_constraints_map[lhs] = c_true;
        
        // 2.2.- Compute the constraint that corresponds to the false branch taken from this node
        // x < c;       --FALSE-->      X1 = X0 ∩ [c+1, +∞]
        // 2.2.1.- Build the FALSE constraint symbol
        std::stringstream ss_false; ss_false << get_next_id(lhs);
        Symbol s_false(n.retrieve_context().new_symbol(orig_s.get_name() + "_" + ss_false.str()));
        s_false.set_type(t);
        ssa_to_original_var[s_false] = lhs;
        // 2.2.2.- Build the FALSE constraint value
        NBase lb = (rhs.is_constant() ? const_value_to_nodecl(const_value_add(rhs.get_constant(), one)) 
                                      : Nodecl::Add::make(val.shallow_copy(), const_value_to_nodecl(one), t));
        NBase val_false = 
            Nodecl::Analysis::RangeIntersection::make(
                s.make_nodecl(/*set_ref_type*/false), 
                Nodecl::Range::make(lb,
                                    Nodecl::Analysis::PlusInfinity::make(t), 
                                    const_value_to_nodecl(zero), t), 
                t);
        // 2.2.3.- Build the FALSE constraint and store it
        Utils::Constraint c_false = build_constraint(s_false, val_false, t, "LowerOrEqualThan FALSE");
        _output_false_constraints_map[lhs] = c_false;
    }
    
    // x < c;    ---TRUE-->    X1 = X0 ∩ [-∞, c-1]
    //           --FALSE-->    X1 = X0 ∩ [ c,  +∞]
    void ConstraintBuilderVisitor::visit(const Nodecl::LowerThan& n)
    {
        NBase lhs = n.get_lhs().no_conv();
        NBase rhs = n.get_rhs().no_conv();
        
        // Check the input is something we expect: LHS has a constraint or is a parameter
        ERROR_CONDITION(_input_constraints_map.find(lhs) == _input_constraints_map.end(),
                        "Some input constraint required for the LHS when parsing a %s nodecl", 
                        ast_print_node_type(n.get_kind()));
        
        Symbol orig_s(Utils::get_nodecl_base(lhs).get_symbol());
        Type t = orig_s.get_type();
        std::string orig_s_str = orig_s.get_name();
        
        // 1.- Compute the conditions associated with the current node
        Symbol s = get_condition_node_constraints(lhs, t, orig_s_str, "LowerThan");
        
        // 2.- Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        NBase val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            ConstraintReplacement cr(_input_constraints_map);
            cr.walk(val);
        }
        // 2.1.- Compute the constraint that corresponds to the true branch taken from this node
        // x < c;       --TRUE-->       X1 = X0 ∩ [-∞, c-1]
        // 2.1.1.- Build the TRUE constraint symbol
        std::stringstream ss_true; ss_true << get_next_id(lhs);
        Symbol s_true(n.retrieve_context().new_symbol(orig_s.get_name() + "_" + ss_true.str()));
        s_true.set_type(t);
        ssa_to_original_var[s_true] = lhs;
        // 2.1.2.- Build the TRUE constraint value
        NBase ub = (rhs.is_constant() ? const_value_to_nodecl(const_value_sub(rhs.get_constant(), one)) 
                                      : Nodecl::Minus::make(val.shallow_copy(), const_value_to_nodecl(one), t));
        NBase val_true = 
            Nodecl::Analysis::RangeIntersection::make(
                s.make_nodecl(/*set_ref_type*/false), 
                Nodecl::Range::make(Nodecl::Analysis::MinusInfinity::make(t), 
                                    ub, 
                                    const_value_to_nodecl(zero), t),
                t);
        // 2.1.3.- Build the TRUE constraint and store it
        Utils::Constraint c_true = build_constraint(s_true, val_true, t, "LowerThan TRUE");
        _output_true_constraints_map[lhs] = c_true;
        
        // 2.2.- Compute the constraint that corresponds to the false branch taken from this node
        // x < c;       --FALSE-->      X1 = X0 ∩ [ c, +∞]
        // 2.2.1.- Build the FALSE constraint symbol
        std::stringstream ss_false; ss_false << get_next_id(lhs);
        Symbol s_false(n.retrieve_context().new_symbol(orig_s.get_name() + "_" + ss_false.str()));
        s_false.set_type(t);
        ssa_to_original_var[s_false] = lhs;
        // 2.2.2.- Build the FALSE constraint value
        NBase val_false = 
            Nodecl::Analysis::RangeIntersection::make(
                s.make_nodecl(/*set_ref_type*/false), 
                Nodecl::Range::make(val.shallow_copy(),
                                    Nodecl::Analysis::PlusInfinity::make(t), 
                                    const_value_to_nodecl(zero), t), 
                t);
        // 2.2.3.- Build the FALSE constraint and store it
        Utils::Constraint c_false = build_constraint(s_false, val_false, t, "LowerThan FALSE");
        _output_false_constraints_map[lhs] = c_false;
    }
    
    // x % c;   ---TRUE-->    X1 = X0 ∩ [0, c-1]
    //          --FALSE-->    X1 = X0 ∩ ([-∞, -1] U [c, -∞])
    void ConstraintBuilderVisitor::visit(const Nodecl::Mod& n)
    {
        NBase lhs = n.get_lhs().no_conv();
        NBase rhs = n.get_rhs().no_conv();
        
        // Check the input is something we expect: LHS has a constraint or is a parameter
        ERROR_CONDITION(_input_constraints_map.find(lhs) == _input_constraints_map.end(),
                        "Some input constraint required for the LHS when parsing a %s nodecl", 
                        ast_print_node_type(n.get_kind()));
        
        Symbol orig_s(Utils::get_nodecl_base(lhs).get_symbol());
        Type t = orig_s.get_type();
        std::string orig_s_str = orig_s.get_name();
        
        // 1.- Compute the conditions associated with the current node
        Symbol s = get_condition_node_constraints(lhs, t, orig_s_str, "Mod");
        
        // 2.- Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        NBase val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            ConstraintReplacement cr(_input_constraints_map);
            cr.walk(val);
        }
        
        // 2.1.- Compute the constraint that corresponds to the true branch taken from this node
        // x < x;       --TRUE-->       X1 = X0 ∩ [0, c-1]
        // 2.1.1.- Build the TRUE constraint symbol
        std::stringstream ss_true; ss_true << get_next_id(lhs);
        Symbol s_true(n.retrieve_context().new_symbol(orig_s_str + "_" + ss_true.str()));
        s_true.set_type(t);
        ssa_to_original_var[s_true] = lhs;
        // 2.1.2.- Build the TRUE constraint value
        NBase ub = (rhs.is_constant() ? const_value_to_nodecl(const_value_sub(rhs.get_constant(), one)) 
                                      : Nodecl::Minus::make(val.shallow_copy(), const_value_to_nodecl(one), t));
        NBase val_true = 
            Nodecl::Analysis::RangeIntersection::make(
                s.make_nodecl(/*set_ref_type*/false), 
                Nodecl::Range::make(const_value_to_nodecl(zero),
                                    ub, 
                                    const_value_to_nodecl(one), t),
                t);
        // 2.1.3.- Build the TRUE constraint and store it
        Utils::Constraint c_true = build_constraint(s_true, val_true, t, "Mod TRUE");
        _output_true_constraints_map[lhs] = c_true;
        // 2.2.- Compute the constraint that corresponds to the false branch taken from this node
        // x < c;       --FALSE-->      X1 = X0 ∩ ([-∞, -1] U [c, -∞])
        // 2.2.1.- Build the FALSE constraint symbol
        std::stringstream ss_false; ss_false << get_next_id(lhs);
        Symbol s_false(n.retrieve_context().new_symbol(orig_s_str + "_" + ss_false.str()));
        s_false.set_type(t);
        ssa_to_original_var[s_false] = lhs;
        // 2.2.2.- Build the FALSE constraint value
        NBase val_false = 
            Nodecl::Analysis::RangeIntersection::make(
                s.make_nodecl(/*set_ref_type*/false),
                Nodecl::Analysis::RangeUnion::make(
                    Nodecl::Range::make(Nodecl::Analysis::MinusInfinity::make(t), 
                                        const_value_to_nodecl(minus_one), 
                                        const_value_to_nodecl(one), t), 
                    Nodecl::Range::make(val.shallow_copy(), 
                                        Nodecl::Analysis::PlusInfinity::make(t), 
                                        const_value_to_nodecl(one), t), 
                    t),
                t);
        // 2.2.3.- Build the FALSE constraint and store it
        Utils::Constraint c_false = build_constraint(s_false, val_false, t, "Mod FALSE");
        _output_false_constraints_map[lhs] = c_false;
    }
    
    void ConstraintBuilderVisitor::visit(const Nodecl::ObjectInit& n)
    {
        Symbol s(n.get_symbol());
        Nodecl::Symbol lhs = s.make_nodecl(/*set_ref_type*/false);
        NBase rhs = s.get_value();
        visit_assignment(lhs, rhs);
    }
    
    // FIXME Check the order of creation of constraints depending on whether the op. is pre-in/decrement or post-in/decrement
    //       Example: x = y++;              Wrong: 
    //                X0 = Y0;                     Y1 = Y0 + 1;
    //                Y1 = Y0 + 1;                 X0 = Y1;
    
    
    // x--;    -->    X1 = X0 + 1
    void ConstraintBuilderVisitor::visit(const Nodecl::Postdecrement& n)
    {
        visit_increment(n.get_rhs(), /*positive*/ false);
    }
    
    // x++;    -->    X1 = X0 + 1
    void ConstraintBuilderVisitor::visit(const Nodecl::Postincrement& n)
    {
        visit_increment(n.get_rhs(), /*positive*/ true);
    }
    
    // --x;    -->    X1 = X0 - 1
    void ConstraintBuilderVisitor::visit(const Nodecl::Predecrement& n)
    {
        visit_increment(n.get_rhs(), /*positive*/ false);
    }
    
    // ++x;    -->    X1 = X0 + 1
    void ConstraintBuilderVisitor::visit(const Nodecl::Preincrement& n)
    {
        visit_increment(n.get_rhs(), /*positive*/ true);
    }
    
    // ************************** END Visitor implementing constraint building **************************** //
    // **************************************************************************************************** //
    
    
    
    // **************************************************************************************************** //
    // ******************************* Class implementing constraint graph ******************************** //
    
    ConstraintGraph::ConstraintGraph(std::string name)
        : _name(name), _nodes(), _node_to_scc_map()
    {}
    
    CGNode* ConstraintGraph::get_node_from_ssa_var(const NBase& n)
    {
        CGValueToCGNode_map::iterator it = _nodes.find(n);
        ERROR_CONDITION (it == _nodes.end(), 
                         "No SSA variable '%s' found in Constraint Graph '%s'", 
                         n.prettyprint().c_str(), _name.c_str());
        return it->second;
    }
    
    CGNode* ConstraintGraph::insert_node(const NBase& value)
    {
        CGNode* node = ((_nodes.find(value) == _nodes.end()) ? NULL 
                                                             : _nodes[value]);
        if(node==NULL)
        {
            node = new CGNode(__CG_Sym, value);
            _nodes[value] = node;
        }
        return node;
    }
    
    CGNode* ConstraintGraph::insert_node(CGNode_type type)
    {
        CGNode* node = new CGNode(type, NBase::null());
        NBase value = Nodecl::IntegerLiteral::make(Type::get_int_type(), 
                                                   const_value_get_integer(node->get_id(), /*num_bytes*/4, /*sign*/1));
        _nodes[value] = node;
        return node;
    }
    
    void ConstraintGraph::connect_nodes(CGNode* source, CGNode* target, NBase predicate)
    {
        ObjectList<CGNode*> children = source->get_children();
        if(!children.contains(target))
        {
            CGEdge* e = source->add_child(target, /*is_back_edge*/source->get_id()>target->get_id(), predicate);
            target->add_entry(e);
        }
    }
    
    /*! Generate a constraint graph from the PCFG and the precomputed Constraints for each node
     *  A Constraint Graph is created as follows:
     *  - The nodes of the graphs are:
     *    - A. A node for each SSA variable from Constraints: c0 = ...              Node    c0
     *    - B. A node for each Constraint Value which is a Range: c1 = [lb, ub]     Node [lb, ub]
     *  - The edges are built following the rules below:
     *    - D. Constraint Phi: c2 = Phi(c0, c1)                                     Edge    c0 ---------------> c2
     *                                                                              Edge    c1 ---------------> c2
     *    - E. Constraint Intersection: c1 = c0 ∩ [lb, ub]                          Edge    c0 ----[lb, ub]---> c1
     *    - F. Constraint Range: c0 = [lb, ub]                                      Edge [lb, ub]-------------> c1
     *    - G. Constraint Arithmetic op: a. c1 = c0 + 1                             Edge    c1 ------ 1 ------> c0
     *                                   b. c1 = d + e                              Edge    d  ---------------> c1
     *                                                                              Edge    e  ---------------> c1
     *    - H. Constraint : c1 = c0                                                 Edge    c0 ---------------> c1
     */
    void ConstraintGraph::fill_constraint_graph(
        const SSAVarToValue_map& constraints,
        const NodeclList& ordered_constraints)
    {
        std::map<NBase, CGNode*> back_edges;
        for(NodeclList::const_iterator oit = ordered_constraints.begin(); oit != ordered_constraints.end(); ++oit)
        {
            std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less>::const_iterator it = constraints.find(*oit);
            ERROR_CONDITION(it == constraints.end(), 
                            "Constraint %s not found in the constraints map.\n", 
                            oit->prettyprint().c_str());
            NBase s = it->first;
            NBase val = it->second;
            
            // Insert in the CG the edges corresponding to the current Constraint
            if(val.is<Nodecl::Symbol>())
            {   // H. 
                CGNode* source = insert_node(val);
                CGNode* target = insert_node(s);  // A.
                connect_nodes(source, target);
            }
            else if(val.is<Nodecl::Analysis::Phi>())
            {   // D.
                // Create the target
                CGNode* target_phi = insert_node(__CG_Phi);
                // Create the sources
                std::queue<CGNode*> sources;
                Nodecl::List expressions = val.as<Nodecl::Analysis::Phi>().get_expressions().as<Nodecl::List>();
                for(Nodecl::List::iterator ite = expressions.begin(); ite != expressions.end(); ++ite)
                {
                    NBase e = *ite;
                    if(_nodes.find(e) == _nodes.end())
                    {   // The expression inside the Phi node comes from a back edge
                        // We will print it later so the order of node creation respects the order in the sequential code
                        back_edges.insert(std::pair<NBase, CGNode*>(e, target_phi));
                    }
                    else
                    {
                        CGNode* source = insert_node(e);
                        sources.push(source);
                    }
                }
                CGNode* target = insert_node(s);  // A.
                // Connect them
                while(!sources.empty())
                {
                    CGNode* source = sources.front();
                    sources.pop();
                    connect_nodes(source, target_phi);
                }
                connect_nodes(target_phi, target);
            }
            else if(val.is<Nodecl::Analysis::RangeIntersection>())
            {   // E.
                NBase lhs = val.as<Nodecl::Analysis::RangeIntersection>().get_lhs().no_conv();
                const NBase rhs = val.as<Nodecl::Analysis::RangeIntersection>().get_rhs().no_conv();
                
                CGNode* source = insert_node(lhs);
                CGNode* target = insert_node(s);  // A.
                connect_nodes(source, target, rhs.shallow_copy());
            }
            else if(val.is<Nodecl::Range>())
            {
                // B. Create a new node if the Constraint Value is a Range
                CGNode* source = insert_node(val);
                CGNode* target = insert_node(s);  // A.
                // F. Create edge between the Range node and the Constraint node
                connect_nodes(source, target);
            }
            else if(val.is<Nodecl::Add>())
            {   // G.
                NBase lhs = val.as<Nodecl::Add>().get_lhs().no_conv();
                const NBase rhs = val.as<Nodecl::Add>().get_rhs().no_conv();
                if(lhs.is<Nodecl::Symbol>())
                {
                    if(rhs.is<Nodecl::Symbol>())
                    {   // G.b.
                        CGNode* source_1 = insert_node(lhs);
                        CGNode* source_2 = insert_node(rhs);
                        
                        CGNode* target_add = insert_node(__CG_Add);
                        CGNode* target = insert_node(s);  // A.
                        
                        connect_nodes(source_1, target);
                        connect_nodes(source_2, target);
                        connect_nodes(target_add, target);
                    }
                    else if(rhs.is_constant())
                    {   // G.a.
                        CGNode* source = insert_node(lhs);
                        CGNode* target = insert_node(s);  // A.
                        Nodecl::Plus predicate = Nodecl::Plus::make(rhs.shallow_copy(), rhs.get_type());
                        connect_nodes(source, target, predicate);
                    }
                    else
                    {
                        internal_error("Unexpected Add constraint value %s. Expected 's+s', 'c+s' or 's+c'.\n", val.prettyprint().c_str());
                    }
                }
                else if(lhs.is_constant())
                {
                    if(rhs.is<Nodecl::Symbol>())
                    {   // G.a.
                        CGNode* source = insert_node(rhs);
                        CGNode* target = insert_node(s);  // A.
                        connect_nodes(source, target);
                    }
                    else
                    {
                        internal_error("Unexpected Add constraint value %s. Expected 's+s', 'c+s' or 's+c'.\n", val.prettyprint().c_str());
                    }
                }
                else
                {
                    internal_error("Unexpected Add constraint value %s. Expected 's+s', 'c+s' or 's+c'.\n", val.prettyprint().c_str());
                }
            }
            else if(val.is<Nodecl::Minus>())
            {   // G.
                NBase lhs = val.as<Nodecl::Add>().get_lhs().no_conv();
                const NBase rhs = val.as<Nodecl::Add>().get_rhs().no_conv();
                if(lhs.is<Nodecl::Symbol>())
                {
                    if(rhs.is<Nodecl::Symbol>())
                    {   // G.b.
                        CGNode* source_1 = insert_node(lhs);
                        CGNode* source_2 = insert_node(rhs);
                        
                        CGNode* target_sub = insert_node(__CG_Sub);
                        CGNode* target = insert_node(s);  // A.
                        
                        connect_nodes(source_1, target_sub);
                        connect_nodes(source_2, target_sub);
                        connect_nodes(target_sub, target);
                    }
                    else if(rhs.is_constant())
                    {   // G.a.
                        CGNode* source = insert_node(lhs);
                        CGNode* target = insert_node(s);  // A.
                        NBase predicate = Nodecl::Neg::make(rhs.shallow_copy(), rhs.get_type());
                        connect_nodes(source, target, predicate);
                    }
                    else
                    {
                        internal_error("Unexpected Minus constraint value %s. Expected 's+s', 'c+s' or 's+c'.\n", val.prettyprint().c_str());
                    }
                }
                else if(lhs.is_constant())
                {
                    if(rhs.is<Nodecl::Symbol>())
                    {   // G.a.
                        CGNode* source = insert_node(rhs);
                        CGNode* target = insert_node(s);  // A.
                        NBase neg_lhs = Nodecl::Neg::make(lhs.shallow_copy(), lhs.get_type());
                        const_value_t* c_val = calc.compute_const_value(neg_lhs);
                        NBase predicate = Nodecl::IntegerLiteral::make(Type::get_int_type(), c_val);
                        connect_nodes(source, target, predicate);
                    }
                    else
                    {
                        internal_error("Unexpected Minus constraint value %s. Expected 's+s', 'c+s' or 's+c'.\n", val.prettyprint().c_str());
                    }
                }
                else
                {
                    internal_error("Unexpected Minus constraint value %s. Expected 's+s', 'c+s' or 's+c'.\n", val.prettyprint().c_str());
                }
            }
            else
            {
                internal_error("Unexpected type of Constraint value '%s' for constraint '%s'.\n", 
                               ast_print_node_type(val.get_kind()), val.prettyprint().c_str());
            }
        }
        
        // Connect now the back edges
        for(std::map<NBase, CGNode*>::iterator it = back_edges.begin(); it != back_edges.end(); ++it)
        {   // Both source and target must exist already
            CGNode* source = insert_node(it->first);
            CGNode* target = it->second;
            connect_nodes(source, target);
        }
    }
    
    void ConstraintGraph::print_graph()
    {
        // Get a file to print a DOT with the Constraint Graph
        // Create the directory of dot files if it has not been previously created
        char buffer[1024];
        char* err = getcwd(buffer, 1024);
        if(err == NULL)
            internal_error ("An error occurred while getting the path of the current directory", 0);
        struct stat st;
        std::string directory_name = std::string(buffer) + "/dot/";
        if(stat(directory_name.c_str(), &st) != 0)
        {
            int dot_directory = mkdir(directory_name.c_str(), S_IRWXU);
            if(dot_directory != 0)
                internal_error ("An error occurred while creating the dot directory in '%s'", 
                                directory_name.c_str());
        }
        
        // Create the file where we will store the DOT CG
        std::string dot_file_name = directory_name + _name + "_cg.dot";
        std::ofstream dot_cg;
        dot_cg.open(dot_file_name.c_str());
        if(!dot_cg.good())
            internal_error ("Unable to open the file '%s' to store the CG.", dot_file_name.c_str());
        if(VERBOSE)
            std::cerr << "- CG DOT file '" << dot_file_name << "'" << std::endl;
        dot_cg << "digraph CG {\n";
        dot_cg << "\tcompound=true;\n";
        for(CGValueToCGNode_map::iterator it = _nodes.begin(); it != _nodes.end(); ++it)
        {
            CGNode* n = it->second;
            unsigned int source = n->get_id();
            
            // 1.- Print the Constraint Node
            CGNode_type node_t = n->get_type();
            // 1.1.- The node has a constraint associated
            if(node_t == __CG_Sym)
            {
                NBase constraint = n->get_constraint();
                dot_cg << "\t" << source << " [label=\"[" << source << "] " << constraint.prettyprint() << "\"];\n";
                NBase val = n->get_valuation();
                if(!constraint.is<Nodecl::Range>() && !val.is_null())
                {
                    // Print a node containing the valuation
                    dot_cg << "\t0" << source << " [label=\"" << val.prettyprint() << "\", "
                           << "style=\"dashed\", color=\"gray55\", fontcolor=\"gray27\", shape=\"polygon\"];\n";
                    // Connect it to its constraint node
                    dot_cg << "\t" << source << "->" << "0" << source << " [label=\"\", style=\"dashed\", color=\"gray55\"];\n";
                    // set the same rank to the constraint node an its valuation node
                    dot_cg << "\t{rank=same; " << source << "; 0" << source << ";}";
                }
            }
            else
            {
                dot_cg << "\t" << source << " [label=\"[" << source << "] " << n->get_type_as_str() << "\"];\n";
            }
            
            // Print the node relations
            ObjectList<CGEdge*> exits = n->get_exits();
            for(ObjectList<CGEdge*>::iterator ite = exits.begin(); ite != exits.end(); ++ite)
            {
                unsigned int target = (*ite)->get_target()->get_id();
                const NBase predicate = (*ite)->get_predicate();
                bool back_edge = (*ite)->is_back_edge();
                std::string attrs = " [label=\"" + (predicate.is_null() ? "" : predicate.prettyprint()) + "\","
                                  + " style=\"" + (back_edge ? "dotted" : "solid") + "\"]";
                dot_cg << "\t" << source << "->" << target << attrs << ";\n";
            }
        }
        dot_cg << "}\n";
        dot_cg.close();
        if(!dot_cg.good())
            internal_error ("Unable to close the file '%s' where CG has been stored.", dot_file_name.c_str());
    }
    
namespace {
    bool stack_contains_cgnode(const std::stack<CGNode*>& s, CGNode* n)
    {
        bool result = false;
        std::stack<CGNode*> tmp = s;
        while(!tmp.empty() && !result)
        {
            if(tmp.top()==n)
                result = true;
            tmp.pop();
        }
        return result;
    }
}
    
    void ConstraintGraph::strong_connect(CGNode* n, unsigned int& scc_current_index, 
                                         std::stack<CGNode*>& s, std::vector<SCC*>& scc_list, 
                                         std::map<CGNode*, int>& scc_lowlink_index,
                                         std::map<CGNode*, int>& scc_index)
    {
        // Set the depth index for 'n' to the smallest unused index
        scc_index[n] = scc_current_index;
        scc_lowlink_index[n] = scc_current_index;
        ++scc_current_index;
        s.push(n);
        
        // Consider the successors of 'n'
        ObjectList<CGNode*> succ = n->get_children();
        for(ObjectList<CGNode*>::iterator it = succ.begin(); it != succ.end(); ++it)
        {
            CGNode* m = *it;
            if(scc_index.find(m)==scc_index.end())
            {   // Initialize values for this node if it has not yet been initialized
                scc_index[m] = -1;
                scc_lowlink_index[m] = -1;
            }
            if(scc_index[m] == -1)
            {   // Successor 'm' has not yet been visited: recurse on it
                strong_connect(m, scc_current_index, s, scc_list, scc_lowlink_index, scc_index);
                scc_lowlink_index[n] = std::min(scc_lowlink_index[n], scc_lowlink_index[m]);
            }
            else if(stack_contains_cgnode(s, m))
            {   // Successor 'm' is in the current SCC
                scc_lowlink_index[n] = std::min(scc_lowlink_index[n], scc_index[m]);
            }
        }   
        
        // If 'n' is a root node, pop the set and generate an SCC
        if((scc_lowlink_index[n] == scc_index[n]) && !s.empty())
        {
            SCC* scc = new SCC(&_node_to_scc_map);
            while(!s.empty() && s.top()!=n)
            {
                scc->add_node(s.top());
                s.pop();
            }
            if(!s.empty() && s.top()==n)
            {
                scc->add_node(s.top());
                s.pop();
            }
            scc_list.push_back(scc);
        }
    }
    
    // Implementation of the Tarjan's strongly connected components algorithm
    std::vector<SCC*> ConstraintGraph::topologically_compose_strongly_connected_components()
    {
        std::vector<SCC*> scc_list;
        std::stack<CGNode*> s;
        unsigned int scc_current_index = 0;
        
        // Collect each set of nodes that form a SCC
        std::map<CGNode*, int> scc_lowlink_index;
        std::map<CGNode*, int> scc_index;
        for(CGValueToCGNode_map::iterator it = _nodes.begin(); it != _nodes.end(); ++it)
        {
            CGNode* n = it->second;
            if((scc_index.find(n) == scc_index.end()) || (scc_index[n] == -1))
                strong_connect(n, scc_current_index, s, scc_list, scc_lowlink_index, scc_index);
        }
        
        // Create a map between the Constraint Graph nodes and their SCC
        for(std::vector<SCC*>::iterator it = scc_list.begin(); it != scc_list.end(); ++it)
        {
            std::vector<CGNode*> scc_nodes = (*it)->get_nodes();
            for(std::vector<CGNode*>::iterator itt = scc_nodes.begin(); itt != scc_nodes.end(); ++itt)
            {
                _node_to_scc_map.insert(std::pair<CGNode*, SCC*>(*itt, *it));
            }
        }
        
        // Compute the root of each SCC
        for(std::vector<SCC*>::iterator it = scc_list.begin(); it != scc_list.end(); ++it)
        {
            SCC* scc = *it;
            std::vector<CGNode*> nodes = scc->get_nodes();
            if(scc->is_trivial())
            {
                scc->set_root(nodes[0]);
            }
            else
            {
                for(std::vector<CGNode*>::iterator itt = nodes.begin(); itt != nodes.end(); ++itt)
                {
                    ObjectList<CGNode*> parents = (*itt)->get_parents();
                    for(ObjectList<CGNode*>::iterator ittt = parents.begin(); ittt != parents.end(); ++ittt)
                    {
                        if(_node_to_scc_map[*ittt] != scc)
                        {
                            scc->set_root(*itt);
                            goto root_done;
                        }
                    }
                }
            }
root_done:  ;
        }
        
        print_sccs(scc_list);
        
        // Collect the roots of each SCC tree
        std::vector<SCC*> roots;
        for(std::map<CGNode*, SCC*>::iterator it = _node_to_scc_map.begin(); it != _node_to_scc_map.end(); ++it)
        {
            if(it->first->get_entries().empty())
                roots.push_back(it->second);
        }
        
        return roots;
    }
    
    void ConstraintGraph::evaluate_cgnode(CGNode* const node)
    {
        NBase valuation;
        CGNode_type type = node->get_type();
        NBase constraint = node->get_constraint();
        if(!constraint.is_null() && constraint.is<Nodecl::Range>())
        {
            valuation = constraint;
        }
        else
        {
            NodeclList entry_valuations;
            ObjectList<CGEdge*> entries = node->get_entries();
            ERROR_CONDITION(entries.empty(), 
                            "CG node %d representing symbol or operation has no entries. Expected at least one entry.\n", 
                            node->get_id());
            for(ObjectList<CGEdge*>::iterator it = entries.begin(); it != entries.end(); ++it)
            {
                if((*it)->is_back_edge())
                    continue;
                Nodecl::Range last_valuation = (*it)->get_source()->get_valuation().as<Nodecl::Range>();
                NBase predicate = (*it)->get_predicate();
                if(predicate.is_null())
                    valuation = last_valuation;
                else if(predicate.is<Nodecl::Plus>())
                    valuation = Utils::range_value_add(last_valuation, predicate.as<Nodecl::Plus>().get_rhs());
                else if(predicate.is<Nodecl::Neg>())
                    valuation = Utils::range_value_subtract(last_valuation, predicate.as<Nodecl::Neg>().get_rhs());
                else if(predicate.is<Nodecl::Range>() || predicate.is<Nodecl::Analysis::RangeUnion>())
                {
                    // Check whether the cycle is positive or negative
                    SCC* scc = _node_to_scc_map[node];
                    Utils::CycleDirection cycle_direction = scc->get_cycle_direction(*it);
                    valuation = Utils::range_intersection(last_valuation, predicate, cycle_direction);
                }
                else
                    internal_error("Unexpected type %s of CG predicate %s. Expected IntegerLiteral or Range.\n", 
                                   ast_print_node_type(predicate.get_kind()), predicate.prettyprint().c_str());
                    entry_valuations.append(valuation);
            }
            
            NodeclList::iterator it = entry_valuations.begin();
            if(type == __CG_Sym)
            {
                ERROR_CONDITION(entry_valuations.size()>1, 
                                "Only one entry valuation expected for a Sym CGNode but %d found.\n", 
                                entry_valuations.size());
                valuation = *it;
            }
            else
            {
                if(type == __CG_Phi)
                    valuation = join_valuations(Utils::range_union, entry_valuations);         // RANGE_UNION
                else if(type == __CG_Add)
                    valuation = join_valuations(Utils::range_addition, entry_valuations);      // RANGE_ADD
                else if(type == __CG_Sub)
                    valuation = join_valuations(Utils::range_subtraction, entry_valuations);   // RANGE_SUB
            }
        }
#ifdef RANGES_DEBUG
        std::cerr << "    EVALUATE " << node->get_id() << "  ::  " << valuation.prettyprint() << std::endl;
#endif
        node->set_valuation(valuation);
    }
    
    void ConstraintGraph::resolve_cycle(SCC* scc)
    {
        // Evaluate the root of the SCC
        CGNode* root = scc->get_root();
        evaluate_cgnode(root);
        
        // Saturate all non-saturated edges in the SCC
        // Propagate the rest of edges
        std::queue<CGNode*> next_nodes;
        ObjectList<CGNode*> children = root->get_children();
        for(ObjectList<CGNode*>::iterator it = children.begin(); it != children.end(); ++it)
            next_nodes.push(*it);
        std::set<CGNode*> visited;
        visited.insert(root);
        ObjectList<CGEdge*> back_edges;
        while(!next_nodes.empty())
        {
            CGNode* n = next_nodes.front();
            next_nodes.pop();
            
            // If the node is not in the same SCC, then we will treat it later
            if(_node_to_scc_map[n] != scc)
                continue;
            
            // If the node was already treated, there is nothing to be done
            const ObjectList<CGEdge*> entries = n->get_entries();
            if(visited.find(n) != visited.end())
            {
                for(ObjectList<CGEdge*>::const_iterator it = entries.begin(); it != entries.end(); ++it)
                {
                    if((*it)->is_back_edge())
                        back_edges.append(*it);
                }
                continue;
            }
            
            // Check whether the node can be treated or not: all parent have already been treated
            bool is_ready = true;
            for(ObjectList<CGEdge*>::const_iterator it = entries.begin(); it != entries.end(); ++it)
            {
                if((*it)->get_source()->get_valuation().is_null())
                {
                    is_ready = false;
                    break;
                }
            }
            if(!is_ready)
            {   // Insert the node again in the queue and keep iterating
                next_nodes.push(n);
                continue;
            }
            
            // Treat the node and insert it in the list of treated nodes    
            evaluate_cgnode(n);
            visited.insert(n);
            
            // Add the children of the node to the queue
            ObjectList<CGNode*> n_children = n->get_children();
            for(ObjectList<CGNode*>::iterator it = n_children.begin(); it != n_children.end(); ++it)
                next_nodes.push(*it);
        }
        
        NBase valuation;
        for(ObjectList<CGEdge*>::iterator it = back_edges.begin(); it != back_edges.end(); ++it)
        {
            NBase predicate = (*it)->get_predicate();
            ERROR_CONDITION(!predicate.is_null(), 
                            "Propagation in back edges with a predicate is not yet implemented.\n", 0);
            NBase source_valuation = (*it)->get_source()->get_valuation();
            if(!source_valuation.is_null())
            {
                CGNode* target = (*it)->get_target();
                NBase target_valuation = target->get_valuation();
                if(target_valuation.is_null())
                    valuation = source_valuation;
                else
                    valuation = Utils::range_union(source_valuation, target_valuation);
                target->set_valuation(valuation);
                ObjectList<CGEdge*> exits = target->get_exits();
                ERROR_CONDITION(exits.size() != 1, 
                                "The number of children of a Phi node is expected to be one, but %d found for node %d.\n", 
                                exits.size(), target->get_id());
                ERROR_CONDITION(!exits[0]->get_predicate().is_null(), 
                                "Null predicate expected for a back-edge, but %s found for edge %d->%d.\n", 
                                exits[0]->get_predicate().prettyprint().c_str(), target->get_id(), exits[0]->get_target()->get_id());
                exits[0]->get_target()->set_valuation(valuation);
#ifdef RANGES_DEBUG
                std::cerr << "        PROPAGATE back edge to " << target->get_id() << " and " << exits[0]->get_target()->get_id() 
                          << "  ::  " << valuation.prettyprint() << std::endl;
#endif
            }
        }
    }
    
    void ConstraintGraph::solve_constraints(const std::vector<SCC*>& roots)
    {
#ifdef RANGES_DEBUG
        std::cerr << "________________________________________________" << std::endl;
        std::cerr << "SOLVE CONSTRAINTS:" << std::endl;
        std::cerr << "------------------" << std::endl;
#endif
        std::queue<SCC*> next_scc;
        for(std::vector<SCC*>::const_iterator it = roots.begin(); it != roots.end(); ++it)
            next_scc.push(*it);
        std::set<SCC*> visited;
        while(!next_scc.empty())
        {
            SCC* scc = next_scc.front();
            next_scc.pop();
            
            // Check whether this scc is ready to be solved
            CGNode* root = scc->get_root();
            ObjectList<CGEdge*> entries = root->get_entries();
            bool is_ready = true;
            for(ObjectList<CGEdge*>::iterator it = entries.begin(); it != entries.end(); ++it)
            {
                if((*it)->is_back_edge())
                    continue;
                if((*it)->get_source()->get_valuation().is_null())
                {
                    is_ready = false;
                    break;
                }
            }
            if(!is_ready)
            {
                next_scc.push(scc);
                continue;
            }
            
            // Treat the current SCC
            if(scc->is_trivial())
            {   // Evaluate the only node within the SCC
#ifdef RANGES_DEBUG
                std::cerr << "  SCC " << scc->get_id() << "  ->  TRIVIAL" << std::endl;
#endif
                evaluate_cgnode(scc->get_nodes()[0]);
            }
            else
            {   // Cycle resolution
#ifdef RANGES_DEBUG
                std::cerr << "  SCC " << scc->get_id() << "  ->  RESOLVE CYCLE" << std::endl;
#endif
                resolve_cycle(scc);
            }
            visited.insert(scc);
            
            // Prepare next iterations, if there are
            // We will add more than one child here when a single node generates more than one constraint
            ObjectList<SCC*> scc_exits = scc->get_scc_exits();
            for(ObjectList<SCC*>::iterator it = scc_exits.begin(); it != scc_exits.end(); ++it)
            {
                if(visited.find(*it) == visited.end())
                {
                    next_scc.push(*it);
                }
            }
        }
    }
    
    // ***************************** END class implementing constraint graph ****************************** //
    // **************************************************************************************************** //
    
    
    
    // **************************************************************************************************** //
    // ******************************** Class implementing range analysis ********************************* //
    
    RangeAnalysis::RangeAnalysis(ExtensibleGraph* pcfg)
        : _pcfg(pcfg), _cg(new ConstraintGraph(pcfg->get_name())), 
          _constraints(), _ordered_constraints()
    {}
    
    void RangeAnalysis::compute_range_analysis()
    {   
        std::map<Node*, Utils::VarToConstraintMap> constr_map;
        std::map<Node*, Utils::VarToConstraintMap> propagated_constr_map;
        
        // 1.- Compute the constraints of the current PCFG
        compute_constraints(constr_map, propagated_constr_map);

        // 2.- Build the Constraint Graph (CG) from the computed constraints
        build_constraint_graph();
        
        // 3.- Extract the Strongly Connected Components (SCC) of the graph
        //     And get the root of each topologically ordered subgraph
        std::vector<SCC*> roots = _cg->topologically_compose_strongly_connected_components();
        
        // 4.- Constraints evaluation
        _cg->solve_constraints(roots);
        if(VERBOSE)
            _cg->print_graph();
        
        // 5.- Insert computed ranges in the PCFG
        set_ranges_to_pcfg(constr_map);
        set_ranges_to_pcfg(propagated_constr_map);
    }
    
    void RangeAnalysis::compute_constraints(
        std::map<Node*, Utils::VarToConstraintMap>& constr_map,
        std::map<Node*, Utils::VarToConstraintMap>& propagated_constr_map)
    {
        compute_parameters_constraints(constr_map);
        
        Node* entry = _pcfg->get_graph()->get_graph_entry_node();
        compute_constraints_rec(entry, constr_map, propagated_constr_map);
        ExtensibleGraph::clear_visits(entry);
        
        propagate_constraints_from_back_edges(entry, constr_map, propagated_constr_map);
        ExtensibleGraph::clear_visits(entry);
        
#ifdef RANGES_DEBUG
        print_constraints();
#endif
    }
    
    void RangeAnalysis::build_constraint_graph()
    {
        _cg->fill_constraint_graph(_constraints, _ordered_constraints);
    }
    
    // Set an constraint to the graph entry node for each parameter of the function
    void RangeAnalysis::compute_parameters_constraints(std::map<Node*, Utils::VarToConstraintMap>& constr_map)
    {
        Symbol func_sym = _pcfg->get_function_symbol();
        if(!func_sym.is_valid())    // The PCFG have been built for something other than a FunctionCode
            return;

        Node* entry = _pcfg->get_graph()->get_graph_entry_node();
        const ObjectList<Symbol>& params = func_sym.get_function_parameters();
        ConstraintBuilderVisitor cbv(entry, &_constraints, &_ordered_constraints);
        cbv.compute_parameters_constraints(params);
        constr_map[entry] = cbv.get_output_constraints_map();
    }
    
    // This is a breadth-first search because for a given node we need all its parents 
    // (except from those that come from back edges, for they will be recalculated later 'propagate_constraints_from_back_edges')
    // to be computed before propagating their information to the node
    void RangeAnalysis::compute_constraints_rec(
            Node* entry, 
            std::map<Node*, Utils::VarToConstraintMap>& constr_map, 
            std::map<Node*, Utils::VarToConstraintMap>& propagated_constr_map)
    {
        ERROR_CONDITION(!entry->is_entry_node(), 
                        "Expected ENTRY node but found %s node.", entry->get_type_as_string().c_str());
        
        ObjectList<Node*> currents(1, entry);
        while(!currents.empty())
        {
            ObjectList<Node*>::iterator it = currents.begin();
            while(it != currents.end())
            {
                Node* n = *it;
                if(n->is_visited())
                {
                    currents.erase(it);
                    continue;
                }
                else
                {
                    n->set_visited(true);
                    ++it;
                }
                if(n->is_graph_node())
                {
                    // 1.- Recursively compute the constraints for the inner nodes
                    compute_constraints_rec(n->get_graph_entry_node(), constr_map, propagated_constr_map);
                    
                    // 2.- Propagate constraint from the inner nodes (summarized in the exit node) to the graph node
                    Node* graph_exit = n->get_graph_exit_node();
                    Utils::VarToConstraintMap exit_constrs = constr_map[graph_exit];
                    propagated_constr_map[n] = propagated_constr_map[graph_exit];
                    propagated_constr_map[n].insert(exit_constrs.begin(), exit_constrs.end());
                }
                else
                {
                    // 1.- Collect and join constraints computed for all the parents
                    Utils::VarToConstraintMap input_constrs;            // Constraint coming directly from parents
                    Utils::VarToConstraintMap new_input_constrs;        // Constraints resulting from the join of parents' constraints
                    const ObjectList<Node*>& parents = (n->is_entry_node() ? n->get_outer_node()->get_parents() 
                                                                           : n->get_parents());
                    ConstraintBuilderVisitor cbv_propagated(n, &_constraints, &_ordered_constraints);
                    for(ObjectList<Node*>::const_iterator itp = parents.begin(); itp != parents.end(); ++itp)
                    {
                        Utils::VarToConstraintMap itp_all_constrs = constr_map[*itp];
                        Utils::VarToConstraintMap itp_propagated_constrs = propagated_constr_map[*itp];
                        // We use the 'insert' method because when a constraint is already in the 'constr_map', we
                        // do not take into account the constraints being propagated from the parents
                        itp_all_constrs.insert(itp_propagated_constrs.begin(), itp_propagated_constrs.end());
                        for (Utils::VarToConstraintMap::iterator itc = itp_all_constrs.begin(); 
                             itc != itp_all_constrs.end(); ++itc)
                        {
                            const NBase& ssa_var = itc->first;
                            const Utils::Constraint& c = itc->second;
                            if(input_constrs.find(ssa_var)==input_constrs.end() && 
                               new_input_constrs.find(ssa_var)==new_input_constrs.end())
                            {   // No constraints already found for variable ssa_var
                                input_constrs[ssa_var] = c;
                            }
                            else
                            {   // Constraints for variable ssa_var already found: merge them with the new constraint
                                // 1.1- Get the existing constraint
                                Utils::Constraint old_c = 
                                    ((input_constrs.find(ssa_var) != input_constrs.end()) ? input_constrs[ssa_var] 
                                                                                          : new_input_constrs[ssa_var]);
                                NBase old_c_val = old_c.get_constraint();
                                
                                // 1.2.- If the new constraint is different from the old one, compute the combination of both
                                NBase c_nodecl = c.get_constraint();
                                if(!Nodecl::Utils::structurally_equal_nodecls(old_c_val, c_nodecl, 
                                                                              /*skip_conversion_nodes*/true))
                                {
                                    // 1.2.2.- Get a new symbol for the new constraint
                                    std::stringstream ss; ss << get_next_id(ssa_var);
                                    Symbol orig_s(ssa_var.get_symbol());
                                    std::string constr_name = orig_s.get_name() + "_" + ss.str();
                                    Symbol s(ssa_var.retrieve_context().new_symbol(constr_name));
                                    Type t(orig_s.get_type());
                                    s.set_type(t);
                                    ssa_to_original_var[s] = ssa_var;
                                    
                                    // 1.2.3.- Build the value of the new constraint
                                    NBase new_constraint_val;
                                    if(old_c_val.is<Nodecl::Analysis::Phi>())
                                    {   // Attach a new element to the list inside the node Phi
                                        Nodecl::List expressions = old_c_val.as<Nodecl::Analysis::Phi>().get_expressions().as<Nodecl::List>();
                                        expressions.append(ssa_var);
                                        new_constraint_val = Nodecl::Analysis::Phi::make(expressions, ssa_var.get_type());
                                    }
                                    else
                                    {   // Create a new node Phi with the combination of the old constraint and the new one
                                        Nodecl::Symbol tmp1 = old_c.get_symbol().make_nodecl(/*set_ref_type*/false);
                                        Nodecl::Symbol tmp2 = c.get_symbol().make_nodecl(/*set_ref_type*/false);
                                        Nodecl::List expressions = Nodecl::List::make(tmp1, tmp2);
                                        new_constraint_val = Nodecl::Analysis::Phi::make(expressions, c_nodecl.get_type());
                                    }
                                    
                                    // 1.2.4.- Remove the old constraint from the input_constrs 
                                    //         If it was in the new_input_constrs map, it will be deleted with the insertion
                                    Utils::VarToConstraintMap::iterator it_tmp = input_constrs.find(ssa_var);
                                    if(it_tmp != input_constrs.end())
                                    {
                                        input_constrs.erase(ssa_var);
                                    }
                                    // Build the actual constraint and insert it in the proper list
                                    Utils::Constraint new_c = cbv_propagated.build_constraint(s, new_constraint_val, t, "Propagated");
                                    new_input_constrs[ssa_var] = new_c;
                                }
                            }
                        }
                    }
                    
                    // 2.- Propagate constraints from parent nodes to the current node
                    constr_map[n].insert(new_input_constrs.begin(), new_input_constrs.end());
                    
                    // 3.- Compute the constraints generated in the current node
                    if(n->has_statements())
                    {
                        // 3.1.- Compute the constraints of the current node
                        // Note: take into account the constraints the node may already have (if it is the TRUE or FALSE child of a conditional)
                        Utils::VarToConstraintMap& current_constraints_map = constr_map[n];
                        ConstraintBuilderVisitor cbv(n, input_constrs, current_constraints_map, 
                                                     &_constraints, &_ordered_constraints);
                        NodeclList stmts = n->get_statements();
                        for(NodeclList::iterator itt = stmts.begin(); itt != stmts.end(); ++itt)
                            cbv.compute_stmt_constraints(*itt);
                        
                        Utils::VarToConstraintMap output_constrs = cbv.get_output_constraints_map();
                        current_constraints_map.insert(output_constrs.begin(), output_constrs.end());
                        
                        // 3.2.- Set true/false output constraints to current children, if applies
                        ObjectList<Edge*> exits = n->get_exit_edges();
                        if (exits.size()==2 &&
                            ((exits[0]->is_true_edge() && exits[1]->is_false_edge()) || (exits[1]->is_true_edge() && exits[0]->is_false_edge())))
                        {
                            Utils::VarToConstraintMap out_true_constrs = cbv.get_output_true_constraints_map();
                            Utils::VarToConstraintMap out_false_constrs = cbv.get_output_false_constraints_map();
                            
                            // 3.2.1.- We always propagate to the TRUE edge
                            Node* true_node = (exits[0]->is_true_edge() ? exits[0]->get_target() : exits[1]->get_target());
                            Node* real_true_node = true_node;
                            while(true_node->is_exit_node())
                                true_node = true_node->get_outer_node()->get_children()[0];
                            if(true_node->is_graph_node())
                                true_node = true_node->get_graph_entry_node();
                            constr_map[true_node].insert(out_true_constrs.begin(), out_true_constrs.end());
                            
                            // 3.2.2.- For the if_else cases, we only propagate to the FALSE edge when it contains statements ('else' statements)
                            Node* false_node = (exits[0]->is_true_edge() ? exits[1]->get_target() : exits[0]->get_target());
                            ObjectList<Node*> real_true_node_children = real_true_node->get_children();
                            if((false_node->get_entry_edges().size() == 1) || !real_true_node_children.contains(false_node))
                            {   // If the true_node is a parent of the false_node, then there are no statements
                                // Avoid cases where the FALSE edge leads to the end of the graph
                                ObjectList<Node*> children;
                                while(false_node->is_exit_node())
                                {
                                    children = false_node->get_outer_node()->get_children();
                                    if(!children.empty())
                                        false_node = children[0];
                                    else 
                                    {
                                        false_node = NULL;
                                        break;
                                    }
                                }
                                if(false_node!=NULL)
                                {
                                    if(false_node->is_graph_node())
                                        false_node = false_node->get_graph_entry_node();
                                    constr_map[false_node].insert(out_false_constrs.begin(), out_false_constrs.end());
                                }
                            }
                        }
                    }
                    
                    // 4.- Purge propagated constraints:
                    // When the node generates a constraint for a given variable
                    // any propagated constraint from parents for that variable is deleted here
                    for (Utils::VarToConstraintMap::iterator itt = constr_map[n].begin(); 
                         itt != constr_map[n].end(); ++itt)
                    {
                        Utils::VarToConstraintMap::iterator ittt = input_constrs.find(itt->first);
                        if(ittt != input_constrs.end())
                            input_constrs.erase(ittt);
                    }
                    propagated_constr_map[n] = input_constrs;
                }
            }
            
            ObjectList<Node*> next_currents;
            for(ObjectList<Node*>::iterator itt = currents.begin(); itt != currents.end(); ++itt)
                next_currents.append((*itt)->get_children());
            currents = next_currents;
        }
    }
    
    static void recompute_node_constraints(
            Node* n, 
            const Utils::VarToConstraintMap& new_constraint_map, 
            SSAVarToValue_map *constraints,
            NodeclList *ordered_constraints, 
            std::map<Node*, Utils::VarToConstraintMap>& constr_map)
    {
        Utils::VarToConstraintMap constrs = constr_map[n];
        ConstraintBuilderVisitor cbv(n, constraints, ordered_constraints);
        for (Utils::VarToConstraintMap::const_iterator it = new_constraint_map.begin(); 
             it != new_constraint_map.end(); ++it)
        {
            if ((constrs.find(it->first) != constrs.end()) && 
                (constrs[it->first] != it->second))
            {
                NBase c1 = constrs[it->first].get_constraint().shallow_copy();
                NBase c2 = it->second.get_symbol().make_nodecl(/*set_ref_type*/false);
                Nodecl::List expressions = Nodecl::List::make(c1, c2);
                NBase c_val = Nodecl::Analysis::Phi::make(expressions, Utils::get_nodecl_base(it->first).get_symbol().get_type());
                // Set the new constraint
                Utils::Constraint c = cbv.build_constraint(
                        constrs[it->first].get_symbol(), c_val, c_val.get_type(), "Recomputed");
                constrs[it->first] = c;
            }
        }
        constr_map[n] = constrs;
    }
    
    void RangeAnalysis::propagate_constraints_from_back_edges(
            Node* n, 
            std::map<Node*, Utils::VarToConstraintMap>& constr_map, 
            std::map<Node*, Utils::VarToConstraintMap>& propagated_constr_map)
    {
        if(n->is_visited())
            return;
        n->set_visited(true);
        
        if(n->is_graph_node())
            propagate_constraints_from_back_edges(n->get_graph_entry_node(), constr_map, propagated_constr_map);
        
        // Check the exit edges looking for back edges
        ObjectList<Edge*> exit_edges = n->get_exit_edges();
        for(ObjectList<Edge*>::iterator it = exit_edges.begin(); it != exit_edges.end(); ++it)
        {
            if((*it)->is_back_edge())
            {
                Utils::VarToConstraintMap all_constrs = constr_map[n];
                Utils::VarToConstraintMap propagated_constrs = propagated_constr_map[n];
                all_constrs.insert(propagated_constrs.begin(), propagated_constrs.end());
                recompute_node_constraints((*it)->get_target(), all_constrs, 
                                           &_constraints, &_ordered_constraints, constr_map);
            }
        }
        
        // Recursively treat the children
        ObjectList<Node*> children = n->get_children();
        for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            propagate_constraints_from_back_edges(*it, constr_map, propagated_constr_map);
    }
    
    void RangeAnalysis::set_ranges_to_pcfg(
        const std::map<Node*, Utils::VarToConstraintMap>& constr_map)
    {
        for (std::map<Node*, Utils::VarToConstraintMap>::const_iterator it = constr_map.begin();
             it != constr_map.end(); ++it)
        {
            const Utils::VarToConstraintMap constraints = it->second;
            if(!constraints.empty())
            {
#ifdef RANGES_DEBUG
//                 std::cerr << "    " << it->first->get_id() << std::endl;
#endif
                for(Utils::VarToConstraintMap::const_iterator itt = constraints.begin(); itt != constraints.end(); ++itt)
                {
                    Symbol s(itt->second.get_symbol());
                    std::map<Symbol, NBase, Nodecl::Utils::Nodecl_structural_less>::iterator ssa_to_var_it;
                    ssa_to_var_it = ssa_to_original_var.find(s);
                    ERROR_CONDITION (ssa_to_var_it == ssa_to_original_var.end(), 
                                     "SSA symbol '%s' is not related to any variable of the original code\n", 
                                     s.get_name().c_str());
                    CGNode* n = _cg->get_node_from_ssa_var(itt->second.get_symbol().make_nodecl(/*set_ref_type*/false));
                    it->first->set_range(ssa_to_var_it->second, n->get_valuation());
#ifdef RANGES_DEBUG
//                     std::cerr << "        " << s.get_name() << " : " << ssa_to_var_it->second.prettyprint() << " -> " << n->get_valuation().prettyprint() <<  std::endl;
#endif
                }
            }
        }
    }
    
    void RangeAnalysis::print_constraints()
    {
        std::cerr << "________________________________________________" << std::endl;
        std::cerr << "CONSTRAINT MAP:" << std::endl;
        std::cerr << "---------------" << std::endl;
        for(NodeclList::iterator it = _ordered_constraints.begin(); it != _ordered_constraints.end(); ++it)
        {
            std::pair<NBase, NBase> c = *_constraints.find(*it);
            std::cerr << "    " << c.first.prettyprint() << "  ->  " << c.second.prettyprint() << std::endl;
        }
    }
    
    // ****************************** End class implementing range analysis ******************************* //
    // **************************************************************************************************** //
}
}
