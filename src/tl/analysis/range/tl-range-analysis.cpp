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
#include "tl-expression-reduction.hpp"
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
    
    ConstraintReplacement::ConstraintReplacement(
            Utils::VarToConstraintMap* constraints_map,
            Node* n,
            SSAVarToValue_map *constraints,
            NodeclList *ordered_constraints)
        : _constraints_map(constraints_map),
          _n(n),
          _constraints(constraints),
          _ordered_constraints(ordered_constraints) // Attributes needed to create new constraints
    {}
    
    void ConstraintReplacement::visit(const Nodecl::ArraySubscript& n)
    {
        if (_constraints_map->find(n) != _constraints_map->end())
            n.replace((*_constraints_map)[n].get_symbol().make_nodecl(/*set_ref_type*/false));
        else
        {
            // Create a new ssa variable here to use it from now on in the current function
            // 1. Build a symbol for the new constraint based on the name of the original variable
            std::stringstream ss; ss << get_next_id(n);
            Symbol orig_s(Utils::get_nodecl_base(n).get_symbol());
            std::string subscripts_str;
            const Nodecl::List& subscripts = n.get_subscripts().as<Nodecl::List>();
            for (Nodecl::List::const_iterator it = subscripts.begin(); it != subscripts.end(); ++it)
            {
                if (_constraints_map->find(*it) != _constraints_map->end())
                    subscripts_str += (*_constraints_map)[*it].get_symbol().get_name();
                else    // The subscript is a global variable
                    subscripts_str += it->prettyprint();
                subscripts_str += "_";
            }
            std::string constr_name = orig_s.get_name() + "_" + subscripts_str + ss.str();
            Symbol s(n.retrieve_context().new_symbol(constr_name));
            Type t = orig_s.get_type();
            s.set_type(t);
            ssa_to_original_var[s] = n;
            // 2. Get the value for the constraint
            NBase val = Nodecl::Range::make(Nodecl::Analysis::MinusInfinity::make(t),
                                            Nodecl::Analysis::PlusInfinity::make(t),
                                            const_value_to_nodecl(zero), t);
            // 3. Build the constraint and insert it in the constraints map
            ConstraintBuilderVisitor cbv(_n, *_constraints_map, _constraints, _ordered_constraints);
            Utils::Constraint c = cbv.build_constraint(s, val, t, __GlobalVar);
            (*_constraints_map)[n] = c;

            n.replace(s.make_nodecl(/*set_ref_type*/false));
        }
    }

    void ConstraintReplacement::visit(const Nodecl::Cast& n)
    {   // Remove casts from the constraints
        n.replace(n.get_rhs());
        walk(n);
    }

    void ConstraintReplacement::visit(const Nodecl::ClassMemberAccess& n)
    {
        if (_constraints_map->find(n) != _constraints_map->end())
            n.replace((*_constraints_map)[n].get_symbol().make_nodecl(/*set_ref_type*/false));
        else
        {
            walk(n.get_lhs());
            walk(n.get_member());
        }
    }

    void ConstraintReplacement::visit(const Nodecl::FunctionCall& n)
    {
        if (_constraints_map->find(n) != _constraints_map->end())
            n.replace((*_constraints_map)[n].get_symbol().make_nodecl(/*set_ref_type*/false));
        else
        {
            Type t(Type::get_int_type());
            NBase val = Nodecl::Range::make(
                    Nodecl::Analysis::MinusInfinity::make(t),
                    Nodecl::Analysis::PlusInfinity::make(t),
                    const_value_to_nodecl(zero), t);
            n.replace(val);
        }
    }

    void ConstraintReplacement::visit(const Nodecl::Symbol& n)
    {
        if (_constraints_map->find(n) == _constraints_map->end())
        {
            // FunctionCalls are replaced with the value [-inf, +inf]
            if (n.get_symbol().is_function())
            {
                // 1. Build a symbol for the new constraint based on the name of the original variable
                std::stringstream ss; ss << get_next_id(n);
                Symbol orig_s(Utils::get_nodecl_base(n).get_symbol());
                std::string constr_name = orig_s.get_name() + "_" + ss.str();
                Symbol s(n.retrieve_context().new_symbol(constr_name));
                Type t = orig_s.get_type();
                s.set_type(t);
                ssa_to_original_var[s] = n;
                // 2. Get the value for the constraint
                NBase val = Nodecl::Range::make(Nodecl::Analysis::MinusInfinity::make(t),
                                                Nodecl::Analysis::PlusInfinity::make(t),
                                                const_value_to_nodecl(zero), t);
                // 3. Build the constraint and insert it in the constraints map
                ConstraintBuilderVisitor cbv(_n, *_constraints_map, _constraints, _ordered_constraints);
                Utils::Constraint c = cbv.build_constraint(s, val, t, __GlobalVar);
                (*_constraints_map)[n] = c;
                return;
            }

            // Check for global variables
            ERROR_CONDITION(n.get_symbol().get_scope().is_namespace_scope(),
                    "No constraints found for non-global symbol %s in locus %s."
                    "We should replace the variable with the corresponding constraint.",
                    n.prettyprint().c_str(), n.get_locus_str().c_str());

            // n is a global variable
            return;
        }

        n.replace((*_constraints_map)[n].get_symbol().make_nodecl(/*set_ref_type*/false));
    }
    
    ConstraintBuilderVisitor::ConstraintBuilderVisitor(
            Node* n,
            Utils::VarToConstraintMap input_constraints_map,
            Utils::VarToConstraintMap current_constraints,
            SSAVarToValue_map *constraints,
            NodeclList *ordered_constraints)
        : _n(n), _input_constraints_map(input_constraints_map), _output_constraints_map(current_constraints),
          _output_true_constraints_map(), _output_false_constraints_map(), 
          _constraints(constraints), _ordered_constraints(ordered_constraints),
          _cr(ConstraintReplacement(&_input_constraints_map, n, constraints, ordered_constraints))
    {}
    
    ConstraintBuilderVisitor::ConstraintBuilderVisitor(
            Node* n,
            Utils::VarToConstraintMap input_constraints_map,
            SSAVarToValue_map *constraints,
            NodeclList *ordered_constraints)
        : _n(n), _input_constraints_map(input_constraints_map), _output_constraints_map(), 
          _output_true_constraints_map(), _output_false_constraints_map(), 
          _constraints(constraints), _ordered_constraints(ordered_constraints),
          _cr(ConstraintReplacement(&_input_constraints_map, n, constraints, ordered_constraints))
    {}
    
    Utils::Constraint ConstraintBuilderVisitor::build_constraint(
            const Symbol& s, 
            const NBase& val, 
            const Type& t, 
            ConstraintKind c_kind)
    {
        // Create the constraint
        Utils::Constraint c(s, val);
        
        // Insert the constraint in the global structures that will allow us building the Constraint Graph
        Nodecl::Symbol s_n = s.make_nodecl(/*set_ref_type*/false);
        if(_constraints->find(s_n) == _constraints->end())
            _ordered_constraints->push_back(s_n);
        (*_constraints)[s_n] = val.no_conv();
        
        // Print the constraint in the standard error
        print_constraint(c_kind, s, val, t);
        
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
            // Avoid function pointers
            if (it->get_type().is_pointer()
                    && it->get_type().points_to().is_function())
                continue;

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
            Utils::Constraint c = build_constraint(s, val, t, __Parameter);
            _output_constraints_map[param_s] = c;
        }
    }
    
    void ConstraintBuilderVisitor::set_false_constraint_to_inf(const NBase& n)
    {
        if (n.is<Nodecl::Equal>()
            || n.is<Nodecl::LowerThan>() || n.is<Nodecl::LowerOrEqualThan>()
            || n.is<Nodecl::GreaterThan>() || n.is<Nodecl::GreaterOrEqualThan>())
        {
            // False constraints are not just the negation of the condition,
            // because LHS fulfilling is not enough for the whole condition to fulfill
            const NBase& lhs = n.as<Nodecl::Equal>().get_lhs();
            ERROR_CONDITION(_output_false_constraints_map.find(lhs) == _output_false_constraints_map.end(),
                            "Nodecl %s not found in the set of 'output false constraints' while replacing constraint",
                            lhs.prettyprint().c_str());
            Utils::Constraint old_c_false = _output_false_constraints_map[lhs];
            TL::Symbol old_s = old_c_false.get_symbol();
            const NBase& old_val = old_c_false.get_constraint();
            ERROR_CONDITION(!old_val.is<Nodecl::Analysis::RangeIntersection>(),
                            "Constraint value of a 'false' flow edge has type '%s' when RangeIntersection expected.\n",
                            ast_print_node_type(old_val.get_kind()));
            const NBase& old_val_input_ssa_var = old_val.as<Nodecl::Analysis::RangeIntersection>().get_lhs();
            ERROR_CONDITION(ssa_to_original_var.find(old_val_input_ssa_var.get_symbol()) == ssa_to_original_var.end(),
                            "Constraint value of a 'false' flow edge does not contain ssa variable, but '%s' instead",
                            old_val_input_ssa_var.prettyprint().c_str());
            Type t(Type::get_int_type());
            NBase val_false = Nodecl::Analysis::RangeIntersection::make(
                    old_val_input_ssa_var,
                    Nodecl::Range::make(
                            Nodecl::Analysis::MinusInfinity::make(t),
                            Nodecl::Analysis::PlusInfinity::make(t),
                            const_value_to_nodecl(zero), t),
                    t
            );
            Utils::Constraint new_c_false = build_constraint(
                    old_s, val_false, t, __Replace);
            _output_false_constraints_map[lhs] = new_c_false;
        }
        else if (n.is<Nodecl::LogicalAnd>() || n.is<Nodecl::Different>())
        {}  // Nothing to be done because the infinite range is set to the edge recursively
        else
        {
            internal_error("Unexpected node of type %s while setting false branch to [-inf, +inf]\n",
                           ast_print_node_type(n.get_kind()));
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
            std::string s_str, ConstraintKind c_kind)
    {
        Utils::Constraint c;
        NBase val;

        // 1. Compute the second constraint that corresponds to the current node: x COMP_OP c
        // -->    X1 = X0
        // 1.1 Get a new symbol for the constraint
        std::stringstream ss_tmp; ss_tmp << get_next_id(lhs);
        Symbol s(lhs.retrieve_context().new_symbol(s_str + "_" + ss_tmp.str()));
        s.set_type(t);
        ssa_to_original_var[s] = lhs;

        // 1.2 Build the value of the constraints
        val = _input_constraints_map[lhs].get_symbol().make_nodecl(/*set_ref_type*/false);

        // 1.3 Build the actual constraint and insert it in the corresponding map
        c = build_constraint(s, val, t, c_kind);
        _input_constraints_map[lhs] = c;
        _output_constraints_map[lhs] = c;
        
        return s;
    }

    void ConstraintBuilderVisitor::visit_assignment(const NBase& lhs, const NBase& rhs)
    {
        // Build a symbol for the new constraint based on the name of the original variable
        std::stringstream ss; ss << get_next_id(lhs);
        Symbol orig_s(Utils::get_nodecl_base(lhs).get_symbol());

        std::string subscripts_str;
        if (lhs.no_conv().is<Nodecl::ArraySubscript>())
        {
            const Nodecl::List& subscripts = lhs.no_conv().as<Nodecl::ArraySubscript>().get_subscripts().as<Nodecl::List>();
            for (Nodecl::List::const_iterator it = subscripts.begin(); it != subscripts.end(); ++it)
            {
                if (_input_constraints_map.find(*it) != _input_constraints_map.end())
                    subscripts_str += _input_constraints_map[*it].get_symbol().get_name();
                else    // The subscript is a global variable
                    subscripts_str += it->prettyprint();
                subscripts_str += "_";
            }
        }
        std::string constr_name = orig_s.get_name() + "_" + subscripts_str + ss.str();
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
            val = rhs.shallow_copy();
            _cr.walk(val);
        }
        
        // Build the constraint and insert it in the corresponding maps
        Utils::Constraint c = build_constraint(s, val, t, __BinaryOp);
        _input_constraints_map[lhs] = c;
        _output_constraints_map[lhs] = c;
    }
    
    
    void ConstraintBuilderVisitor::visit_increment(const NBase& rhs, bool positive)
    {
        ERROR_CONDITION(_input_constraints_map.find(rhs) == _input_constraints_map.end(),
                        "Some input constraint required for the increment's RHS %s (%s).\n",
                        rhs.prettyprint().c_str(),
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
        Utils::Constraint c = build_constraint(s, val, t, __UnaryOp);
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

    // x != c;   ---TRUE-->    X1 = X0 ∩ ([-∞, c-1] U [c+1, -∞])
    //           --FALSE-->    X1 = X0 ∩ [c, c]
    void ConstraintBuilderVisitor::visit(const Nodecl::Different& n)
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
        Symbol s = get_condition_node_constraints(lhs, t, orig_s_str, __Comparator);

        // 2.- Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        NBase val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            _cr.walk(val);
        }
        // 2.1.- Compute the constraint that corresponds to the true branch taken from this node
        // x < x;       --TRUE-->       X1 = X0 ∩ [c, c]
        // 2.1.1.- Build the TRUE constraint symbol
        std::stringstream ss_true; ss_true << get_next_id(lhs);
        Symbol s_true(n.retrieve_context().new_symbol(orig_s_str + "_" + ss_true.str()));
        s_true.set_type(t);
        ssa_to_original_var[s_true] = lhs;
        // 2.1.2.- Build the TRUE constraint value
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
        NBase val_true =
            Nodecl::Analysis::RangeIntersection::make(
                s.make_nodecl(/*set_ref_type*/false),
                Nodecl::Analysis::RangeUnion::make(Nodecl::Range::make(Nodecl::Analysis::MinusInfinity::make(t), ub,
                                                                       const_value_to_nodecl(one), t),
                                                   Nodecl::Range::make(lb, Nodecl::Analysis::PlusInfinity::make(t),
                                                                       const_value_to_nodecl(one), t),
                                                   t),
                t);
        // 2.1.3.- Build the TRUE constraint and store it
        Utils::Constraint c_true = build_constraint(s_true, val_true, t, __ComparatorTrue);
        _output_true_constraints_map[lhs] = c_true;
        // 2.2.- Compute the constraint that corresponds to the false branch taken from this node
        // x < c;       --FALSE-->      X1 = X0 ∩ ([-∞, c-1] U [c+1, -∞])
        // 2.2.1.- Build the FALSE constraint symbol
        std::stringstream ss_false; ss_false << get_next_id(lhs);
        Symbol s_false(n.retrieve_context().new_symbol(orig_s_str + "_" + ss_false.str()));
        s_false.set_type(t);
        ssa_to_original_var[s_false] = lhs;
        // 2.2.2.- Build the FALSE constraint value
        NBase val_false =
            Nodecl::Analysis::RangeIntersection::make(
                s.make_nodecl(/*set_ref_type*/false),
                Nodecl::Range::make(val.shallow_copy(),
                                    val.shallow_copy(),
                                    const_value_to_nodecl(one), t),
                t);
        // 2.2.3.- Build the FALSE constraint and store it
        Utils::Constraint c_false = build_constraint(s_false, val_false, t, __ComparatorFalse);
        _output_false_constraints_map[lhs] = c_false;
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
        Symbol s = get_condition_node_constraints(lhs, t, orig_s_str, __Comparator);
        
        // 2.- Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        NBase val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            _cr.walk(val);
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
        Utils::Constraint c_true = build_constraint(s_true, val_true, t, __ComparatorTrue);
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
        Utils::Constraint c_false = build_constraint(s_false, val_false, t, __ComparatorFalse);
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
        Symbol s = get_condition_node_constraints(lhs, t, orig_s_str, __Comparator);
        
        // 2.- Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        NBase val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            _cr.walk(val);
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
        Utils::Constraint c_true = build_constraint(s_true, val_true, t, __ComparatorTrue);
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
        Utils::Constraint c_false = build_constraint(s_false, val_false, t, __ComparatorFalse);
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
        Symbol s = get_condition_node_constraints(lhs, t, orig_s_str, __Comparator);
        
        // 2.- Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        NBase val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            _cr.walk(val);
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
        Utils::Constraint c_true = build_constraint(s_true, val_true, t, __ComparatorTrue);
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
        Utils::Constraint c_false = build_constraint(s_false, val_false, t, __ComparatorFalse);
        _output_false_constraints_map[lhs] = c_false;
    }

    void ConstraintBuilderVisitor::visit(const Nodecl::LogicalAnd& n)
    {
        // 1.- Compute the constraints for the LSH
        const NBase& lhs = n.get_lhs();
        walk(lhs);
        set_false_constraint_to_inf(lhs);

        // 2.- Compute the constraints for the RHS
        const NBase& rhs = n.get_rhs();
        walk(rhs);
        set_false_constraint_to_inf(rhs);
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
        Symbol s = get_condition_node_constraints(lhs, t, orig_s_str, __Comparator);
        
        // 2.- Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        NBase val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            _cr.walk(val);
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
        Utils::Constraint c_true = build_constraint(s_true, val_true, t, __ComparatorTrue);
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
        Utils::Constraint c_false = build_constraint(s_false, val_false, t, __ComparatorFalse);
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
        Symbol s = get_condition_node_constraints(lhs, t, orig_s_str, __Comparator);
        
        // 2.- Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        NBase val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            _cr.walk(val);
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
            Utils::Constraint c_true = build_constraint(s_true, val_true, t, __ComparatorTrue);
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
            Utils::Constraint c_false = build_constraint(s_false, val_false, t, __ComparatorFalse);
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
        Symbol s = get_condition_node_constraints(lhs, t, orig_s_str, __Mod);
        
        // 2.- Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        NBase val = rhs.shallow_copy();
        if(!rhs.is_constant())
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            _cr.walk(val);
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
        Utils::Constraint c_true = build_constraint(s_true, val_true, t, __ModTrue);
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
        Utils::Constraint c_false = build_constraint(s_false, val_false, t, __ModFalse);
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
            node = new CGNode(__Sym, value);
            _nodes[value] = node;
        }
        return node;
    }
    
    CGNode* ConstraintGraph::insert_node(CGOpType type)
    {
        CGNode* node = new CGNode(type, NBase::null());
        NBase value = Nodecl::IntegerLiteral::make(Type::get_int_type(), 
                                                   const_value_get_integer(node->get_id(), /*num_bytes*/4, /*sign*/1));
        _nodes[value] = node;
        return node;
    }
    
    void ConstraintGraph::connect_nodes(CGNode* source, CGNode* target,
            CGOpType edge_type, NBase predicate, bool is_back_edge)
    {
        ObjectList<CGNode*> children = source->get_children();
        if(!children.contains(target))
        {
            CGEdge* e = source->add_child(target, edge_type, predicate, is_back_edge);
            target->add_entry(e);
        }
    }

    static CGOpType get_op_type_from_value(const NBase& val)
    {
        switch(val.get_kind())
        {
            case NODECL_ADD:                            return __Add;
            case NODECL_DIV:                            return __Div;
            case NODECL_ANALYSIS_RANGE_INTERSECTION:    return __Intersection;
            case NODECL_MUL:                            return __Mul;
            case NODECL_ANALYSIS_PHI:                   return __Phi;
            case NODECL_MINUS:                          return __Sub;
            case NODECL_SYMBOL:                         return __Sym;
            default:
                internal_error("Unexpected Constraint value %s.\n", val.prettyprint().c_str());
        }
    }

    CGNode* ConstraintGraph::fill_cg_with_binary_op_rec(
            const NBase& val,
            CGOpType n_type)
    {
        ERROR_CONDITION(!Nodecl::Utils::nodecl_is_arithmetic_op(val),
                        "Expected arithmetic operation in constraint, but found '%s'.\n",
                        val.prettyprint().c_str());

        // We take the liberty of casting to Nodecl::Add always because
        // all binary operation structurally have the same tree
        const NBase& lhs = val.as<Nodecl::Add>().get_lhs().no_conv();
        const NBase& rhs = val.as<Nodecl::Add>().get_rhs().no_conv();
        CGOpType val_type = get_op_type_from_value(val);

        CGNode* target_op;
        if(lhs.is<Nodecl::Symbol>())
        {
            if(rhs.is<Nodecl::Symbol>())
            {   // var1 OP var2
                CGNode* source1 = insert_node(lhs);                 // var1
                CGNode* source2 = insert_node(rhs);                 // var2
                target_op = insert_node(val_type);                  // OP

                connect_nodes(source1, target_op);                  // var1 -> OP
                connect_nodes(source2, target_op);                  // var2 -> OP
            }
            else if(rhs.is_constant())
            {   // var OP const
                CGNode* source = insert_node(lhs);                          // var
                target_op = insert_node(n_type);                            // outerOP
                NBase predicate = rhs.shallow_copy();                       // const
                Optimizations::ReduceExpressionVisitor rev;
                rev.walk(predicate);
                connect_nodes(source, target_op, val_type, predicate);      // var --val_type--> outerOP
            }
            else if (Nodecl::Utils::nodecl_is_arithmetic_op(rhs))
            {   // var OP (...)
                CGNode* source1 = insert_node(lhs);                         // var
                CGNode* source2 = fill_cg_with_binary_op_rec(rhs, val_type);// (...)
                target_op = insert_node(val_type);                          // OP

                connect_nodes(source1, target_op);                         // var -> OP
                connect_nodes(source2, target_op);                         // (...) -> OP
            }
            else
            {
                internal_error("Unexpected value '%s' for a Constraint.\n", val.prettyprint().c_str());
            }
        }
        else if (lhs.is_constant())
        {
            if (rhs.is<Nodecl::Symbol>())
            {   // const OP var
                CGNode* source = insert_node(rhs);                  // var
                target_op = insert_node(n_type);                    // outerOP
                connect_nodes(source, target_op);                   // var -> outerOP
            }
            else if (rhs.is_constant())
            {
                // TODO
            }
            else if (Nodecl::Utils::nodecl_is_arithmetic_op(rhs))
            {   // const OP (...)
                CGNode* source = fill_cg_with_binary_op_rec(rhs, val_type); // (...)
                target_op = insert_node(n_type);                            // OP

                NBase predicate = lhs.shallow_copy();                       // const
                Optimizations::ReduceExpressionVisitor rev;
                rev.walk(predicate);
                connect_nodes(source, target_op, val_type, predicate);      // (...) --const--> OP
            }
            else
            {
                internal_error("Unexpected value '%s' for a Constraint.\n", val.prettyprint().c_str());
            }
        }
        else if (Nodecl::Utils::nodecl_is_arithmetic_op(lhs))
        {
            CGNode* source1 = fill_cg_with_binary_op_rec(lhs, val_type);
            if (rhs.is<Nodecl::Symbol>())
            {   // (...) OP var
                CGNode* source2 = insert_node(rhs);                 // var
                target_op = insert_node(val_type);                  // OP
                connect_nodes(source1, target_op);                  // var -> OP
                connect_nodes(source2, target_op);                  // var -> OP
            }
            else if (rhs.is_constant())
            {   // var OP const
                target_op = insert_node(n_type);                                // outerOP
                NBase predicate = rhs.shallow_copy();                           // const
                Optimizations::ReduceExpressionVisitor rev;
                rev.walk(predicate);
                connect_nodes(source1, target_op, val_type, predicate);         // var --val_type--> outerOP
            }
            else if (Nodecl::Utils::nodecl_is_arithmetic_op(rhs))
            {   // (.x.) OP (.y.)
                CGNode* source2 = fill_cg_with_binary_op_rec(rhs, val_type);    // (.y.)
                target_op = insert_node(val_type);                              // OP
                connect_nodes(source1, target_op);                              // (.x.) -> OP
                connect_nodes(source2, target_op);                              // (.y.) -> OP
            }
            else
            {
                internal_error("Unexpected value '%s' for a Constraint.\n", val.prettyprint().c_str());
            }
        }
        else
        {
            internal_error("Unexpected value '%s' for a Constraint.\n", val.prettyprint().c_str());
        }

        return target_op;
    }

    void ConstraintGraph::fill_cg_with_binary_op(
            const NBase& s,
            const NBase& val,
            CGOpType op_type)
    {
        // res = (...) OP (...)
        CGNode* target_op = fill_cg_with_binary_op_rec(val, op_type);   // OP
        CGNode* target = insert_node(s);                                // res
        connect_nodes(target_op, target);                               // OP -> res
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
        for (NodeclList::const_iterator oit = ordered_constraints.begin(); oit != ordered_constraints.end(); ++oit)
        {
            std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less>::const_iterator it = constraints.find(*oit);
            ERROR_CONDITION(it == constraints.end(), 
                            "Constraint %s not found in the constraints map.\n", 
                            oit->prettyprint().c_str());
            const NBase& s = it->first;
            NBase val = it->second;

            // Insert in the CG the edges corresponding to the current Constraint
            if (val.is<Nodecl::Symbol>())
            {   // H. 
                CGNode* source = insert_node(val);
                CGNode* target = insert_node(s);  // A.
                connect_nodes(source, target);
            }
            else if (val.is<Nodecl::Analysis::Phi>())
            {   // D.
                // Create the target
                CGNode* target_phi = insert_node(__Phi);
                // Create the sources
                std::queue<CGNode*> sources;
                Nodecl::List expressions = val.as<Nodecl::Analysis::Phi>().get_expressions().as<Nodecl::List>();
                for (Nodecl::List::iterator ite = expressions.begin(); ite != expressions.end(); ++ite)
                {
                    NBase e = *ite;
                    if (_nodes.find(e) == _nodes.end())
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
                while (!sources.empty())
                {
                    CGNode* source = sources.front();
                    sources.pop();
                    connect_nodes(source, target_phi);
                }
                connect_nodes(target_phi, target);
            }
            else if (val.is<Nodecl::Analysis::RangeIntersection>())
            {   // E.
                const NBase& lhs = val.as<Nodecl::Analysis::RangeIntersection>().get_lhs().no_conv();
                const NBase& rhs = val.as<Nodecl::Analysis::RangeIntersection>().get_rhs().no_conv();
                
                CGNode* source = insert_node(lhs);
                CGNode* target = insert_node(s);  // A.
                NBase predicate = rhs.shallow_copy();
                Optimizations::ReduceExpressionVisitor rev;
                rev.walk(predicate);
                connect_nodes(source, target, __Intersection, predicate);
            }
            else if (val.is<Nodecl::Range>())
            {
                // B. Create a new node if the Constraint Value is a Range
                CGNode* source = insert_node(val);
                CGNode* target = insert_node(s);  // A.
                // F. Create edge between the Range node and the Constraint node
                connect_nodes(source, target);
            }
            else if (Nodecl::Utils::nodecl_is_arithmetic_op(val))
            {
                CGOpType type = get_op_type_from_value(val);
                fill_cg_with_binary_op(s, val, type);
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
            connect_nodes(source, target, __Flow, NBase::null(), /*is_back_edge*/true);
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
            CGOpType node_t = n->get_type();
            // 1.1.- The node has a constraint associated
            if(node_t == __Sym)
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
                dot_cg << "\t" << source << " [label=\"[" << source << "] " << n->get_type_as_string() << "\"];\n";
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
        
        // 1.- Collect each set of nodes that form a SCC
        std::map<CGNode*, int> scc_lowlink_index;
        std::map<CGNode*, int> scc_index;
        for(CGValueToCGNode_map::iterator it = _nodes.begin(); it != _nodes.end(); ++it)
        {
            CGNode* n = it->second;
            if((scc_index.find(n) == scc_index.end()) || (scc_index[n] == -1))
                strong_connect(n, scc_current_index, s, scc_list, scc_lowlink_index, scc_index);
        }

        // 2.- Compute the directionality of each scc_current_index
        // 3.- Create a map between the Constraint Graph nodes and their SCC
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
    
    void ConstraintGraph::evaluate_cgnode(CGNode* const node, bool &changes)
    {
        const NBase& old_valuation = node->get_valuation();
        NBase valuation;
        CGOpType type = node->get_type();
        NBase constraint = node->get_constraint();
        if (!constraint.is_null() && constraint.is<Nodecl::Range>())
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
            for (ObjectList<CGEdge*>::iterator it = entries.begin(); it != entries.end(); ++it)
            {
                if ((*it)->is_back_edge())
                    continue;
                Nodecl::Range last_valuation = (*it)->get_source()->get_valuation().as<Nodecl::Range>();
                CGOpType edge_type = (*it)->get_edge_type();
                NBase predicate = (*it)->get_predicate();
                if (edge_type == __Flow)
                    valuation = last_valuation;
                else if (edge_type == __Add)
                    valuation = Utils::range_value_add(last_valuation, predicate);
                else if (edge_type == __Sub)
                    valuation = Utils::range_value_sub(last_valuation, predicate);
                else if (edge_type == __Mul)
                    valuation = Utils::range_value_mul(last_valuation, predicate);
                else if (edge_type == __Div)
                    valuation = Utils::range_value_div(last_valuation, predicate);
                else if(edge_type == __Intersection)
                {
                    // Check whether the cycle is positive or negative
                    SCC* scc = _node_to_scc_map[node];
                    // FIXME First approach: cyle directionality could be unique and compute only once for each SCC
                    Utils::CycleDirection cycle_direction = scc->get_cycle_direction(*it);
                    valuation = Utils::range_intersection(last_valuation, predicate, cycle_direction);
                }
                else
                {
                    internal_error("Unexpected CG edge %s with predicate %s.\n",
                                   (*it)->get_type_as_string().c_str(),
                                   ast_print_node_type(predicate.get_kind()),
                                   predicate.prettyprint().c_str());
                }

                if (!valuation.is_null())
                    entry_valuations.append(valuation);
            }

            if(type == __Sym)
            {
                ERROR_CONDITION(entry_valuations.size()>1, 
                                "Only one entry valuation expected for a Sym CGNode but %d found.\n", 
                                entry_valuations.size());
                valuation = *entry_valuations.begin();
            }
            else
            {
                if (type == __Phi)
                    valuation = join_valuations(Utils::range_union, entry_valuations);          // RANGE_UNION
                else if (type == __Add)
                    valuation = join_valuations(Utils::range_addition, entry_valuations);       // RANGE_ADD
                else if (type == __Sub)
                    valuation = join_valuations(Utils::range_subtraction, entry_valuations);    // RANGE_SUB
                else if (type == __Mul)
                    valuation = join_valuations(Utils::range_division, entry_valuations);       // RANGE_MUL
                else if (type == __Div)
                    valuation = join_valuations(Utils::range_multiplication, entry_valuations); // RANGE_DIV
            }
        }
#ifdef RANGES_DEBUG
        std::cerr << "    EVALUATE " << node->get_id() << "  ::  " << valuation.prettyprint() << std::endl;
#endif

        if (Nodecl::Utils::structurally_equal_nodecls(old_valuation, valuation, /*skip_conversions*/ true))
        {
            changes = changes || false;
        }
        else
        {
            changes = true;
            node->set_valuation(valuation);
        }
    }
    
    void ConstraintGraph::resolve_cycle(SCC* scc)
    {
        // Evaluate the root of the SCC
        CGNode* root = scc->get_root();
        bool changes = true;
        evaluate_cgnode(root, changes);
        
        // Saturate all non-saturated edges in the SCC
        // Propagate the rest of edges
        std::queue<CGNode*> next_nodes;
        const ObjectList<CGNode*>& root_children = root->get_children();
        std::set<CGNode*> visited;
        ObjectList<CGEdge*> back_edges;
        while (changes)
        {
            // Prepare the next traversal
            changes = false;
            for (ObjectList<CGNode*>::const_iterator it = root_children.begin();
                 it != root_children.end(); ++it)
                 next_nodes.push(*it);
            visited.clear();
            back_edges.clear();

            while (!next_nodes.empty())
            {
                // Get the next node to be treated
                CGNode* n = next_nodes.front();
                next_nodes.pop();

                // If the node is not in the same SCC, then we will treat it later
                if (_node_to_scc_map[n] != scc)
                    continue;

                // If the node was already treated, there is nothing to be done (in this iteration)
                if (visited.find(n) != visited.end())
                    continue;

                // Treat the node and insert it in the list of treated nodes
                evaluate_cgnode(n, changes);
                visited.insert(n);

                // Prepare following iterations
                const ObjectList<CGEdge*>& exits = n->get_exits();
                for (ObjectList<CGEdge*>::const_iterator it = exits.begin(); it != exits.end(); ++it)
                {
                    CGNode* tgt = (*it)->get_target();
                    if ((*it)->is_back_edge())
                        back_edges.append(*it);
                    else if (visited.find(tgt) == visited.end())
                        next_nodes.push(tgt);
                }
            }

            // Propagate back edges valuations
            for (ObjectList<CGEdge*>::iterator it = back_edges.begin(); it != back_edges.end(); ++it)
            {
                NBase predicate = (*it)->get_predicate();
                ERROR_CONDITION(!predicate.is_null(),
                                "Propagation in back edges with a predicate is not yet implemented.\n", 0);
                NBase source_valuation = (*it)->get_source()->get_valuation();
                if (!source_valuation.is_null())
                {
                    CGNode* target = (*it)->get_target();
                    NBase target_valuation = target->get_valuation();
                    const NBase& valuation = (target_valuation.is_null() ? source_valuation
                                                                         : Utils::range_union(source_valuation, target_valuation));
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
                bool changes;   // Unnecessary when calling evaluate_cgnode from here
                evaluate_cgnode(scc->get_nodes()[0], changes);
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
        _cg->print_graph();

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
        std::set<Node*> treated;
        compute_constraints_rec(entry, constr_map, propagated_constr_map, treated);
        
#ifdef RANGES_DEBUG
        print_constraints();
#endif
    }
    
    void RangeAnalysis::build_constraint_graph()
    {
        _cg->fill_constraint_graph(_constraints, _ordered_constraints);
    }
    
    // Set an constraint to the graph entry node for each parameter of the function
    void RangeAnalysis::compute_parameters_constraints(
            std::map<Node*, Utils::VarToConstraintMap>& constr_map)
    {
        Symbol func_sym = _pcfg->get_function_symbol();
        if(!func_sym.is_valid())    // The PCFG have been built for something other than a FunctionCode
            return;

        Node* entry = _pcfg->get_graph()->get_graph_entry_node();
        const ObjectList<Symbol>& params = func_sym.get_function_parameters();
        ConstraintBuilderVisitor cbv(
                entry,
                /*unnecessary for parameters*/constr_map[entry],
                &_constraints, &_ordered_constraints);
        cbv.compute_parameters_constraints(params);
        constr_map[entry] = cbv.get_output_constraints_map();
    }

namespace {

    void create_recomputed_constraint(
            const Utils::VarToConstraintMap& new_constrs,
            Utils::VarToConstraintMap& constrs,
            SSAVarToValue_map *constraints,
            NodeclList *ordered_constraints,
            ConstraintBuilderVisitor& cbv)
    {
        // Example:
        //     we had:      i1 = i0
        //     we have:     i3 = phi(i1,i2)
        //     but we want: i3 = i0
        //                  i1 = phi(i3,i2)
        for (Utils::VarToConstraintMap::const_iterator it = new_constrs.begin();
             it != new_constrs.end(); ++it)
        {
            const NBase& orig_var = it->first;
            if ((constrs.find(orig_var) != constrs.end())
                    && (constrs[orig_var] != it->second))
            {
                // 1.- Get a new symbol for the new constraint
                std::stringstream ss; ss << get_next_id(orig_var);
                Symbol orig_sym(orig_var.get_symbol());
                std::string constr_name = orig_sym.get_name() + "_" + ss.str();
                Symbol ssa_var(orig_var.retrieve_context().new_symbol(constr_name));
                Type t(orig_sym.get_type());
                ssa_var.set_type(t);
                ssa_to_original_var[ssa_var] = orig_var;
                // 2.- Rebuild the old constraint               (i1 = i0   =>   i3 = i0)
                Utils::Constraint& old_c = constrs[orig_var];
                Symbol old_ssa_var = old_c.get_symbol();
                NBase old_ssa_nodecl = old_ssa_var.make_nodecl(/*set_ref_type*/false);
                NBase new_ssa_nodecl = ssa_var.make_nodecl(/*set_ref_type*/false);
                NBase old_val = old_c.get_constraint();
                (*constraints)[new_ssa_nodecl] = old_val;
                // Look for the position to insert the new constraint
                NodeclList::iterator ito = ordered_constraints->begin();
                while (!Nodecl::Utils::structurally_equal_nodecls(*ito, old_ssa_nodecl)
                        && ito != ordered_constraints->end())
                    ++ito;
                ERROR_CONDITION(ito == ordered_constraints->end(),
                                "SSA variable %s not found in the list of ordered constraints\n",
                                new_ssa_nodecl.prettyprint().c_str());
                ordered_constraints->std::vector<NBase>::insert(ito, new_ssa_nodecl);
                // 3.- Build the value of the new constraint    (i1 = phi(i3,i2))
                NBase e1 = new_ssa_nodecl;
                NBase e2 = it->second.get_symbol().make_nodecl(/*set_ref_type*/false);
                Nodecl::List exprs = Nodecl::List::make(e1, e2);
                NBase val = Nodecl::Analysis::Phi::make(exprs, t);
                // 3.- Build the new constraint and insert it in the proper list
                Utils::Constraint new_c = cbv.build_constraint(old_ssa_var, val, t, __BackEdge);
                constrs[orig_var] = new_c;
            }
        }
    }
    
    void compute_constraint_from_back_edge(
            Node* n,
            const Utils::VarToConstraintMap& new_constraint_map,
            const Utils::VarToConstraintMap& new_propagated_constraint_map,
            SSAVarToValue_map *constraints,
            NodeclList *ordered_constraints,
            std::map<Node*, Utils::VarToConstraintMap>& constr_map,
            std::map<Node*, Utils::VarToConstraintMap>& propagated_constr_map)
    {
        Utils::VarToConstraintMap& constrs = constr_map[n];
        ConstraintBuilderVisitor cbv(n, constrs, constraints, ordered_constraints);

        // 1.- constr_map must contain the combination of both new_constraint_map and new_propagated_constraint_map
        Utils::VarToConstraintMap all_new_constraint_map = new_constraint_map;
        all_new_constraint_map.insert(new_propagated_constraint_map.begin(), new_propagated_constraint_map.end());
        create_recomputed_constraint(all_new_constraint_map, constrs, constraints, ordered_constraints, cbv);

        // 2.- propagated_constr_map must contain only new_propagated_constraint_map
        Utils::VarToConstraintMap& propagated_constrs = propagated_constr_map[n];
        create_recomputed_constraint(new_propagated_constraint_map, propagated_constrs, constraints, ordered_constraints, cbv);
    }
}

    // This is a breadth-first search because for a given node we need all its parents 
    void RangeAnalysis::compute_constraints_rec(
            Node* entry, 
            std::map<Node*, Utils::VarToConstraintMap>& constr_map, 
            std::map<Node*, Utils::VarToConstraintMap>& propagated_constr_map,
            std::set<Node*>& treated)
    {
        ERROR_CONDITION(!entry->is_entry_node(), 
                        "Expected ENTRY node but found %s node.", entry->get_type_as_string().c_str());

        std::queue<Node*> worklist;
        worklist.push(entry);
        while (!worklist.empty())
        {
            Node* n = worklist.front();
            worklist.pop();

            if (treated.find(n) != treated.end())
                continue;

            // 1.- Check whether all n parents (coming from non-back-edges) are already computed
            const ObjectList<Edge*>& entries = n->get_entry_edges();
            bool ready = true;
            for (ObjectList<Edge*>::const_iterator it = entries.begin(); it != entries.end() && ready; ++it)
            {
                if (!(*it)->is_back_edge()                                  // *it is a dominator of n
                        && treated.find((*it)->get_source())==treated.end() // *it is not yet visited
                        && !(*it)->get_source()->is_omp_task_node())        // *it is not a task node
                {
                    ready = false;
                }
            }
            if (!ready)
            {
                worklist.push(n);
                continue;
            }

            // 2.- The element is ready to be computed
            if (n->is_graph_node())
            {   // 2.1.- For graph nodes, call recursively to compute inner nodes constraints
                // 2.1.1.- Recursive call with the graph entry node
                compute_constraints_rec(n->get_graph_entry_node(), constr_map, propagated_constr_map, treated);
                // 2.1.2.- Propagate the information form the exit node to the graph node
                Node* graph_exit = n->get_graph_exit_node();
                Utils::VarToConstraintMap exit_constrs = constr_map[graph_exit];
                propagated_constr_map[n] = propagated_constr_map[graph_exit];
                propagated_constr_map[n].insert(exit_constrs.begin(), exit_constrs.end());
            }
            else
            {
                // 2.2.- For the rest of nodes,
                //       - merge information from parents
                //       - compute current node information
                //       - merge parents' info with current node's info

                // 2.2.1.- Collect and join constraints computed for all the parents
                Utils::VarToConstraintMap input_constrs;            // Constraint coming directly from parents
                Utils::VarToConstraintMap new_input_constrs;        // Constraints resulting from the join of parents' constraints
                ConstraintBuilderVisitor cbv_propagated(n, input_constrs, &_constraints, &_ordered_constraints);

                const ObjectList<Node*>& parents = (n->is_entry_node() ? n->get_outer_node()->get_parents()
                                                                       : n->get_parents());
                NodeclSet treated_omp_private_vars;
                for (ObjectList<Node*>::const_iterator itp = parents.begin(); itp != parents.end(); ++itp)
                {
                    Utils::VarToConstraintMap itp_all_constrs = constr_map[*itp];
                    Utils::VarToConstraintMap itp_propagated_constrs = propagated_constr_map[*itp];
                    // We use the 'insert' method because when a constraint is already in the 'constr_map', we
                    // do not take into account the constraints being propagated from the parents
                    itp_all_constrs.insert(itp_propagated_constrs.begin(), itp_propagated_constrs.end());
                    for (Utils::VarToConstraintMap::iterator itc = itp_all_constrs.begin();
                        itc != itp_all_constrs.end(); ++itc)
                    {
                        const NBase& orig_var = itc->first;
                        // 2.2.1.1.- Treat omp nodes depending on the data-sharing of the variables
                        if (n->is_entry_node() && n->get_outer_node()->is_omp_task_node())
                        {
                            // If the variable is:
                            //    - firstprivate -> the only parent we have to take into account is the task creation node
                            //    - private      -> no propagation exists and the range of the variable is [-inf, +inf]
                            //    - shared       -> take into account all parents, and any possible modification done in the concurrent code
                            Node* task_node = n->get_outer_node();
                            const NodeclSet& fp_vars = task_node->get_firstprivate_vars();
                            const NodeclSet& p_vars = task_node->get_private_vars();
                            if (Utils::nodecl_set_contains_nodecl(orig_var, fp_vars))
                            {
                                if (!(*itp)->is_omp_task_creation_node())
                                    continue;   // Skip this parent because its values for #orig_var must not be propagated
                            }
                            else if (Utils::nodecl_set_contains_nodecl(orig_var, p_vars))
                            {
                                // Avoid propagating the range [-inf, +inf] for the same variable multiple times
                                if (Utils::nodecl_set_contains_nodecl(orig_var, treated_omp_private_vars))
                                    continue;
                                treated_omp_private_vars.insert(orig_var);
                                // Build a new constraint with value [-inf, +inf] because
                                // the initialization value of this variable is undefined
                                // 2.2.1.1.1.- Get a new symbol for the new constraint
                                std::stringstream ss; ss << get_next_id(orig_var);
                                Symbol orig_sym(orig_var.get_symbol());
                                std::string constr_name = orig_sym.get_name() + "_" + ss.str();
                                Symbol ssa_var(orig_var.retrieve_context().new_symbol(constr_name));
                                Type t(orig_sym.get_type());
                                ssa_var.set_type(t);
                                ssa_to_original_var[ssa_var] = orig_var;
                                // 2.2.1.1.2.- Build the value of the new constraint
                                NBase new_constraint_val = Nodecl::Range::make(Nodecl::Analysis::MinusInfinity::make(t),
                                                                               Nodecl::Analysis::PlusInfinity::make(t),
                                                                               const_value_to_nodecl(zero), t);
                                // 2.2.1.1.3.- Remove the old constraint from the input_constrs
                                //         If it was in the new_input_constrs map, it will be deleted with the insertion
                                if (input_constrs.find(orig_var) != input_constrs.end())
                                    input_constrs.erase(orig_var);
                                // 2.2.1.1.4.- Build the current constraint and insert it in the proper list
                                Utils::Constraint new_c = cbv_propagated.build_constraint(ssa_var, new_constraint_val, t, __Propagated);
                                new_input_constrs[orig_var] = new_c;
                            }
                            else
                            {   // FIXME
                                // The variable is shared: much more values could be propagated here (from all concurrent definitions)
                                // But, as an initial approach, we propagate the values of all parents
                                // so we do nothing special here, as it was a non-OpenMP node
                            }
                        }
                        // 2.2.1.2.- Treat non-OpenMP nodes
                        const Utils::Constraint& c = itc->second;
                        if (input_constrs.find(orig_var) == input_constrs.end() &&
                            new_input_constrs.find(orig_var) == new_input_constrs.end())
                        {   // No constraints already found for variable orig_var
                            input_constrs[orig_var] = c;
                        }
                        else
                        {   // Constraints for variable orig_var already found: merge them with the new constraint
                            // 2.2.1.2.1.- Get the existing constraint
                            Utils::Constraint old_c =
                                    ((input_constrs.find(orig_var) != input_constrs.end()) ? input_constrs[orig_var]
                                                                                           : new_input_constrs[orig_var]);
                            NBase old_c_val = old_c.get_constraint();

                            // 2.2.1.2.2.- If the new constraint is different from the old one, compute the combination of both
                            NBase c_nodecl = c.get_constraint();
                            if (!Nodecl::Utils::structurally_equal_nodecls(old_c_val, c_nodecl,
                                /*skip_conversion_nodes*/true))
                            {
                                // 2.2.1.2.2.1.- Get a new symbol for the new constraint
                                std::stringstream ss; ss << get_next_id(orig_var);
                                Symbol orig_sym(orig_var.get_symbol());
                                std::string constr_name = orig_sym.get_name() + "_" + ss.str();
                                Symbol ssa_var(orig_var.retrieve_context().new_symbol(constr_name));
                                Type t(orig_sym.get_type());
                                ssa_var.set_type(t);
                                ssa_to_original_var[ssa_var] = orig_var;

                                // 2.2.1.2.2.2.- Build the value of the new constraint
                                NBase new_constraint_val;
                                if (old_c_val.is<Nodecl::Analysis::Phi>())
                                {   // Attach a new element to the list inside the node Phi
                                    Nodecl::List expressions = old_c_val.as<Nodecl::Analysis::Phi>().get_expressions().as<Nodecl::List>();
                                    Nodecl::Symbol new_expr = c.get_symbol().make_nodecl(/*set_ref_type*/false);
                                    expressions.append(new_expr);
                                    new_constraint_val = Nodecl::Analysis::Phi::make(expressions, orig_var.get_type());
                                }
                                else
                                {   // Create a new node Phi with the combination of the old constraint and the new one
                                    Nodecl::Symbol tmp1 = old_c.get_symbol().make_nodecl(/*set_ref_type*/false);
                                    Nodecl::Symbol tmp2 = c.get_symbol().make_nodecl(/*set_ref_type*/false);
                                    Nodecl::List expressions = Nodecl::List::make(tmp1, tmp2);
                                    new_constraint_val = Nodecl::Analysis::Phi::make(expressions, c_nodecl.get_type());
                                }

                                // 2.2.1.2.2.3.- Remove the old constraint from the input_constrs
                                //         If it was in the new_input_constrs map, it will be deleted with the insertion
                                if (input_constrs.find(orig_var) != input_constrs.end())
                                    input_constrs.erase(orig_var);
                                // 2.2.1.2.2.4.- Build the current constraint and insert it in the proper list
                                Utils::Constraint new_c = cbv_propagated.build_constraint(ssa_var, new_constraint_val, t, __Propagated);
                                new_input_constrs[orig_var] = new_c;
                            }
                        }
                    }
                }

                // 3.- Propagate constraints from parent nodes to the current node
                constr_map[n].insert(new_input_constrs.begin(), new_input_constrs.end());

                // 4.- Compute the constraints generated in the current node
                if (n->has_statements())
                {
                    // 4.1.- Compute the constraints of the current node
                    // Note: take into account the constraints the node may already have (if it is the TRUE or FALSE child of a conditional)
                    Utils::VarToConstraintMap& current_constraints_map = constr_map[n];
                    ConstraintBuilderVisitor cbv(n, input_constrs, current_constraints_map,
                                                &_constraints, &_ordered_constraints);
                    NodeclList stmts = n->get_statements();
                    for (NodeclList::iterator itt = stmts.begin(); itt != stmts.end(); ++itt)
                        cbv.compute_stmt_constraints(*itt);

                    Utils::VarToConstraintMap output_constrs = cbv.get_output_constraints_map();
                    current_constraints_map.insert(output_constrs.begin(), output_constrs.end());

                    // 4.2.- Set true/false output constraints to current children, if applies
                    ObjectList<Edge*> exits = n->get_exit_edges();
                    if (exits.size()==2 &&
                        ((exits[0]->is_true_edge() && exits[1]->is_false_edge()) || (exits[1]->is_true_edge() && exits[0]->is_false_edge())))
                    {
                        Utils::VarToConstraintMap out_true_constrs = cbv.get_output_true_constraints_map();
                        Utils::VarToConstraintMap out_false_constrs = cbv.get_output_false_constraints_map();

                        // 4.2.1.- We always propagate to the TRUE edge
                        Node* true_node = (exits[0]->is_true_edge() ? exits[0]->get_target() : exits[1]->get_target());
                        Node* real_true_node = true_node;
                        while (true_node->is_exit_node())
                            true_node = true_node->get_outer_node()->get_children()[0];
                        if (true_node->is_graph_node())
                            true_node = true_node->get_graph_entry_node();
                        constr_map[true_node].insert(out_true_constrs.begin(), out_true_constrs.end());

                        // 4.2.2.- For the if_else cases, we only propagate to the FALSE edge when it contains statements ('else' statements)
                        Node* false_node = (exits[0]->is_true_edge() ? exits[1]->get_target() : exits[0]->get_target());
                        ObjectList<Node*> real_true_node_children = real_true_node->get_children();
                        if ((false_node->get_entry_edges().size() == 1) || !real_true_node_children.contains(false_node))
                        {   // If the true_node is a parent of the false_node, then there are no statements
                            // Avoid cases where the FALSE edge leads to the end of the graph
                            ObjectList<Node*> children;
                            while (false_node->is_exit_node())
                            {
                                children = false_node->get_outer_node()->get_children();
                                if (!children.empty())
                                    false_node = children[0];
                                else
                                {
                                    false_node = NULL;
                                    break;
                                }
                            }
                            if (false_node!=NULL)
                            {
                                if (false_node->is_graph_node())
                                    false_node = false_node->get_graph_entry_node();
                                constr_map[false_node].insert(out_false_constrs.begin(), out_false_constrs.end());
                            }
                        }
                    }
                }

                // 5.- Purge propagated constraints:
                // When the node generates a constraint for a given variable
                // any propagated constraint from parents for that variable is deleted here
                for (Utils::VarToConstraintMap::iterator itt = constr_map[n].begin();
                    itt != constr_map[n].end(); ++itt)
                {
                    Utils::VarToConstraintMap::iterator ittt = input_constrs.find(itt->first);
                    if (ittt != input_constrs.end())
                        input_constrs.erase(ittt);
                }
                propagated_constr_map[n] = input_constrs;
            }
            treated.insert(n);

            if (!n->is_omp_task_node())
            {
                const ObjectList<Edge*>& exits = n->get_exit_edges();
                for (ObjectList<Edge*>::const_iterator it = exits.begin(); it != exits.end(); ++it)
                {
                    Node* t = (*it)->get_target();
                    if ((*it)->is_back_edge())
                    {   // Propagate here constraints from the back edge
                        compute_constraint_from_back_edge(
                                t, constr_map[n], propagated_constr_map[n],
                                &_constraints, &_ordered_constraints,
                                constr_map, propagated_constr_map);
                    }

                    if (treated.find(t) == treated.end())
                        worklist.push(t);
                }
            }
        }
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
