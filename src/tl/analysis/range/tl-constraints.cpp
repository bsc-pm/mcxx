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

#include <limits.h>

#include "tl-range-analysis.hpp"

namespace TL {
namespace Analysis {

namespace {
    // ************************************************************************************** //
    // ***************** initialize global variables for ranges operations ****************** //

    const_value_t* zero = const_value_get_zero(/*num_bytes*/ 4, /*signed*/ 1);
    const_value_t* one = const_value_get_one(/*num_bytes*/ 4, /*signed*/ 1);
    const_value_t* minus_one = const_value_get_minus_one(/*num_bytes*/ 4, /*sign*/1);
    const_value_t* long_max = const_value_get_integer(LONG_MAX, /*num_bytes*/sizeof(long), /*sign*/1);
    NBase plus_inf = Nodecl::Analysis::PlusInfinity::make(Type::get_long_int_type(), long_max);
    const_value_t* long_min = const_value_get_integer(LONG_MIN, /*num_bytes*/sizeof(long), /*sign*/1);
    NBase minus_inf = Nodecl::Analysis::MinusInfinity::make(Type::get_long_int_type(), long_min);
    
    // ************************************************************************************** //
}

    // ***************************************************************************** //
    // ************ Class replacing original variables with ssa symbols ************ //

    ConstraintReplacement::ConstraintReplacement(
            VarToConstraintMap* input_constraints,
            Constraints *constraints,
            std::vector<Symbol> *ordered_constraints)
        : _input_constraints(input_constraints),
          _constraints(constraints),
          _ordered_constraints(ordered_constraints) // Attributes needed to create new constraints
    {}

    void ConstraintReplacement::visit(const Nodecl::ArraySubscript& n)
    {
        if (_input_constraints->find(n) != _input_constraints->end())
            n.replace((*_input_constraints)[n].get_symbol().make_nodecl(/*set_ref_type*/false));
        else
        {
            // 1. Build a symbol for the new constraint based on the name of the original variable
            std::stringstream ss; ss << get_next_id(n);
            std::string array_subscript_str = get_array_subscript_string(n, *_input_constraints);
            std::string ssa_name = array_subscript_str + ss.str();
            Symbol ssa_sym(ssa_scope.new_symbol(ssa_name));
            Type t = Utils::get_nodecl_base(n).get_symbol().get_type();
            ssa_sym.set_type(t);
            ssa_to_original_var[ssa_sym] = n;

            // 2. Build the value for the constraint
            NBase val = Nodecl::Range::make(minus_inf.shallow_copy(),
                                            plus_inf.shallow_copy(),
                                            const_value_to_nodecl(zero), t);

            // 3. Build the constraint and insert it in the constraints map
            ConstraintBuilder cbv(*_input_constraints, _constraints, _ordered_constraints);
            Utils::Constraint c = cbv.build_constraint(ssa_sym, val, Utils::__GlobalVar);
            (*_input_constraints)[n] = c;

            n.replace(ssa_sym.make_nodecl(/*set_ref_type*/false));
        }
    }

    void ConstraintReplacement::visit(const Nodecl::ClassMemberAccess& n)
    {
        if (_input_constraints->find(n) != _input_constraints->end())
            n.replace((*_input_constraints)[n].get_symbol().make_nodecl(/*set_ref_type*/false));
        else
        {
            // Create a new ssa variable here to use it from now on in the current function
            // 1. Build a symbol for the new constraint based on the name of the original variable
            std::stringstream ss; ss << get_next_id(n);
            std::string class_member_str = get_class_member_string(n, *_input_constraints);
            std::string ssa_name = class_member_str + ss.str();
            Symbol ssa_sym(ssa_scope.new_symbol(ssa_name));
            Type t = n.get_type();
            ssa_sym.set_type(t);
            ssa_to_original_var[ssa_sym] = n;

            // 2. Build the value of the constraint
            NBase val = Nodecl::Range::make(minus_inf.shallow_copy(),
                                            plus_inf.shallow_copy(),
                                            const_value_to_nodecl(zero), t);

            // 3. Build the constraint and insert it in the constraints map
            ConstraintBuilder cbv(*_input_constraints, _constraints, _ordered_constraints);
            Utils::Constraint c = cbv.build_constraint(ssa_sym, val, Utils::__GlobalVar);
            (*_input_constraints)[n] = c;

            n.replace(ssa_sym.make_nodecl(/*set_ref_type*/false));
        }
    }

    void ConstraintReplacement::visit(const Nodecl::Conversion& n)
    {
        // Catch constants at Conversion level
        if (n.is_constant())
        {
            n.replace(const_value_to_nodecl(n.get_constant()));
        }
        else
        {
            walk(n.get_nest());
        }
    }

    void ConstraintReplacement::visit(const Nodecl::FunctionCall& n)
    {
        if (_input_constraints->find(n) != _input_constraints->end())
            n.replace((*_input_constraints)[n].get_symbol().make_nodecl(/*set_ref_type*/false));
        else
        {
            Type t(Type::get_long_int_type());
            NBase val = Nodecl::Range::make(
                    minus_inf.shallow_copy(),
                    plus_inf.shallow_copy(),
                    const_value_to_nodecl(zero), t);
            n.replace(val);
        }
    }

    void ConstraintReplacement::visit(const Nodecl::Symbol& n)
    {
        if (_input_constraints->find(n) == _input_constraints->end())
        {
            Symbol n_sym = n.get_symbol();

            // FunctionCalls are replaced with the value [-inf, +inf]
            if (n_sym.is_function())
            {
                // 1. Build a symbol for the new constraint based on the name of the original variable
                std::stringstream ss; ss << get_next_id(n);
                Symbol orig_s(Utils::get_nodecl_base(n).get_symbol());
                std::string constr_name = orig_s.get_name() + "_" + ss.str();
                Symbol s(ssa_scope.new_symbol(constr_name));
                Type t = orig_s.get_type();
                s.set_type(t);
                ssa_to_original_var[s] = n;
                // 2. Get the value for the constraint
                NBase val = Nodecl::Range::make(minus_inf.shallow_copy(),
                                                plus_inf.shallow_copy(),
                                                const_value_to_nodecl(zero), t);
                // 3. Build the constraint and insert it in the constraints map
                ConstraintBuilder cbv(*_input_constraints, _constraints, _ordered_constraints);
                Utils::Constraint c = cbv.build_constraint(s, val, Utils::__GlobalVar);
                (*_input_constraints)[n] = c;
                return;
            }
            else if (!n_sym.is_variable())
            {   // The symbol is probably an enum
                ERROR_CONDITION(!n.is_constant(),
                                "Expected enum symbol whereas '%s' is not an enum.\n",
                                n_sym.get_name().c_str());
                n.replace(const_value_to_nodecl(n.get_constant()));
                return;
            }

            ERROR_CONDITION(n_sym.get_scope().is_namespace_scope(),
                    "No constraints found for non-global symbol %s in locus %s."
                    "We should replace the variable with the corresponding constraint.",
                    n.prettyprint().c_str(), n.get_locus_str().c_str());

            return;
        }

        n.replace((*_input_constraints)[n].get_symbol().make_nodecl(/*set_ref_type*/false));
    }

    // ************ Class replacing original variables with ssa symbols ************ //
    // ***************************************************************************** //



    // ***************************************************************************** //
    // **************** Visitor building constraints from statements *************** //

    ConstraintBuilder::ConstraintBuilder(
            const VarToConstraintMap& input_constraints_map,
            Constraints *constraints,
            std::vector<Symbol> *ordered_constraints)
        : _input_constraints(input_constraints_map), _output_constraints(), 
          _output_true_constraints(), _output_false_constraints(), 
          _constraints(constraints), _ordered_constraints(ordered_constraints)
    {}

    ConstraintBuilder::ConstraintBuilder(
            const VarToConstraintMap& input_constraints_map,
            const VarToConstraintMap& current_constraints,
            Constraints *constraints,
            std::vector<Symbol> *ordered_constraints)
        : _input_constraints(input_constraints_map), _output_constraints(current_constraints),
          _output_true_constraints(), _output_false_constraints(), 
          _constraints(constraints), _ordered_constraints(ordered_constraints)
    {}

    Utils::Constraint ConstraintBuilder::build_constraint(
            const Symbol& s,
            const NBase& val,
            Utils::ConstraintKind c_kind)
    {
        NBase val_no_conv = val;
        while (val_no_conv.is<Nodecl::Conversion>())
            val_no_conv = val_no_conv.as<Nodecl::Conversion>().get_nest();
        // Create the constraint
        Utils::Constraint c(s, val_no_conv, c_kind);

        // Insert the constraint in the global structures
        // that will allow us building the Constraint Graph
        // We only insert in the ordered constraints set if the SSA variable was not there yet
        // This is caused by the constraints derived from the back edges,
        // which reuse SSA variables that were calculated before
        if (_constraints->find(s) == _constraints->end())
            _ordered_constraints->push_back(s);
        (*_constraints)[s] = val_no_conv;

        // Print the constraint in the standard error
        c.print_constraint();

        return c;
    }

    void ConstraintBuilder::compute_parameters_constraints(const ObjectList<Symbol>& params)
    {
        for (ObjectList<Symbol>::const_iterator it = params.begin(); it != params.end(); ++it)
        {
            Symbol param = *it;

            // Avoid function pointers
            if (param.get_type().is_pointer()
                    && param.get_type().points_to().is_function())
                continue;

            Nodecl::Symbol param_n = param.make_nodecl(/*set_ref_type*/false);
            Type t = param.get_type();

            // Build a symbol for the new constraint based on the name of the original variable
            std::stringstream ss; ss << get_next_id(param_n);
            std::string ssa_name = param.get_name() + "_" + ss.str();
            Symbol ssa_sym(param.get_scope().new_symbol(ssa_name));
            ssa_sym.set_type(t);
            ssa_to_original_var[ssa_sym] = param_n;

            // Get the value for the constraint
            NBase val = Nodecl::Range::make(minus_inf.shallow_copy(), 
                                            plus_inf.shallow_copy(), 
                                            const_value_to_nodecl(zero), t);

            // Build the constraint and insert it in the constraints map
            Utils::Constraint c = build_constraint(ssa_sym, val, Utils::__Parameter);
            _output_constraints[param_n] = c;
        }
    }

    void ConstraintBuilder::compute_non_local_symbols_constraints(
        const ObjectList<Nodecl::Symbol>& non_local_syms)
    {
        for (ObjectList<Nodecl::Symbol>::const_iterator it = non_local_syms.begin();
             it != non_local_syms.end(); ++it)
        {
            Nodecl::Symbol occ = *it;

            Symbol s = occ.get_symbol();
            if (s.is_function())    // Called functions do not require initial constraint
                continue;

            Type t = s.get_type();
            if (t.is_array() || t.is_pointer())     // We do not process the values of pointers so far
                continue;

            // Build a symbol for the new constraint based on the name of the original variable
            std::stringstream ss; ss << get_next_id(occ);
            std::string ssa_name = s.get_name() + "_" + ss.str();
            Symbol ssa_sym(s.get_scope().new_symbol(ssa_name));
            ssa_sym.set_type(t);
            ssa_to_original_var[ssa_sym] = occ;

            // Get the value for the constraint
            // Note that the constant value is not associated to the occurrence anymore,
            // but to the conversion, so we must check the value associated to the symbol instead
            NBase val;
            NBase s_val = s.get_value();
            if (s_val.is_constant())
            {
                const NBase& const_val = const_value_to_nodecl(s_val.get_constant());
                val = Nodecl::Range::make(const_val.shallow_copy(),
                                          const_val.shallow_copy(),
                                          const_value_to_nodecl(zero), t);
                val.set_constant(s_val.get_constant());
            }
            else
            {
                val = Nodecl::Range::make(minus_inf.shallow_copy(),
                                          plus_inf.shallow_copy(),
                                          const_value_to_nodecl(zero), t);
            }

            // Build the constraint and insert it in the constraints map
            Utils::Constraint c = build_constraint(ssa_sym, val, Utils::__NonLocalSym);
            _output_constraints[occ] = c;
        }
    }

    void ConstraintBuilder::set_false_constraint_to_inf(const NBase& n)
    {
        if (n.is<Nodecl::Equal>() || n.is<Nodecl::Different>()
                || n.is<Nodecl::LowerThan>() || n.is<Nodecl::LowerOrEqualThan>()
                || n.is<Nodecl::GreaterThan>() || n.is<Nodecl::GreaterOrEqualThan>()
                || n.is<Nodecl::LogicalNot>())
        {
            // False constraints are not just the negation of the condition,
            // because LHS fulfilling is not enough for the whole condition to fulfill
            const NBase& lhs = n.as<Nodecl::Equal>().get_lhs();
            ERROR_CONDITION(_output_false_constraints.find(lhs) == _output_false_constraints.end(),
                            "Nodecl %s not found in the set of 'output false constraints' while replacing constraint",
                            lhs.prettyprint().c_str());
            Utils::Constraint old_c_false = _output_false_constraints[lhs];
            TL::Symbol old_s = old_c_false.get_symbol();
            const NBase& old_val = old_c_false.get_value();
            ERROR_CONDITION(!old_val.is<Nodecl::Analysis::RangeIntersection>(),
                            "Constraint value of a 'false' flow edge has type '%s' when RangeIntersection expected.\n",
                            ast_print_node_type(old_val.get_kind()));
            const NBase& old_val_input_ssa_var = old_val.as<Nodecl::Analysis::RangeIntersection>().get_lhs();
            ERROR_CONDITION(ssa_to_original_var.find(old_val_input_ssa_var.get_symbol()) == ssa_to_original_var.end(),
                            "Constraint value of a 'false' flow edge does not contain ssa variable, but '%s' instead",
                            old_val_input_ssa_var.prettyprint().c_str());
            Type t(Type::get_long_int_type());
            NBase val_false = Nodecl::Analysis::RangeIntersection::make(
                    old_val_input_ssa_var,
                    Nodecl::Range::make(
                            minus_inf.shallow_copy(),
                            plus_inf.shallow_copy(),
                            const_value_to_nodecl(zero), t),
                    t);
            Utils::Constraint new_c_false = build_constraint(old_s, val_false, Utils::__Replace);
            _output_false_constraints[lhs] = new_c_false;
        }
        else if (n.is<Nodecl::LogicalAnd>() || n.is<Nodecl::Different>())
        {}  // Nothing to be done because the infinite range is set to the edge recursively
        else
        {
            internal_error("Unexpected node of type %s while setting false branch to [-inf, +inf]\n",
                           ast_print_node_type(n.get_kind()));
        }
    }

    VarToConstraintMap ConstraintBuilder::get_output_constraints() const
    {
        return _output_constraints;
    }

    VarToConstraintMap ConstraintBuilder::get_output_true_constraints() const
    {
        return _output_true_constraints;
    }

    VarToConstraintMap ConstraintBuilder::get_output_false_constraints() const
    {
        return _output_false_constraints;
    }

    void ConstraintBuilder::join_list(TL::ObjectList<Utils::Constraint>& list)
    {
        WARNING_MESSAGE("join_list of a list of constraint is not yet supported. Doing nothing.", 0);
    }

    void ConstraintBuilder::visit_assignment(const NBase& lhs, const NBase& rhs)
    {
        // 1.- Build a symbol for the new constraint based on
        //     the name of the original variable
        std::string ssa_name;
        if (lhs.no_conv().is<Nodecl::ArraySubscript>())
        {
            ssa_name = get_array_subscript_string(
                            lhs.no_conv().as<Nodecl::ArraySubscript>(),
                            _input_constraints);
        }
        else
        {
            ssa_name = Utils::get_nodecl_base(lhs).get_symbol().get_name() + "_";
        }
        std::stringstream ss; ss << get_next_id(lhs);
        ssa_name += ss.str();
        Symbol ssa_sym(ssa_scope.new_symbol(ssa_name));
        Type t = Utils::get_nodecl_base(lhs).get_symbol().get_type();
        ssa_sym.set_type(t);
        ssa_to_original_var[ssa_sym] = lhs;

        // 2.- Build the value of the constraint
        NBase val;
        if (rhs.is_constant())       // x = c;    -->    X1 = c
        {
            const_value_t* rhs_cnst = rhs.get_constant();
            if (const_value_is_object(rhs_cnst) || const_value_is_address(rhs_cnst))
            {
                // We know the value is constant, but cannot know its value at compile time
                val = Nodecl::Range::make(minus_inf.shallow_copy(),
                                          plus_inf.shallow_copy(),
                                          const_value_to_nodecl(zero), t);
            }
            else
            {
                Nodecl::NodeclBase cnst = const_value_to_nodecl(rhs_cnst);
                val = Nodecl::Range::make(cnst.shallow_copy(), cnst.shallow_copy(), const_value_to_nodecl(zero), t);
                val.set_constant(rhs_cnst);
            }
        }
        else 
        {   // Replace all the memory accesses by the symbols of the constraints arriving to the current node
            val = rhs.no_conv().shallow_copy();
            ConstraintReplacement cr(&_input_constraints, _constraints, _ordered_constraints);
            cr.walk(val);
        }

        // 3.- Build the constraint and insert it in the corresponding maps
        Utils::Constraint c = build_constraint(ssa_sym, val, Utils::__BinaryOp);
        _input_constraints[lhs] = c;
        _output_constraints[lhs] = c;
    }
    
    
    void ConstraintBuilder::visit_increment(const NBase& rhs, bool positive)
    {
        // 1.- Check the integrity of the analysis
        ERROR_CONDITION(_input_constraints.find(rhs) == _input_constraints.end(),
                        "Some input constraint required for the increment's RHS '%s' (%s).\n",
                        rhs.prettyprint().c_str(),
                        ast_print_node_type(rhs.get_kind()));
        
        // 2.- Build a symbol for the new constraint based on the name of the original variable
        std::stringstream ss; ss << get_next_id(rhs);
        Symbol s(Utils::get_nodecl_base(rhs).get_symbol());
        Type t(s.get_type());
        std::string constr_name = s.get_name() + "_" + ss.str();
        Symbol ssa_sym(ssa_scope.new_symbol(constr_name));
        ssa_sym.set_type(t);
        ssa_to_original_var[ssa_sym] = rhs;

        NBase val;
        Symbol entry_ssa_sym = _input_constraints[rhs].get_symbol();
        if (positive)
        {
            val = Nodecl::Add::make(entry_ssa_sym.make_nodecl(/*set_ref_type*/false),
                                    const_value_to_nodecl(one), t);
        }
        else
        {
            val = Nodecl::Minus::make(entry_ssa_sym.make_nodecl(/*set_ref_type*/false),
                                      const_value_to_nodecl(one), t);
        }
        Utils::Constraint c = build_constraint(ssa_sym, val, Utils::__UnaryOp);
        _input_constraints[rhs] = c;
        _output_constraints[rhs] = c;
    }

    void ConstraintBuilder::create_array_fake_constraint(const NBase& n)
    {   // It may happen that this position of the array has a value set,
        // but a different subscript was used
        // Add a new constraint for this nodecl with value [-inf, +inf]

        // 1.- Build a symbol for the new constraint based on the name of the original variable
        std::string array_subscript_str = get_array_subscript_string(
                n.no_conv().as<Nodecl::ArraySubscript>(),
                _input_constraints);
        std::stringstream ss; ss << get_next_id(n);
        std::string ssa_name = array_subscript_str + ss.str();
        Symbol ssa_sym(ssa_scope.new_symbol(ssa_name));
        Type t = Utils::get_nodecl_base(n).get_symbol().get_type();
        ssa_sym.set_type(t);
        ssa_to_original_var[ssa_sym] = n;

        // 2.- Build the value for the constraint
        NBase inf_val = Nodecl::Range::make(minus_inf.shallow_copy(),
                                            plus_inf.shallow_copy(),
                                            const_value_to_nodecl(zero), t);

        // 3.- Build the constraint and insert it in the constraints map
        Utils::Constraint c = build_constraint(ssa_sym, inf_val, Utils::__Array);
        _input_constraints[n] = c;  // We add it in the input constraints, so
        // the rest of this algorithm works properly
        _output_constraints[n] = c;
    }

    void ConstraintBuilder::create_struct_fake_constraint(const NBase& n)
    {   // NOTE: To simplify we create a fake constraint with value [-inf, +inf]

        // 1.- Build a symbol for the new constraint based on the name of the original variable
        std::string member_str = get_class_member_string(
                n.no_conv().as<Nodecl::ClassMemberAccess>(),
                _input_constraints);
        std::stringstream ss; ss << get_next_id(n);
        std::string ssa_name = member_str + ss.str();
        Symbol ssa_sym(ssa_scope.new_symbol(ssa_name));
        Type t = Utils::get_nodecl_base(n).get_symbol().get_type();
        ssa_sym.set_type(t);
        ssa_to_original_var[ssa_sym] = n;

        // 2.- Build the value for the constraint
        NBase inf_val = Nodecl::Range::make(minus_inf.shallow_copy(),
                                            plus_inf.shallow_copy(),
                                            const_value_to_nodecl(zero), t);

        // 3.- Build the constraint and insert it in the constraints map
        Utils::Constraint c = build_constraint(ssa_sym, inf_val, Utils::__Array);
        _input_constraints[n] = c;  // We add it in the input constraints, so
        // the rest of this algorithm works properly
        _output_constraints[n] = c;
    }

    // NOTE build_different_constraint is the same as build_equal_constraint
    // exchanging val_true and val_false
    // so we use the same method and change the order of the arguments when calling
    void ConstraintBuilder::build_equal_constraint(
            /*in*/  const Symbol& ssa_sym,
            /*in*/  const NBase& n,
            /*in*/  Type t,
            /*out*/ NBase& val_true,
            /*out*/ NBase& val_false)
    {
        // All constant values must have the same bytes and sign
        // to ensure comparisons work as expected
        const_value_t* n_const = n.get_constant();
        if (n.is_constant()
                && !const_value_is_object(n_const)
                && !const_value_is_address(n_const))
        {
            // FIXME This may cause problems with unsigned values
            if (!const_value_is_signed(n_const))
                n_const = const_value_cast_as_another(n_const, one);
        }
        else
            n_const = NULL;

        // 1.- Build the TRUE constraint value
        // v == x;   --TRUE--->   v1 = v0 ∩ [x, x]
        val_true = Nodecl::Analysis::RangeIntersection::make(
                ssa_sym.make_nodecl(/*set_ref_type*/false),
                Nodecl::Range::make(n.shallow_copy(),
                                    n.shallow_copy(),
                                    const_value_to_nodecl(zero), t),
                t);

        // 4.3.2.- Build the FALSE constraint value
        // v == x;   --FALSE-->   v2 = v0 ∩ ([-∞, x-1] U [x+1, -∞])
        NBase lb, ub;
        if (n_const != NULL)
        {
            const_value_t* lb_const = const_value_add(n_const, one);
            lb = const_value_to_nodecl(lb_const);
            lb.set_constant(lb_const);
            const_value_t* ub_const = const_value_sub(n_const, one);
            ub = const_value_to_nodecl(ub_const);
            ub.set_constant(ub_const);
        }
        else
        {
            lb = Nodecl::Add::make(n.shallow_copy(), const_value_to_nodecl(one), t);
            ub = Nodecl::Minus::make(n.shallow_copy(), const_value_to_nodecl(one), t);
        }
        val_false = Nodecl::Analysis::RangeIntersection::make(
            ssa_sym.make_nodecl(/*set_ref_type*/false),
                Nodecl::Analysis::RangeUnion::make(
                        Nodecl::Range::make(minus_inf.shallow_copy(), ub,
                                            const_value_to_nodecl(zero), t),
                        Nodecl::Range::make(lb, plus_inf.shallow_copy(),
                                            const_value_to_nodecl(zero), t),
                        t),
                t);
    }

    void ConstraintBuilder::build_lower_constraint(
        /*in*/  const Symbol& ssa_sym,
        /*in*/  const NBase& n,
        /*in*/  Type t,
        /*out*/ NBase& val_true,
        /*out*/ NBase& val_false)
    {
        // All constant values must have the same bytes and sign
        // to ensure comparisons work as expected
        const_value_t* n_const = n.get_constant();
        if (n.is_constant()
            && !const_value_is_object(n_const)
            && !const_value_is_address(n_const))
        {
            // FIXME This may cause problems with unsigned values
            if (!const_value_is_signed(n_const))
                n_const = const_value_cast_as_another(n_const, one);
        }
        else
            n_const = NULL;

        NBase lb, ub;
        NBase incr = const_value_to_nodecl(zero);

        // 1.- Build the TRUE constraint value for the LHS
        //     v < x;   --TRUE--->  v1 = v0 ∩ [-∞, x-1]
        // 1.1.- Build LB: -∞
        lb = minus_inf.shallow_copy();
        // 1.2.- Build UB: x-1
        if (n_const != NULL)
        {
            const_value_t* ub_const = const_value_sub(n_const, one);
            ub = const_value_to_nodecl(ub_const);
            ub.set_constant(ub_const);
        }
        else
        {
            ub = Nodecl::Minus::make(n.shallow_copy(), const_value_to_nodecl(one), t);
        }
        // 1.3.- Build the whole value: v0 ∩ [-∞, x-1]
        val_true = Nodecl::Analysis::RangeIntersection::make(
                ssa_sym.make_nodecl(/*set_ref_type*/false),
                Nodecl::Range::make(lb, ub, incr.shallow_copy(), t),
                t);

        // 2.- Build the FALSE constraint value for the LHS
        //     v < x;   --FALSE-->  v2 = v0 ∩ [x, +∞]
        // 2.1.- Build LB: -∞
        if (n_const != NULL)
            lb = const_value_to_nodecl(n_const);
        else
            lb = n.shallow_copy();
        // 2.1.- Build UB: +∞
        ub = plus_inf.shallow_copy();
        // 2.3.- Build the whole value: v0 ∩ [x, +∞]
        val_false = Nodecl::Analysis::RangeIntersection::make(
                ssa_sym.make_nodecl(/*set_ref_type*/false),
                Nodecl::Range::make(lb, ub, incr.shallow_copy(), t),
                t);
    }

    void ConstraintBuilder::build_lower_or_equal_constraint(
        /*in*/  const Symbol& ssa_sym,
        /*in*/  const NBase& n,
        /*in*/  Type t,
        /*out*/ NBase& val_true,
        /*out*/ NBase& val_false)
    {
        // All constant values must have the same bytes and sign
        // to ensure comparisons work as expected
        const_value_t* n_const = n.get_constant();
        if (n.is_constant()
            && !const_value_is_object(n_const)
            && !const_value_is_address(n_const))
        {
            // FIXME This may cause problems with unsigned values
            if (!const_value_is_signed(n_const))
                n_const = const_value_cast_as_another(n_const, one);
        }
        else
            n_const = NULL;

        NBase lb, ub;
        NBase incr = const_value_to_nodecl(zero);

        // 1.- Build the TRUE constraint value for the LHS
        //     v <= x;   --TRUE--->  v1 = v0 ∩ [-∞, x]
        // 1.1.- Build LB: -∞
        lb = minus_inf.shallow_copy();
        // 1.2.- Build UB: x
        if (n_const != NULL)
            ub = const_value_to_nodecl(n_const);
        else
            ub = n.shallow_copy();
        // 1.3.- Build the whole value: v0 ∩ [-∞, x]
                val_true = Nodecl::Analysis::RangeIntersection::make(
                ssa_sym.make_nodecl(/*set_ref_type*/false),
                Nodecl::Range::make(lb, ub, incr.shallow_copy(), t),
                t);

        // 2.- Build the FALSE constraint value for the LHS
        //     v <= x;   --FALSE-->  v2 = v0 ∩ [x+1, +∞]
        // 2.1.- Build LB: x
        if (n_const != NULL)
        {
            const_value_t* lb_const = const_value_add(n_const, one);
            lb = const_value_to_nodecl(lb_const);
            lb.set_constant(lb_const);
        }
        else
        {
            lb = Nodecl::Minus::make(n.shallow_copy(), const_value_to_nodecl(one), t);
        }
        // 2.2.- Build UB: +∞
        ub = plus_inf.shallow_copy();
        // 2.3.- Build the whole value: v0 ∩ [x+1, +∞]
        val_false = Nodecl::Analysis::RangeIntersection::make(
                ssa_sym.make_nodecl(/*set_ref_type*/false),
                Nodecl::Range::make(lb, ub, incr.shallow_copy(), t),
                t);
    }

    void ConstraintBuilder::build_greater_constraint(
        /*in*/  const Symbol& ssa_sym,
        /*in*/  const NBase& n,
        /*in*/  Type t,
        /*out*/ NBase& val_true,
        /*out*/ NBase& val_false)
    {
        // All constant values must have the same bytes and sign
        // to ensure comparisons work as expected
        const_value_t* n_const = n.get_constant();
        if (n.is_constant()
            && !const_value_is_object(n_const)
            && !const_value_is_address(n_const))
        {
            // FIXME This may cause problems with unsigned values
            if (!const_value_is_signed(n_const))
                n_const = const_value_cast_as_another(n_const, one);
        }
        else
            n_const = NULL;

        NBase lb, ub;
        NBase incr = const_value_to_nodecl(zero);

        // 1.- Build the TRUE constraint value for the RHS
        //     v > x   --TRUE--->  v1 = v0 ∩ [x+1, +∞]
        // 1.1.- Build LB: x+1
        if (n_const != NULL)
        {
            const_value_t* lb_const = const_value_add(n_const, one);
            lb = const_value_to_nodecl(lb_const);
            lb.set_constant(lb_const);
        }
        else
        {
            lb = Nodecl::Add::make(n.shallow_copy(), const_value_to_nodecl(one), t);
        }
        // 1.2.- Build UB: +∞
        ub = plus_inf.shallow_copy();
        // 1.3.- Build the whole value: v0 ∩ [x+1, +∞]
        val_true = Nodecl::Analysis::RangeIntersection::make(
                ssa_sym.make_nodecl(/*set_ref_type*/false),
                Nodecl::Range::make(lb, ub, incr.shallow_copy(), t),
                t);

        // 2.- Build the FALSE constraint value for the RHS
        //     v > x   --FALSE-->  v2 = v0 ∩ [-∞, x]
        // 1.1.- Build LB: -∞
        lb = minus_inf.shallow_copy();
        // 1.2.- Build UB: x
        if (n_const != NULL)
            ub = const_value_to_nodecl(n_const);
        else
            ub = n.shallow_copy();
        // 1.3.- Build the whole value: v0 ∩ [-∞, x]
        val_false = Nodecl::Analysis::RangeIntersection::make(
                ssa_sym.make_nodecl(/*set_ref_type*/false),
                Nodecl::Range::make(lb, ub, incr.shallow_copy(), t),
                t);
    }

    void ConstraintBuilder::build_greater_or_equal_constraint(
        /*in*/  const Symbol& ssa_sym,
        /*in*/  const NBase& n,
        /*in*/  Type t,
        /*out*/ NBase& val_true,
        /*out*/ NBase& val_false)
    {
        // All constant values must have the same bytes and sign
        // to ensure comparisons work as expected
        const_value_t* n_const = n.get_constant();
        if (n.is_constant()
            && !const_value_is_object(n_const)
            && !const_value_is_address(n_const))
        {
            // FIXME This may cause problems with unsigned values
            if (!const_value_is_signed(n_const))
                n_const = const_value_cast_as_another(n_const, one);
        }
        else
            n_const = NULL;

        NBase lb, ub;
        NBase incr = const_value_to_nodecl(zero);

        // 1.- Build the TRUE constraint value for the RHS
        //     v >= x   --TRUE--->  v1 = v0 ∩ [x, +∞]
        // 1.1.- Build LB: x
        if (n_const != NULL)
            lb = const_value_to_nodecl(n_const);
        else
            lb = n.shallow_copy();
        // 1.2.- Build UB: +∞
        ub = plus_inf.shallow_copy();
        // 1.3.- Build the whole value: v0 ∩ [x, +∞]
        val_true = Nodecl::Analysis::RangeIntersection::make(
                ssa_sym.make_nodecl(/*set_ref_type*/false),
                Nodecl::Range::make(lb, ub, incr.shallow_copy(), t),
                t);

        // 2.- Build the FALSE constraint value for the RHS
        //     v >= x   --FALSE-->  v2 = v0 ∩ [-∞, x-1]
        // 1.1.- Build LB: -∞
        lb = minus_inf.shallow_copy();
        // 1.2.- Build UB: x-1
        if (n_const != NULL)
        {
            const_value_t* ub_const = const_value_sub(n_const, one);
            ub = const_value_to_nodecl(ub_const);
            ub.set_constant(ub_const);
        }
        else
        {
            ub = Nodecl::Add::make(n.shallow_copy(), const_value_to_nodecl(one), t);
        }
        // 1.3.- Build the whole value: v0 ∩ [-∞, x-1]
        val_false = Nodecl::Analysis::RangeIntersection::make(
                ssa_sym.make_nodecl(/*set_ref_type*/false),
                Nodecl::Range::make(lb, ub, incr.shallow_copy(), t),
                t);
    }

    void ConstraintBuilder::compute_comparison_constraint(
            /*in*/  const NBase& n,
            /*in*/  const NBase& m,
            /*in*/  node_t comparison_kind,
            /*out*/ NBase& val_true,
            /*out*/ NBase& val_false)
    {
        // 1.- Get the original symbol of n
        const NBase& base_n = Utils::get_nodecl_base(n);
        Symbol s(base_n.get_symbol());
        Type t = s.get_type();
        std::string s_name = s.get_name();

        // 2.- Get the last ssa symbol related to the original symbol
        std::string last_ssa_name = _input_constraints.find(n)->second.get_symbol().get_name();
        Symbol last_ssa = ssa_scope.get_symbol_from_name(last_ssa_name);
        ERROR_CONDITION(!last_ssa.is_valid(),
                        "No symbol '%s' found while building constraint for variable '%s'",
                        last_ssa_name.c_str(), n.prettyprint().c_str());

        // 3.- Compute the constraints generated from the condition
        //         to the possible TRUE and FALSE exit edges
        // =========================================================
        // 3.1.- Build the symbols created for the TRUE and FALSE edges
        // 3.1.1.- Build the TRUE constraint symbol
        std::stringstream ss_true; ss_true << get_next_id(n);
        Symbol s_true(ssa_scope.new_symbol(s_name + "_" + ss_true.str()));
        s_true.set_type(t);
        ssa_to_original_var[s_true] = n;
        // 3.1.2.- Build the FALSE constraint symbol
        std::stringstream ss_false; ss_false << get_next_id(n);
        Symbol s_false(ssa_scope.new_symbol(s_name + "_" + ss_false.str()));
        s_false.set_type(t);
        ssa_to_original_var[s_false] = n;

        // 3.2.- Build the values created for the TRUE and FALSE edges
        NBase m_ssa = (m.is_constant()
                            ? m.shallow_copy()
                            : _input_constraints.find(m)->second.get_symbol().make_nodecl());
        switch (comparison_kind)
        {
            case NODECL_EQUAL:
            {
                build_equal_constraint(last_ssa, m_ssa, t, val_true, val_false);
                break;
            }
            case NODECL_DIFFERENT:
            {
                build_equal_constraint(last_ssa, m_ssa, t, val_false, val_true);
                break;
            }
            case NODECL_LOWER_THAN:
            {
                build_lower_constraint(last_ssa, m_ssa, t, val_true, val_false);
                break;
            }
            case NODECL_LOWER_OR_EQUAL_THAN:
            {
                build_lower_or_equal_constraint(last_ssa, m_ssa, t, val_true, val_false);
                break;
            }
            case NODECL_GREATER_THAN:
            {
                build_greater_constraint(last_ssa, m_ssa, t, val_true, val_false);
                break;
            }
            case NODECL_GREATER_OR_EQUAL_THAN:
            {
                build_greater_or_equal_constraint(last_ssa, m_ssa, t, val_true, val_false);
                break;
            }
            default:
            {
                internal_error("Unexpected node kind %s while building constraint for a comparison.\n",
                                ast_print_node_type(comparison_kind));
            }
        };

        // 3.3.- Build the TRUE and FALSE constraints and store them
        Utils::Constraint c_true
                = build_constraint(s_true, val_true, Utils::__ComparatorTrue);
        _output_true_constraints[n] = c_true;
        Utils::Constraint c_false
                = build_constraint(s_false, val_false, Utils::__ComparatorFalse);
        _output_false_constraints[n] = c_false;
    }

namespace {
    void invert_comparison_kind(node_t& kind)
    {
        switch(kind)
        {
            case NODECL_EQUAL:
            case NODECL_DIFFERENT:
            {   // Nothing to change
                break;
            }
            case NODECL_LOWER_THAN:
            {
                kind = NODECL_GREATER_THAN;
                break;
            }
            case NODECL_LOWER_OR_EQUAL_THAN:
            {
                kind = NODECL_GREATER_OR_EQUAL_THAN;
                break;
            }
            case NODECL_GREATER_THAN:
            {
                kind = NODECL_LOWER_THAN;
                break;
            }
            case NODECL_GREATER_OR_EQUAL_THAN:
            {
                kind = NODECL_LOWER_OR_EQUAL_THAN;
                break;
            }
            default:
            {
                internal_error("Unexpected node kind %s while building constraint for a comparison.\n",
                               ast_print_node_type(kind));
            }
        }
    }
}

    // This method assumes the lhs is always a variable and the rhs may be a constant or a variable
    void ConstraintBuilder::visit_comparison(
            const NBase& lhs,
            const NBase& rhs,
            node_t comparison_kind)
    {
        // 1.- Base case: both sides are constant
        if (lhs.is_constant() && rhs.is_constant())
        {   // No constraint needs to be created
            WARNING_MESSAGE("Comparison %d %s %d generates no constraint.\n",
                            lhs.prettyprint().c_str(),
                            ast_print_node_type(comparison_kind),
                            rhs.prettyprint().c_str());
            return;
        }

        // 2.- Simple cases: one side is constant
        if (lhs.is_constant())
        {   // LHS is constant and RHS is not

            // 2.1.1.- Check supported case
            NBase rhs_no_conv = rhs.no_conv();
            if (!rhs_no_conv.is<Nodecl::Symbol>()
                    && !rhs_no_conv.is<Nodecl::ArraySubscript>())
            {
                internal_error("Unsupported case: comparison with no single symbol at any side: %d %s %d.\n",
                               lhs.prettyprint().c_str(),
                               ast_print_node_type(comparison_kind),
                               rhs.prettyprint().c_str());
            }

            // 2.1.2.- Create fake input constraint if necessary
            if (rhs_no_conv.is<Nodecl::ArraySubscript>())
            {
                create_array_fake_constraint(rhs_no_conv);
            }
            else if (rhs_no_conv.is<Nodecl::ClassMemberAccess>())
            {
                create_struct_fake_constraint(rhs_no_conv);
            }

            // 2.3.3.2.- Compute the constraints generated from the condition
            //           Since the order of the operands is inverted,
            //           the operation must be inverted too
            NBase val_true, val_false;
            invert_comparison_kind(comparison_kind);
            compute_comparison_constraint(
                rhs_no_conv, lhs,
                comparison_kind,
                val_true, val_false);
        }
        else if (rhs.is_constant())
        {   // RHS is constant and LHS is not

            // 2.2.1.- Check supported case
            NBase lhs_no_conv = lhs.no_conv();
            if (!lhs_no_conv.is<Nodecl::Symbol>()
                    && !lhs_no_conv.is<Nodecl::ArraySubscript>()
                    && !lhs_no_conv.is<Nodecl::ClassMemberAccess>())
            {
                internal_error("Unsupported case: comparison with no single symbol at any side: %d %s %d.\n",
                                lhs.prettyprint().c_str(),
                                ast_print_node_type(comparison_kind),
                                rhs.prettyprint().c_str());
            }

            // 2.2.2.- Create fake input constraint if necessary
            if (lhs_no_conv.is<Nodecl::ArraySubscript>())
            {
                create_array_fake_constraint(lhs_no_conv);
            }
            else if (lhs_no_conv.is<Nodecl::ClassMemberAccess>())
            {
                create_struct_fake_constraint(lhs_no_conv);
            }

            // 2.2.3.- Compute the constraints generated from the condition
            NBase val_true, val_false;
            compute_comparison_constraint(
                    lhs_no_conv, rhs,
                    comparison_kind,
                    val_true, val_false);
        }
        // 3.- Other cases
        else
        {

            // 2.3.1.- Check supported case
            NBase lhs_no_conv = lhs.no_conv();
            NBase rhs_no_conv = rhs.no_conv();
            if (!lhs_no_conv.is<Nodecl::Symbol>()
                    && !lhs_no_conv.is<Nodecl::ArraySubscript>()
                    && !lhs_no_conv.is<Nodecl::ClassMemberAccess>()
                    && !rhs_no_conv.is<Nodecl::Symbol>()
                    && !rhs_no_conv.is<Nodecl::ArraySubscript>()
                    && !rhs_no_conv.is<Nodecl::ClassMemberAccess>())
            {
                internal_error("Unsupported case: comparison with no single symbol at any side: %s %s %s.\n",
                               lhs.prettyprint().c_str(),
                               ast_print_node_type(comparison_kind),
                               rhs.prettyprint().c_str());
            }

            // 2.3.2.- Create fake input constraint if necessary
            if (lhs_no_conv.is<Nodecl::ArraySubscript>())
            {
                create_array_fake_constraint(lhs_no_conv);
            }
            else if (lhs_no_conv.is<Nodecl::ClassMemberAccess>())
            {
                create_struct_fake_constraint(lhs_no_conv);
            }
            if (rhs_no_conv.is<Nodecl::ArraySubscript>())
            {
                create_array_fake_constraint(rhs);
            }
            else if (rhs_no_conv.is<Nodecl::ClassMemberAccess>())
            {
                create_struct_fake_constraint(rhs_no_conv);
            }

            // 2.3.3.- Compute the constraints generated from the condition
            NBase val_true, val_false;
            // 2.3.3.1.- Call for the symbol in the lhs
            compute_comparison_constraint(
                    lhs_no_conv, rhs,
                    comparison_kind,
                    val_true, val_false);
            // 2.3.3.2.- Call for the symbol in the rhs
            //           Since the order of the operands is inverted,
            //           the operation must be inverted too
            // FIXME We assume the RHS is not modified within the loop
//             invert_comparison_kind(comparison_kind);
//             compute_comparison_constraint(
//                     rhs_no_conv, lhs,
//                     comparison_kind,
//                     val_true, val_false);
        }
    }

    void ConstraintBuilder::visit(const Nodecl::AddAssignment& n)
    {
        NBase lhs = n.get_lhs();
        NBase rhs = n.get_rhs();
        NBase new_rhs
                = Nodecl::Assignment::make(
                        lhs.shallow_copy(),
                        Nodecl::Add::make(lhs.shallow_copy(),
                                          rhs.shallow_copy(),
                                          rhs.get_type()),
                        lhs.get_type());
        n.replace(new_rhs);
        visit_assignment(n.get_lhs(), n.get_rhs());
    }

    void ConstraintBuilder::visit(const Nodecl::Assignment& n)
    {
        visit_assignment(n.get_lhs(), n.get_rhs());
    }

    // x != c;   ---TRUE-->    X1 = X0 ∩ ([-∞, c-1] U [c+1, -∞])
    //           --FALSE-->    X1 = X0 ∩ [c, c]
    void ConstraintBuilder::visit(const Nodecl::Different& n)
    {
        visit_comparison(n.get_lhs(),
                         n.get_rhs(),
                         NODECL_DIFFERENT); 
    }

    // x == c;   ---TRUE-->    X1 = X0 ∩ [c, c]
    //           --FALSE-->    X1 = X0 ∩ ([-∞, c-1] U [c+1, -∞])
    void ConstraintBuilder::visit(const Nodecl::Equal& n)
    {
        visit_comparison(n.get_lhs(),
                         n.get_rhs(),
                         NODECL_EQUAL);
    }

    // x > c;   ---TRUE-->    X1 = X0 ∩ [ c+1, +∞ ]
    //          --FALSE-->    X1 = X0 ∩ [-∞, c]
    void ConstraintBuilder::visit(const Nodecl::GreaterThan& n)
    {
        visit_comparison(n.get_lhs(),
                         n.get_rhs(),
                         NODECL_GREATER_THAN);
    }

    // x >= c;   ---TRUE-->    X1 = X0 ∩ [ c, +∞ ]
    //           --FALSE-->    X1 = X0 ∩ [-∞, c-1]
    void ConstraintBuilder::visit(const Nodecl::GreaterOrEqualThan& n)
    {
        visit_comparison(n.get_lhs(),
                         n.get_rhs(),
                         NODECL_GREATER_OR_EQUAL_THAN);
    }

    void ConstraintBuilder::visit(const Nodecl::LogicalAnd& n)
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

    void ConstraintBuilder::visit(const Nodecl::LogicalNot& n)
    {
        const NBase& rhs = n.get_rhs();
        const NBase& lhs = const_value_to_nodecl(zero);
        visit_comparison(rhs,
                         lhs,
                         NODECL_EQUAL);
    }

    // x <= c;    ---TRUE-->    X1 = X0 ∩ [-∞, c]
    //            --FALSE-->    X1 = X0 ∩ [ c+1,  +∞]
    void ConstraintBuilder::visit(const Nodecl::LowerOrEqualThan& n)
    {
        visit_comparison(n.get_lhs(),
                         n.get_rhs(),
                         NODECL_LOWER_OR_EQUAL_THAN);
    }

    // x < c;    ---TRUE-->    X1 = X0 ∩ [-∞, c-1]
    //           --FALSE-->    X1 = X0 ∩ [ c,  +∞]
    void ConstraintBuilder::visit(const Nodecl::LowerThan& n)
    {
        visit_comparison(n.get_lhs(),
                         n.get_rhs(),
                         NODECL_LOWER_THAN);
    }

    // x % c;   ---TRUE-->    X1 = X0 ∩ [0, c-1]
    //          --FALSE-->    X1 = X0 ∩ ([-∞, -1] U [c, -∞])
    void ConstraintBuilder::visit(const Nodecl::Mod& n)
    {
        NBase lhs = n.get_lhs().no_conv();
        NBase rhs = n.get_rhs().no_conv();
        // 1.- Check the input is something we expect: LHS has a constraint
        ERROR_CONDITION(_input_constraints.find(lhs) == _input_constraints.end(),
                        "Some input constraint required for the LHS when parsing a %s nodecl",
                        ast_print_node_type(n.get_kind()));

        Symbol s(Utils::get_nodecl_base(lhs).get_symbol());
        Type t = s.get_type();
        std::string s_name = s.get_name();
        // Get the last ssa symbol related to the original symbol
        std::string last_ssa_name = _input_constraints.find(n)->second.get_symbol().get_name();
        Symbol last_ssa_s = ssa_scope.get_symbol_from_name(last_ssa_name);
        ERROR_CONDITION(!s.is_valid(),
                        "No symbol '%s' found while building constraint for node '%s'",
                        s.get_name().c_str(), n.prettyprint().c_str());

        // 2.- Compute the constraints generated from the condition to the possible TRUE and FALSE exit edges
        NBase val = rhs.shallow_copy();
        if (!val.is_constant())
        {   // Replace all the memory accesses by the ssa symbols arriving to the current node
            ConstraintReplacement cr(&_input_constraints, _constraints, _ordered_constraints);
            cr.walk(val);
        }

        // 2.1.- Compute the constraint that corresponds to the true branch taken from this node
        // x < x;       --TRUE-->       X1 = X0 ∩ [0, c-1]
        // 2.1.1.- Build the TRUE constraint symbol
        std::stringstream ss_true; ss_true << get_next_id(lhs);
        Symbol s_true(ssa_scope.new_symbol(s_name + "_" + ss_true.str()));
        s_true.set_type(t);
        ssa_to_original_var[s_true] = lhs;
        // 2.1.2.- Build the TRUE constraint value
        NBase ub = (rhs.is_constant() ? const_value_to_nodecl(const_value_sub(rhs.get_constant(), one)) 
                                      : Nodecl::Minus::make(val.shallow_copy(), const_value_to_nodecl(one), t));
        NBase val_true = 
            Nodecl::Analysis::RangeIntersection::make(
                last_ssa_s.make_nodecl(/*set_ref_type*/false), 
                Nodecl::Range::make(const_value_to_nodecl(zero),
                                    ub, 
                                    const_value_to_nodecl(zero), t),
                t);
        // 2.1.3.- Build the TRUE constraint and store it
        Utils::Constraint c_true = build_constraint(s_true, val_true, Utils::__ModTrue);
        _output_true_constraints[lhs] = c_true;
        // 2.2.- Compute the constraint that corresponds to the false branch taken from this node
        // x < c;       --FALSE-->      X1 = X0 ∩ ([-∞, -1] U [c, -∞])
        // 2.2.1.- Build the FALSE constraint symbol
        std::stringstream ss_false; ss_false << get_next_id(lhs);
        Symbol s_false(ssa_scope.new_symbol(s_name + "_" + ss_false.str()));
        s_false.set_type(t);
        ssa_to_original_var[s_false] = lhs;
        // 2.2.2.- Build the FALSE constraint value
        NBase val_false = 
            Nodecl::Analysis::RangeIntersection::make(
                last_ssa_s.make_nodecl(/*set_ref_type*/false),
                Nodecl::Analysis::RangeUnion::make(
                    Nodecl::Range::make(minus_inf.shallow_copy(),
                                        const_value_to_nodecl(minus_one),
                                        const_value_to_nodecl(zero), t),
                    Nodecl::Range::make(val.shallow_copy(),
                                        plus_inf.shallow_copy(),
                                        const_value_to_nodecl(zero), t),
                    t),
                t);
        // 2.2.3.- Build the FALSE constraint and store it
        Utils::Constraint c_false = build_constraint(s_false, val_false, Utils::__ModFalse);
        _output_false_constraints[lhs] = c_false;
    }

    void ConstraintBuilder::visit(const Nodecl::ObjectInit& n)
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
    void ConstraintBuilder::visit(const Nodecl::Postdecrement& n)
    {
        visit_increment(n.get_rhs(), /*positive*/ false);
    }

    // x++;    -->    X1 = X0 + 1
    void ConstraintBuilder::visit(const Nodecl::Postincrement& n)
    {
        visit_increment(n.get_rhs(), /*positive*/ true);
    }

    // --x;    -->    X1 = X0 - 1
    void ConstraintBuilder::visit(const Nodecl::Predecrement& n)
    {
        visit_increment(n.get_rhs(), /*positive*/ false);
    }

    // ++x;    -->    X1 = X0 + 1
    void ConstraintBuilder::visit(const Nodecl::Preincrement& n)
    {
        visit_increment(n.get_rhs(), /*positive*/ true);
    }

    // return x  -->  X1 = X0
    // We need this to avoid removing the constraint when purging unused constraints
    void ConstraintBuilder::visit(const Nodecl::ReturnStatement& n)
    {
        NBase rhs = n.get_value();
        Symbol ssa_sym(ssa_scope.new_symbol("return_sym"));
        ssa_sym.set_type(rhs.get_type());
        Nodecl::Symbol lhs = ssa_sym.make_nodecl(/*set_ref_type*/false);
        visit_assignment(lhs, rhs);
    }

    // ************** END visitor building constraints from statements ************* //
    // ***************************************************************************** //

}
}
