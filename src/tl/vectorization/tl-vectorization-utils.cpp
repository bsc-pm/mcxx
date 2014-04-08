/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include "tl-vectorization-utils.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-cexpr.h"

namespace TL
{
namespace Vectorization
{
namespace Utils
{
    static unsigned int _var_counter = 0;

    LookForReturnVisitor::LookForReturnVisitor()
    {
    }

    bool LookForReturnVisitor::join_list(ObjectList<bool>& list)
    {
        for(ObjectList<bool>::const_iterator it = list.begin();
                it != list.end();
                it++)
        {
            if ((*it) == true)
                return true;
        }

        return false;
    }

    bool LookForReturnVisitor::visit(const Nodecl::ReturnStatement& n)
    {
        return true;
    }

    MaskCheckCostEstimation::MaskCheckCostEstimation()
        :
            _add_cost(1),
            _minus_cost(1),
            _mul_cost(3),
            _div_cost(5),
            _return_cost(3),
            _if_statement_cost(4),
            _else_statement_cost(1),
            _static_for_statement_cost(10),
            _masked_for_statement_cost(3),
            _function_call_cost(1000),
            _nesting_threshold(0),
            _nesting_level(0)
    {
    }

    unsigned int MaskCheckCostEstimation::get_mask_check_cost(
            const Nodecl::NodeclBase& n, unsigned int initial_cost,
            const unsigned int cost_threshold)
    {
        if (initial_cost < cost_threshold)
            _cost = initial_cost;
        else
            _cost = 0;

        this->walk(n);

        return _cost;
    }

    void MaskCheckCostEstimation::binary_operation(const Nodecl::NodeclBase& n,
            const unsigned int cost)
    {
        _cost += cost;
        walk(n.as<Nodecl::Add>().get_lhs());
        walk(n.as<Nodecl::Add>().get_rhs());
    }

    void MaskCheckCostEstimation::visit(const Nodecl::Add& n)
    {
        binary_operation(n, _add_cost);
    }

    void MaskCheckCostEstimation::visit(const Nodecl::Minus& n)
    {
        binary_operation(n, _minus_cost);
    }

    void MaskCheckCostEstimation::visit(const Nodecl::Mul& n)
    {
        binary_operation(n, _mul_cost);
    }

    void MaskCheckCostEstimation::visit(const Nodecl::Div& n)
    {
        binary_operation(n, _div_cost);
    }

    void MaskCheckCostEstimation::visit(const Nodecl::ReturnStatement& n)
    {
        _cost += _return_cost;
        walk(n.get_value());
    }

    void MaskCheckCostEstimation::visit(const Nodecl::IfElseStatement& n)
    {
        _cost += _if_statement_cost + _else_statement_cost;

        if (_nesting_level < _nesting_threshold)
        {
            _nesting_level++;
            walk(n.get_then());
            _nesting_level--;

            if(!n.get_else().is_null())
            {
                _nesting_level++;
                walk(n.get_else());
                _nesting_level--;
            }
        }
    }

    void MaskCheckCostEstimation::visit(const Nodecl::ForStatement& n)
    {
        _cost += _static_for_statement_cost;

        if (_nesting_level < _nesting_threshold)
        {
            _nesting_level++;
            walk(n.get_statement());
            _nesting_level--;
        }
    }

    void MaskCheckCostEstimation::visit(const Nodecl::FunctionCall& n)
    {
        _cost += _function_call_cost;
    }

    Nodecl::NodeclBase get_new_mask_symbol(const Nodecl::NodeclBase& n,
            const int mask_size,
            const bool ref_type)
    {
        TL::Scope scope;

        if (n.is<Nodecl::FunctionCode>())
        {
            scope = n.as<Nodecl::FunctionCode>().get_statements().
                retrieve_context();
        }
        else if (n.is<Nodecl::ForStatement>())
        {
            scope = n.as<Nodecl::ForStatement>().get_parent().get_parent().
                get_parent().get_parent().retrieve_context();
        }
        else
        {
            std::cerr << "---" << std::endl;
            std::cerr << n.prettyprint() << std::endl;

            running_error("get_new_mask_symbol needs FunctionCode or ForStatement");
        }
           
        TL::Symbol new_mask_sym = scope.new_symbol("__mask_" +
                Utils::get_var_counter());
        new_mask_sym.get_internal_symbol()->kind = SK_VARIABLE;
        new_mask_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
        new_mask_sym.set_type(TL::Type::get_mask_type(mask_size));

        return new_mask_sym.make_nodecl(ref_type, make_locus("", 0, 0));
    }

    Nodecl::NodeclBase get_disjunction_mask(
            const ObjectList<Nodecl::NodeclBase>& bb_exit_mask_list,
            Nodecl::List& output_stmt_list,
            const Nodecl::NodeclBase& scope,
            const int mask_size)
    {
        ObjectList<Nodecl::NodeclBase>::const_iterator it = bb_exit_mask_list.begin();

        Nodecl::NodeclBase lhs = *it;
        it++;

        while(it != bb_exit_mask_list.end())
        {
            Nodecl::NodeclBase new_mask_sym_nodecl = get_new_mask_symbol(
                    scope, mask_size, /* ref_type */ true);

            Nodecl::ExpressionStatement new_mask_exp =
                Nodecl::ExpressionStatement::make(
                        Nodecl::VectorMaskAssignment::make(
                            new_mask_sym_nodecl.shallow_copy(),
                            Nodecl::VectorMaskOr::make(
                                lhs.shallow_copy(),
                                it->shallow_copy(),
                                lhs.get_type(),
                                make_locus("", 0, 0)),
                            lhs.get_type(),
                            make_locus("", 0, 0)));

            output_stmt_list.append(new_mask_exp);

            lhs = new_mask_sym_nodecl;
            it++;
        }

        return lhs;
    }
/*
    bool is_declared_in_scope(const scope_t *const  target_scope,
            const scope_t *const symbol_scope)
    {
        if (symbol_scope == NULL)
            return false;
        else if (target_scope == NULL)
            return false;
        else if (target_scope == symbol_scope)
            return true;
        else
        {
            return false;
        }
    }
*/
    bool is_declared_in_inner_scope(const Nodecl::NodeclBase& enclosing_node,
            const TL::Symbol& tl_symbol) 
    {
        scope_t* enclosing_scope = enclosing_node.retrieve_context().
            get_decl_context().current_scope;
 
        scope_t* symbol_scope = tl_symbol.get_scope().
            get_decl_context().current_scope;
        
        if (symbol_scope == NULL)
            return false;
        else if (enclosing_scope == NULL)
            return false;
        else if (scope_is_enclosed_by(symbol_scope, enclosing_scope))
        {
            if(symbol_scope == enclosing_scope)
                return false;
            return true;
        }
        else
        {
            return false;
            //internal_error("Vectorizer: is_declared_in_inner_scope. Scope is no enclosed", 0);
        }
    }

    bool is_all_one_mask(const Nodecl::NodeclBase& n)
    {
        if (n.is<Nodecl::MaskLiteral>())
        {
            Nodecl::MaskLiteral ml = n.as<Nodecl::MaskLiteral>();

            if (ml.is_constant())
            {
                if (const_value_is_minus_one(ml.get_constant()))
                    return true;
            }
        }

        return false;
    }

    Nodecl::NodeclBase get_proper_mask(const Nodecl::NodeclBase& mask)
    {
        if(Utils::is_all_one_mask(mask))
        {
            return Nodecl::NodeclBase::null();
        }
        else
        {
            return mask.shallow_copy();
        }
    }

    Nodecl::NodeclBase get_null_mask()
    {
        return Nodecl::NodeclBase::null();
    }

    TL::Type get_qualified_vector_to(TL::Type src_type, const unsigned int num_elements)
    {
        cv_qualifier_t cv_qualif = get_cv_qualifier(no_ref(src_type.get_internal_type()));
        TL::Type result_type = src_type.no_ref().get_unqualified_type().get_vector_of_elements(num_elements);

        result_type = get_cv_qualified_type(result_type.get_internal_type(), cv_qualif);

        if (src_type.is_lvalue_reference())
        {
            result_type = result_type.get_lvalue_reference_to();
        }

        return result_type;
    }

    std::string get_var_counter()
    {
        std::stringstream result;

        result << Utils::_var_counter;
        Utils::_var_counter++;

        return result.str();
    }

    Nodecl::NodeclBase get_if_mask_is_not_zero_nodecl(const Nodecl::NodeclBase& mask,
            const Nodecl::NodeclBase& then)
    {
        Nodecl::NodeclBase processed_then;

        if (then.is<Nodecl::List>())
            processed_then = then;
        else
            processed_then = Nodecl::List::make(then);


        // Create IF to check if if_mask is all zero
        Nodecl::IfElseStatement if_mask_is_zero =
            Nodecl::IfElseStatement::make(
                    Nodecl::Different::make(
                        mask,
                        Nodecl::IntegerLiteral::make(TL::Type::get_int_type(),
                            const_value_get_zero(4, 0),
                            mask.get_locus()),
                        TL::Type::get_bool_type(),
                        mask.get_locus()),
                    processed_then,
                    Nodecl::NodeclBase::null());

        return if_mask_is_zero;
    }

    Nodecl::NodeclBase get_dimension_offset(const TL::Type& type)
    {
        if (!type.is_array())
        {
            internal_error("GDO: type is not an array", 0);
        }

        if (!type.array_has_size())
        {
            internal_error("GDO: array has no size", 0);
        }

        Nodecl::NodeclBase array_size = type.array_get_size();

        if(!type.array_element().is_array()) // Last Dimension
        {
            return array_size.shallow_copy();
        }
        else
        {
            Nodecl::Mul result = Nodecl::Mul::make(
                    array_size.shallow_copy(),
                    get_dimension_offset(type.array_element()),
                    array_size.get_type());

            return result;
        }
    }

    Nodecl::MaskLiteral get_contiguous_mask_literal(const int size, const int num_active_lanes)
    {
        if (num_active_lanes == 0)
        {
            return Nodecl::MaskLiteral::make(
                    TL::Type::get_mask_type(size),
                    const_value_get_zero(size, 1));
        }

        if ( size == num_active_lanes)
        {
            return Nodecl::MaskLiteral::make(
                    TL::Type::get_mask_type(size),
                    const_value_get_minus_one(size, 1));
        }

        const_value_t* mask_value;

        if (size == 16)
        {
            unsigned short int value =
                ~(((signed short int)0x8000) >> (15 - ((unsigned short int) num_active_lanes)));

            mask_value = const_value_get_integer(value, 2, 0);
        }
        else if (size == 8)
        {
            unsigned char value =
                ~(((signed char)0x80) >> (7 - ((unsigned char) num_active_lanes)));

            mask_value = const_value_get_integer(value, 1, 0);
        }
        else
        {
            internal_error("Vectorization Utils: Unsupported mask size", 0);
        }

        return Nodecl::MaskLiteral::make(
                TL::Type::get_mask_type(size),
                mask_value);
    }

    Nodecl::List get_vector_offset_list(const int start_value, const int increment, const int vector_size)
    {
        TL::ObjectList<Nodecl::NodeclBase> literal_list;

        const_value_t* i = const_value_get_signed_int(start_value);
        const_value_t* c_increment = const_value_get_signed_int(increment);
        for(int j = 0;
                j < vector_size;
                i = const_value_add(i, c_increment), j++)
        {
            literal_list.prepend(const_value_to_nodecl(i));
        }


        Nodecl::List offset_list = Nodecl::List::make(literal_list);
        offset_list.set_constant(get_vector_const_value(literal_list));

        return offset_list;
    }

    const_value_t* get_vector_const_value(const TL::ObjectList<Nodecl::NodeclBase>& list)
    {
        int size = list.size();
        ERROR_CONDITION(size == 0, "Invalid number of items in vector value", 0);

        const_value_t** value_set = new const_value_t*[size];

        int i = 0;
        for (TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = list.begin();
                it != list.end();
                it++, i++)
        {
            value_set[i] = it->get_constant();
            ERROR_CONDITION(value_set[i] == NULL, "Invalid constant", 0);
        }

        const_value_t* result = const_value_make_vector(size, value_set);

        delete[] value_set;

        return result;
    }

    Nodecl::NodeclBase get_denormalize_ub(Nodecl::ForStatement for_statement)
    {
        Nodecl::NodeclBase loop_condition = for_statement.get_loop_header().
            as<Nodecl::LoopControl>().get_cond();

        TL::ForStatement tl_for(for_statement);
        Nodecl::NodeclBase ub = tl_for.get_upper_bound();

        Nodecl::NodeclBase result;

        // UB is normalized. < --> <=
        if (loop_condition.is<Nodecl::LowerThan>() ||
                loop_condition.is<Nodecl::LowerOrEqualThan>())
        {
            result = Nodecl::Add::make(ub.shallow_copy(),
                    Nodecl::IntegerLiteral::make(
                        TL::Type::get_int_type(),
                        const_value_get_one(4, 1)),
                    ub.get_type());

            if (ub.is_constant())
            {
                result.set_constant(const_value_add(ub.get_constant(),
                            const_value_get_one(4, 1)));
            }
        }
        else
        {
            internal_error("Vectorizer: get_denormalize_ub. Unsupported condition", 0);
        }

        return result;
    }
}
}
}


