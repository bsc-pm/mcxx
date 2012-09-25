/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "tl-nodecl-utils.hpp"
#include "tl-nodecl-calc.hpp"
#include "tl-counters.hpp"
#include "tl-predicateutils.hpp"
#include "cxx-nodecl-deep-copy.h"
#include "cxx-utils.h"
#include <algorithm>

namespace Nodecl
{
    static void get_all_symbols_rec(Nodecl::NodeclBase n, TL::ObjectList<TL::Symbol>& result)
    {
        TL::ObjectList<TL::Symbol> sym;

        if (n.is_null())
            return;

        if (n.has_symbol())
        {
            result.insert(n.get_symbol());
        }

        TL::ObjectList<Nodecl::NodeclBase> children = n.children();

        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
                it != children.end();
                it++)
        {
            get_all_symbols_rec(*it, result);
        }
    }

    TL::ObjectList<TL::Symbol> Utils::get_all_symbols(Nodecl::NodeclBase n)
    {
        TL::ObjectList<TL::Symbol> sym_list;
        get_all_symbols_rec(n, sym_list);
        return sym_list;
    }

    static bool is_parameter_of_another_function(TL::Symbol symbol, TL::Scope sc)
    {
        // If this symbol is a parameter of some function but not from the
        // current one (if any), then it is a parameter of another function
        return (symbol.is_parameter_of_a_function()
                && sc.get_decl_context().current_scope->related_entry != NULL
                && !symbol.is_parameter_of(sc.get_decl_context().current_scope->related_entry));
    }

    struct IsLocalSymbol : TL::Predicate<TL::Symbol>
    {
        private:
            TL::Scope _sc;

        public:
            IsLocalSymbol(Nodecl::NodeclBase root)
                : _sc(root.retrieve_context())
            {
            }

            virtual bool do_(const TL::Symbol& sym) const
            {
                // If its scope is contained in the base node one, then it is
                // "local"
                return sym.get_scope().scope_is_enclosed_by(_sc)
                    && !is_parameter_of_another_function(sym, _sc);
            }
    };

    struct IsNonLocalSymbol : TL::Predicate<TL::Symbol>
    {
        private:
            TL::Scope _sc;

        public:
            IsNonLocalSymbol(Nodecl::NodeclBase root)
                : _sc(root.retrieve_context())
            {
            }

            virtual bool do_(const TL::Symbol& sym) const
            {
                // If its scope is not contained in the base node one, then it
                // is "nonlocal"
                return !sym.get_scope().scope_is_enclosed_by(_sc)
                    && !is_parameter_of_another_function(sym, _sc);
            }
    };

    TL::ObjectList<TL::Symbol> Utils::get_local_symbols(Nodecl::NodeclBase n)
    {
        IsLocalSymbol local(n);
        return get_all_symbols(n).filter(local);
    }

    TL::ObjectList<TL::Symbol> Utils::get_nonlocal_symbols(Nodecl::NodeclBase n)
    {
        IsNonLocalSymbol non_local(n);
        return get_all_symbols(n).filter(non_local);
    }

    static void get_all_symbols_occurrences_rec(Nodecl::NodeclBase n, TL::ObjectList<Nodecl::Symbol> &result)
    {
        if (n.is_null())
            return;

        if (n.is<Nodecl::Symbol>())
        {
            result.append(n.as<Nodecl::Symbol>());
        }

        TL::ObjectList<Nodecl::NodeclBase> children = n.children();

        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
                it != children.end();
                it++)
        {
            get_all_symbols_occurrences_rec(*it, result);
        }
    }

    TL::ObjectList<Nodecl::Symbol> Utils::get_all_symbols_occurrences(Nodecl::NodeclBase n)
    {
        TL::ObjectList<Nodecl::Symbol> result;
        get_all_symbols_occurrences_rec(n, result);
        return result;
    }

    struct IsLocalOcurrence : TL::Predicate<Nodecl::Symbol>
    {
        private:
            IsLocalSymbol _pred;

        public:
            IsLocalOcurrence(Nodecl::NodeclBase root)
                : _pred(root)
            {
            }

            virtual bool do_(const Nodecl::Symbol& n) const
            {
                return _pred(n.get_symbol());
            }
    };

    struct IsNonLocalOcurrence : TL::Predicate<Nodecl::Symbol>
    {
        private:
            IsNonLocalSymbol _pred;

        public:
            IsNonLocalOcurrence(Nodecl::NodeclBase root)
                : _pred(root)
            {
            }

            virtual bool do_(const Nodecl::Symbol& n) const
            {
                return _pred(n.get_symbol());
            }
    };

    TL::ObjectList<Nodecl::Symbol> Utils::get_local_symbols_occurrences(Nodecl::NodeclBase n)
    {
        IsLocalOcurrence local(n);
        return get_all_symbols_occurrences(n).filter(local);
    }

    TL::ObjectList<Nodecl::Symbol> Utils::get_nonlocal_symbols_occurrences(Nodecl::NodeclBase n)
    {
        IsNonLocalOcurrence local(n);
        return get_all_symbols_occurrences(n).filter(local);
    }

    static void get_all_symbols_first_occurrence_rec(Nodecl::NodeclBase n, TL::ObjectList<Nodecl::Symbol> &result)
    {
        if (n.is_null())
            return;

        if (n.is<Nodecl::Symbol>())
        {
            result.insert(n.as<Nodecl::Symbol>(),
                    TL::ThisMemberFunctionConstAdapter<TL::Symbol, Nodecl::Symbol>(&Nodecl::Symbol::get_symbol));
        }
        else if (n.is<Nodecl::ObjectInit>())
        {
            get_all_symbols_first_occurrence_rec(n.as<Nodecl::ObjectInit>().get_symbol().get_value(), result);
        }

        TL::ObjectList<Nodecl::NodeclBase> children = n.children();
        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
                it != children.end();
                it++)
        {
            get_all_symbols_first_occurrence_rec(*it, result);
        }
    }

    TL::ObjectList<Nodecl::Symbol> Utils::get_all_symbols_first_occurrence(Nodecl::NodeclBase n)
    {
        TL::ObjectList<Nodecl::Symbol> result;
        get_all_symbols_first_occurrence_rec(n, result);
        return result;
    }

    TL::ObjectList<Nodecl::Symbol> Utils::get_local_symbols_first_occurrence(Nodecl::NodeclBase n)
    {
        IsLocalOcurrence local(n);
        return get_all_symbols_first_occurrence(n).filter(local);
    }

    TL::ObjectList<Nodecl::Symbol> Utils::get_nonlocal_symbols_first_occurrence(Nodecl::NodeclBase n)
    {
        IsNonLocalOcurrence local(n);
        return get_all_symbols_first_occurrence(n).filter(local);
    }

    static bool equal_trees_rec(nodecl_t n1, nodecl_t n2)
    {
        if (nodecl_is_null(n1) == nodecl_is_null(n2))
        {
            if (!nodecl_is_null(n1))
            {
                if ((nodecl_get_kind(n1) == nodecl_get_kind(n2))
                    &&  (nodecl_get_symbol(n1) == nodecl_get_symbol(n2))
                    &&  (nodecl_get_constant(n1) == nodecl_get_constant(n2)))
                {
                    bool equal = true;

                    for (int i = 0; i < MCXX_MAX_AST_CHILDREN && equal; i++)
                    {
                        equal = equal_trees_rec(nodecl_get_child(n1, i), nodecl_get_child(n2, i));
                    }
                    return equal;
                }
            }
            else
            {
                return true;
            }
        }

        return false;
    }

    bool Utils::nodecl_is_arithmetic_op( Nodecl::NodeclBase n )
    {
        bool res = false;
        if ( n.is<Nodecl::Add>( ) || n.is<Nodecl::Minus>( )
            || n.is<Nodecl::Mul>( ) || n.is<Nodecl::Div>( )
            || n.is<Nodecl::Mod>( ) || n.is<Nodecl::Plus>( )
            || n.is<Nodecl::Preincrement>( ) || n.is<Nodecl::Postincrement>( )
            || n.is<Nodecl::Predecrement>( ) || n.is<Nodecl::Postdecrement>( )
            || Utils::nodecl_is_assignment_op( n ) || /* Fortran */ n.is<Nodecl::Power>( )
            || n.is<Nodecl::ArithmeticShr>( ) )
        {
            res = true;
        }
        return res;
    }

    bool Utils::nodecl_is_comparison_op( Nodecl::NodeclBase n )
    {
        bool res = false;
        if ( n.is<Nodecl::Equal>( ) || n.is<Nodecl::Different>( )
            || n.is<Nodecl::LowerThan>( ) || n.is<Nodecl::GreaterThan>( )
            || n.is<Nodecl::LowerOrEqualThan>( ) || n.is<Nodecl::GreaterOrEqualThan>( ) )
        {
            res = true;
        }
        return res;
    }

    bool Utils::nodecl_is_logical_op( Nodecl::NodeclBase n )
    {
        bool res = false;
        if ( n.is<Nodecl::LogicalAnd>( ) || n.is<Nodecl::LogicalOr>( )
            || n.is<Nodecl::LogicalNot>( ) )
        {
            res = true;
        }
        return res;
    }

    bool Utils::nodecl_is_bitwise_op( Nodecl::NodeclBase n )
    {
        bool res = false;
        if ( n.is<Nodecl::BitwiseAnd>( ) || n.is<Nodecl::BitwiseOr>( )
            || n.is<Nodecl::BitwiseXor>( ) || n.is<Nodecl::BitwiseNot>( )
            || n.is<Nodecl::BitwiseShr>( ) || n.is<Nodecl::BitwiseShl>( ))
        {
            res = true;
        }
        return res;
    }

    bool Utils::nodecl_is_assignment_op ( Nodecl::NodeclBase n )
    {
        bool res = false;
        if ( n.is<Nodecl::Assignment>( ) || n.is<Nodecl::AddAssignment>( )
            || n.is<Nodecl::MinusAssignment>( ) || n.is<Nodecl::DivAssignment>( )
            || n.is<Nodecl::MulAssignment>( ) || n.is<Nodecl::ModAssignment>( )
            || n.is<Nodecl::ArithmeticShrAssignment>( ) || n.is<Nodecl::BitwiseShrAssignment>( )
            || n.is<Nodecl::BitwiseShlAssignment>( ) || n.is<Nodecl::BitwiseAndAssignment>( )
            || n.is<Nodecl::BitwiseOrAssignment>( ) || n.is<Nodecl::BitwiseXorAssignment>( ) )
        {
            res = true;
        }
        return res;
    }

    bool Utils::nodecl_is_modifiable_lvalue( Nodecl::NodeclBase n )
    {
        return n.get_type().is_lvalue_reference( );
    }

    bool Utils::equal_nodecls(Nodecl::NodeclBase n1, Nodecl::NodeclBase n2)
    {
        nodecl_t n1_ = n1.get_internal_nodecl();
        nodecl_t n2_ = n2.get_internal_nodecl();

        if (nodecl_is_list(n1_) || nodecl_is_list(n2_))
        {
            std::cerr << "warning: method 'equal_nodecls' is implemented to compare nodecls containing trees with "
                      << " no lists inside. The method returns false but they can be the same tree" << std::endl;
            return false;
        }

        return equal_trees_rec(n1_, n2_);
    }

    size_t Utils::Nodecl_hash::operator() (const Nodecl::NodeclBase& n) const
    {
        return nodecl_hash_table(n.get_internal_nodecl());
    }

    bool Utils::Nodecl_comp::operator() (const Nodecl::NodeclBase& n1, const Nodecl::NodeclBase& n2) const
    {
        return equal_nodecls(n1, n2);
    }

    NodeclBase Utils::reduce_expression(NodeclBase n)
    {
        NodeclBase simplified_expr;
        if (n.is<Symbol>() || n.is<BooleanLiteral>() || n.is<StringLiteral>()
            || n.is<IntegerLiteral>() || n.is<FloatingLiteral>() || n.is<ComplexLiteral>()
            || n.is<Dereference>() || n.is<ClassMemberAccess>())
        {
            simplified_expr = n;
        }
        else if (n.is<List>())
        {
            List l = n.as<List>(); List new_l = List::make(NodeclBase::null());
            for (List::iterator it = l.begin(); it != l.end(); ++it)
            {
                new_l.push_back(reduce_expression(*it));
            }
            simplified_expr = new_l;
        }
        else if (n.is<Conversion>())
        {
            Conversion n_conv = n.as<Conversion>();
            NodeclBase new_conv = reduce_expression(n_conv.get_nest());
            if (!equal_nodecls(n_conv, new_conv))
            {
                n_conv = Conversion::make(new_conv, n.get_type(), n.get_filename(), n.get_line());
            }
            simplified_expr = algebraic_simplification(n_conv.get_nest());
        }
        else if (n.is<Add>())
        {
            Add n_add = n.as<Add>();
            NodeclBase lhs = n_add.get_lhs(); NodeclBase rhs = n_add.get_rhs();
            NodeclBase new_lhs = reduce_expression(lhs);
            NodeclBase new_rhs = reduce_expression(rhs);
            if (!equal_nodecls(new_lhs, lhs) || !equal_nodecls(new_rhs, rhs))
            {
                n = Add::make(new_lhs, new_rhs, lhs.get_type(), n.get_filename(), n.get_line());
            }
            simplified_expr = algebraic_simplification(n);
        }
        else if (n.is<Minus>())
        {
            Minus n_minus = n.as<Minus>();
            NodeclBase lhs = n_minus.get_lhs(); NodeclBase rhs = n_minus.get_rhs();
            NodeclBase new_lhs = reduce_expression(lhs);
            NodeclBase new_rhs = reduce_expression(rhs);
            if (!equal_nodecls(new_lhs, lhs) || !equal_nodecls(new_rhs, rhs))
            {
                n = Minus::make(new_lhs, new_rhs, lhs.get_type(), n.get_filename(), n.get_line());
            }
            simplified_expr = algebraic_simplification(n);
        }
        else if (n.is<Mul>())
        {
            Mul n_mul = n.as<Mul>();
            NodeclBase lhs = n_mul.get_lhs(); NodeclBase rhs = n_mul.get_rhs();
            NodeclBase new_lhs = reduce_expression(lhs);
            NodeclBase new_rhs = reduce_expression(rhs);
            if (!equal_nodecls(new_lhs, lhs) || !equal_nodecls(new_rhs, rhs))
            {
                n = Mul::make(new_lhs, new_rhs, lhs.get_type(), n.get_filename(), n.get_line());
            }
            simplified_expr = algebraic_simplification(n);
        }
        else if (n.is<Div>())
        {
            Div n_div = n.as<Div>();
            NodeclBase lhs = n_div.get_lhs(); NodeclBase rhs = n_div.get_rhs();
            NodeclBase new_lhs = reduce_expression(lhs);
            NodeclBase new_rhs = reduce_expression(rhs);
            if (!equal_nodecls(new_lhs, lhs) || !equal_nodecls(new_rhs, rhs))
            {
                n = Div::make(new_lhs, new_rhs, lhs.get_type(), n.get_filename(), n.get_line());
            }
            simplified_expr = algebraic_simplification(n);
        }
        else if (n.is<Mod>())
        {
            Mod n_mod = n.as<Mod>();
            NodeclBase lhs = n_mod.get_lhs(); NodeclBase rhs = n_mod.get_rhs();
            NodeclBase new_lhs = reduce_expression(lhs);
            NodeclBase new_rhs = reduce_expression(rhs);
            if (!equal_nodecls(new_lhs, lhs) || !equal_nodecls(new_rhs, rhs))
            {
                n = Div::make(new_lhs, new_rhs, lhs.get_type(), n.get_filename(), n.get_line());
            }
            simplified_expr = algebraic_simplification(n);
        }
        else if (n.is<LowerOrEqualThan>())
        {
            LowerOrEqualThan n_low_eq = n.as<LowerOrEqualThan>();
            NodeclBase lhs = n_low_eq.get_lhs(); NodeclBase rhs = n_low_eq.get_rhs();
            NodeclBase new_lhs = reduce_expression(lhs);
            NodeclBase new_rhs = reduce_expression(rhs);
            if (!equal_nodecls(new_lhs, lhs) || !equal_nodecls(new_rhs, rhs))
            {
                n = Minus::make(new_lhs, new_rhs, lhs.get_type(), n.get_filename(), n.get_line());
            }
            simplified_expr = algebraic_simplification(n);
        }
        else if (n.is<ArraySubscript>())
        {
            ArraySubscript array_subs = n.as<ArraySubscript>();
            NodeclBase subscripted = array_subs.get_subscripted(); NodeclBase subscripts = array_subs.get_subscripts();
            NodeclBase new_subscripted = reduce_expression(subscripted);
            NodeclBase new_subscripts = reduce_expression(subscripts);
            if (!equal_nodecls(new_subscripted, subscripted) || !equal_nodecls(new_subscripts, subscripts))
            {
                n = ArraySubscript::make(new_subscripted, new_subscripts, subscripted.get_type(), n.get_filename(), n.get_line());
            }
            simplified_expr = algebraic_simplification(n);
        }
        else
        {
            internal_error("Node type '%s' while simplifying algebraic expression '%s' not yet implemented",
                            ast_print_node_type(n.get_kind()), n.prettyprint().c_str());
        }

        return simplified_expr;
    }

    /*!
     * This method must be called in pre-order form the bottom of a tree expression
     *
     * R1 :   +                                     R3 :    -
     *      /   \          =>     c1 + c2                 /   \     =>    c1 - c2
     *    c1    c2                                      c1    c2
     *
     * R2 :   +                       +             R4      -                +
     *      /   \          =>       /   \                 /   \     =>     /   \
     *     t    c                  c     t               t    c          -c     t
     *
     * R5 :   -
     *      /   \               =>    0
     *     t1   t2 , t1 = t2
     *
     * R6 :       +                    +
     *          /   \               /     \
     *         +    c2     =>    c1+c2     t
     *       /   \
     *     c1    t
     *
     * R7 :   *                                     R8 :    *                 *
     *      /   \          =>     c1 * c2                 /   \     =>      /   \
     *    c1    c2                                       t    c            c     t
     *
     * R9 :       *                    *
     *          /   \               /     \
     *         *    c2     =>    c1*c2     t
     *       /   \
     *     c1    t
     *
     * R10 :  /
     *      /   \          =>     0
     *     0    c,  c != 0
     *
     * R11 :  %         %
     *      /   \  ,  /   \   =>  0
     *     t    1    t    t
     *
     * R20 :    <=                  <=
     *        /    \              /    \
     *       +     c2      =>    t   c2-c1
     *     /   \
     *   c1     t
     */
    NodeclBase Utils::algebraic_simplification(NodeclBase n)
    {
        NodeclBase result = n;
        Calculator calc;
        if (n.is<Add>())
        {
            Add n_add = n.as<Add>();
            NodeclBase lhs = n_add.get_lhs();
            NodeclBase rhs = n_add.get_rhs();
            if (lhs.is_constant() && const_value_is_zero(lhs.get_constant()))
            {   // 0 + t = t
                result = rhs;
            }
            else if (rhs.is_constant() && const_value_is_zero(rhs.get_constant()))
            {   // t + 0 = t
                result = lhs;
            }
            else if (lhs.is_constant() && rhs.is_constant())
            {   // R1
                const_value_t* const_value = calc.compute_const_value(n);
                result = const_value_to_nodecl(const_value);
            }
            else if (rhs.is_constant())
            {
                if (lhs.is<Add>())
                {   // R6
                    Add lhs_add = lhs.as<Add>();
                    NodeclBase lhs_lhs = lhs_add.get_lhs();
                    NodeclBase lhs_rhs = lhs_add.get_rhs();
                    if (lhs_lhs.is_constant())
                    {
                        NodeclBase const_node = Add::make(lhs_lhs, rhs, rhs.get_type(), n.get_filename(), n.get_line());
                        const_value_t* const_value = calc.compute_const_value(const_node);
                        if (!const_value_is_zero(const_value))
                        {
                            result = Add::make(const_value_to_nodecl(const_value), lhs_rhs,
                                               rhs.get_type(), n.get_filename(), n.get_line());
                        }
                        else
                        {
                            result = lhs_rhs;
                        }
                    }
                }
                else
                {   // R2
                    result = Add::make(rhs, lhs, lhs.get_type(), n.get_filename(), n.get_line());
                }
            }
        }
        else if (n.is<Minus>())
        {
            Minus n_minus = n.as<Minus>();
            NodeclBase lhs = n_minus.get_lhs();
            NodeclBase rhs = n_minus.get_rhs();
            if (lhs.is_constant() && rhs.is_constant())
            {   // R3
                Calculator calc;
                const_value_t* const_value = calc.compute_const_value(n);
                result = const_value_to_nodecl(const_value);
            }
            else if (rhs.is_constant())
            {
                Nodecl::NodeclBase zero = const_value_to_nodecl(const_value_get_zero(/*num_bytes*/ 4, /*sign*/1));
                NodeclBase neg_rhs = Minus::make(zero, rhs, rhs.get_type(),
                                                 n.get_filename(), n.get_line());
                const_value_t* const_value = calc.compute_const_value(neg_rhs);
                if (!const_value_is_zero(const_value))
                {
                    result = Add::make(const_value_to_nodecl(const_value), lhs, lhs.get_type(), n.get_filename(), n.get_line());
                }
                else
                {
                    result = lhs;
                }
            }
            else if (equal_nodecls(lhs, rhs))
            {
                result = const_value_to_nodecl(const_value_get_zero(/*num_bytes*/ 4, /*sign*/1));
            }
        }
        else if (n.is<Mul>())
        {
            Mul n_mul = n.as<Mul>();
            NodeclBase lhs = n_mul.get_lhs();
            NodeclBase rhs = n_mul.get_rhs();
            if (lhs.is_constant() && const_value_is_zero(lhs.get_constant()))
            {   // 0 + t = t
                result = const_value_to_nodecl(const_value_get_zero(/*num_bytes*/ 4, /*sign*/1));
            }
            else if (rhs.is_constant() && const_value_is_zero(rhs.get_constant()))
            {   // t + 0 = t
                result = lhs;
            }
            else if (lhs.is_constant() && rhs.is_constant())
            {   // R7
                const_value_t* const_value = calc.compute_const_value(n);
                result = const_value_to_nodecl(const_value);
            }
            else if (rhs.is_constant())
            {
                if (lhs.is<Mul>())
                {   // R9
                    Mul lhs_mul = lhs.as<Mul>();
                    NodeclBase lhs_lhs = lhs_mul.get_lhs();
                    NodeclBase lhs_rhs = lhs_mul.get_rhs();
                    if (lhs_lhs.is_constant())
                    {
                        NodeclBase const_node = Mul::make(lhs_lhs, rhs, rhs.get_type(), n.get_filename(), n.get_line());
                        const_value_t* const_value = calc.compute_const_value(const_node);
                        if (!const_value_is_zero(const_value))
                        {
                            result = Mul::make(const_value_to_nodecl(const_value), lhs_rhs,
                                               rhs.get_type(), n.get_filename(), n.get_line());
                        }
                        else
                        {
                            result = const_value_to_nodecl(const_value_get_zero(/*num_bytes*/ 4, /*sign*/1));
                        }
                    }
                }
                else
                {   // R8
                    result = Mul::make(rhs, lhs, lhs.get_type(), n.get_filename(), n.get_line());
                }
            }
        }
        else if (n.is<Div>())
        {   // R10
            Div n_div = n.as<Div>();
            NodeclBase lhs = n_div.get_lhs();
            NodeclBase rhs = n_div.get_rhs();
            if (lhs.is_constant() && rhs.is_constant() && const_value_is_zero(lhs.get_constant()) && !const_value_is_zero(rhs.get_constant()))
            {
                result = const_value_to_nodecl(const_value_get_zero(/*num_bytes*/ 4, /*sign*/1));
            }
        }
        else if (n.is<Mod>())
        {
            Mod n_mod = n.as<Mod>();
            NodeclBase lhs = n_mod.get_lhs();
            NodeclBase rhs = n_mod.get_rhs();
            if ( (rhs.is_constant() && (lhs.is_constant() && const_value_is_one(lhs.get_constant())))
                 || equal_nodecls(lhs, rhs))
            {   // R12
                result = const_value_to_nodecl(const_value_get_zero(/*num_bytes*/ 4, /*sign*/1));
            }
        }
        else if (n.is<LowerOrEqualThan>())
        {
            LowerOrEqualThan n_low_eq = n.as<LowerOrEqualThan>();
            NodeclBase lhs = n_low_eq.get_lhs();
            NodeclBase rhs = n_low_eq.get_rhs();
            if (rhs.is_constant())
            {
                if (lhs.is<Add>())
                {   // R20
                    Add lhs_add = lhs.as<Add>();
                    NodeclBase lhs_lhs = lhs_add.get_lhs();
                    NodeclBase lhs_rhs = lhs_add.get_rhs();
                    if (lhs_lhs.is_constant())
                    {
                        NodeclBase const_node = Minus::make(rhs, lhs_lhs, rhs.get_type(), n.get_filename(), n.get_line());
                        const_value_t* const_value = calc.compute_const_value(const_node);
                        result = LowerOrEqualThan::make(lhs_rhs, const_value_to_nodecl(const_value),
                                                        rhs.get_type(), n.get_filename(), n.get_line());
                    }
                }
            }
        }
        else if (n.is<IntegerLiteral>())
        {
            result = n;
        }
        else
        {
            internal_error("Node type '%s' while simplifying algebraic expressions not yet implemented", ast_print_node_type(n.get_kind()));
        }

        DEBUG_CODE()
        {
            std::cerr << "=== Algebraic Simplification '" << n.prettyprint() << "' --> '"
                      << result.prettyprint() << "' ===" << std::endl;
        }

        return result;
    }

    Nodecl::List Utils::get_all_list_from_list_node(Nodecl::List n)
    {
        while (n.get_parent().is<Nodecl::List>())
        {
            n = n.get_parent().as<Nodecl::List>();
        }

        return n;
    }

    void Utils::remove_from_enclosing_list(Nodecl::NodeclBase n)
    {
        Nodecl::NodeclBase parent = n.get_parent();

        if (!parent.is<Nodecl::List>())
            return;

        Nodecl::List l = Utils::get_all_list_from_list_node(parent.as<Nodecl::List>());

        Nodecl::List::iterator it = std::find(l.begin(), l.end(), n);

        if (it != l.end())
        {
            l.erase(it);
        }
    }

    TL::Symbol Utils::get_enclosing_function(Nodecl::NodeclBase n)
    {
        TL::Symbol result;
        TL::Scope sc = n.retrieve_context();

        decl_context_t decl_context = sc.get_decl_context();

        if (decl_context.block_scope != NULL)
        {
            result = decl_context.block_scope->related_entry;
        }
        else if (decl_context.function_scope != NULL)
        {
            result = decl_context.function_scope->related_entry;
        }

        return result;
    }

    void Utils::append_to_top_level_nodecl(Nodecl::NodeclBase n)
    {
        if (n.is<Nodecl::List>())
        {
            Nodecl::List l = n.as<Nodecl::List>();
            for (Nodecl::List::iterator it = l.begin();
                    it != l.end();
                    it++)
            {
                append_to_top_level_nodecl(*it);
            }
        }
        else
        {
            Nodecl::TopLevel top_level = Nodecl::NodeclBase(CURRENT_COMPILED_FILE->nodecl).as<Nodecl::TopLevel>();
            Nodecl::List list = top_level.get_top_level().as<Nodecl::List>();
            list.push_back(n);
        }
    }

    namespace
    {
        void simple_replace(Nodecl::NodeclBase dest, Nodecl::NodeclBase src)
        {
            // Simple case
            Nodecl::NodeclBase nodecl_original_parent = dest.get_parent();
            ::nodecl_replace(dest.get_internal_nodecl(), src.get_internal_nodecl());

            // Reparent new children of dest
            ::nodecl_set_parent(dest.get_internal_nodecl(), nodecl_original_parent.get_internal_nodecl());
            for (int i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
            {
                nodecl_t child = nodecl_get_child(dest.get_internal_nodecl(), i);
                if (!nodecl_is_null(child))
                {
                    ::nodecl_set_parent(nodecl_get_child(dest.get_internal_nodecl(), i), dest.get_internal_nodecl());
                }
            }
        }
    }

    void Utils::replace(Nodecl::NodeclBase dest, Nodecl::NodeclBase src)
    {
        ERROR_CONDITION(src.is_null(), "Invalid node", 0);

        if (src.is<Nodecl::List>()
                && !dest.is<Nodecl::List>())
        {
            Nodecl::List new_list = src.as<Nodecl::List>();
            List::iterator new_list_it = new_list.begin();

            if (new_list.size() == 1)
            {
                simple_replace(dest, *new_list_it);
            }
            else
            {
                ERROR_CONDITION(!dest.is_in_list(), "Cannot replace a non-list node by a list if the first is not inside a list", 0);

                simple_replace(dest, *new_list_it);
                new_list_it++;

                Nodecl::List parent_list = dest.get_parent().as<Nodecl::List>();
                Nodecl::List::iterator last_it = parent_list.last();

                for (; new_list_it != new_list.end(); new_list_it++)
                {
                    parent_list.insert(last_it + 1, *new_list_it);
                    // We may have a new last node now
                    last_it = new_list_it->get_parent().as<Nodecl::List>().last();
                }
            }
        }
        else
        {
            simple_replace(dest, src);
        }
    }

    bool Utils::is_in_list(Nodecl::NodeclBase n)
    {
        return (!n.get_parent().is_null()
                && n.get_parent().is<Nodecl::List>());
    }

    void Utils::prepend_items_before(Nodecl::NodeclBase n, Nodecl::NodeclBase items)
    {
        ERROR_CONDITION(!Utils::is_in_list(n), "Node is not in a list", 0);

        if (!items.is<Nodecl::List>())
        {
            items = Nodecl::List::make(items);
        }

        Nodecl::List list_items = items.as<Nodecl::List>();

        Nodecl::List list = n.get_parent().as<Nodecl::List>();
        Nodecl::List::iterator last_it = list.last();

        for (Nodecl::List::iterator it = list_items.begin();
                it != list_items.end();
                it++)
        {
            list.insert(last_it, *it);
            // We may have a new last node now
            last_it = it->get_parent().as<Nodecl::List>().last();
        }
    }

    void Utils::append_items_after(Nodecl::NodeclBase n, Nodecl::NodeclBase items)
    {
        ERROR_CONDITION(!Utils::is_in_list(n), "Node is not in a list", 0);

        if (!items.is<Nodecl::List>())
        {
            items = Nodecl::List::make(items);
        }

        Nodecl::List list_items = items.as<Nodecl::List>();

        Nodecl::List list = n.get_parent().as<Nodecl::List>();
        Nodecl::List::iterator last_it = list.last();

        for (Nodecl::List::iterator it = list_items.begin();
                it != list_items.end();
                it++)
        {
            list.insert(last_it + 1, *it);
            // We may have a new last node now
            last_it = it->get_parent().as<Nodecl::List>().last();
        }
    }

    void Utils::prepend_to_top_level_nodecl(Nodecl::NodeclBase n)
    {
        if (n.is<Nodecl::List>())
        {
            Nodecl::List l = n.as<Nodecl::List>();
            for (Nodecl::List::iterator it = l.begin();
                    it != l.end();
                    it++)
            {
                prepend_to_top_level_nodecl(*it);
            }
        }
        else
        {
            Nodecl::TopLevel top_level = Nodecl::NodeclBase(CURRENT_COMPILED_FILE->nodecl).as<Nodecl::TopLevel>();
            Nodecl::List list = top_level.get_top_level().as<Nodecl::List>();
            list.push_front(n);
        }
    }

    Nodecl::NodeclBase Utils::advance_conversions(Nodecl::NodeclBase n)
    {
        while (n.is<Nodecl::Conversion>())
        {
            n = n.as<Nodecl::Conversion>().get_nest();
        }
        return n;
    }

    std::string Utils::get_elemental_operator_of_binary_expression(Nodecl::NodeclBase n)
    {
        return get_elemental_operator_of_binary_expression(n.get_kind());
    }

    std::string Utils::get_elemental_operator_of_binary_expression(node_t n)
    {
        switch (n)
        {
            case NODECL_ADD:
            case NODECL_ADD_ASSIGNMENT:
                return "+";
            case NODECL_MINUS:
            case NODECL_MINUS_ASSIGNMENT:
                return "-";
            case NODECL_MUL:
            case NODECL_MUL_ASSIGNMENT:
                return "*";
            case NODECL_DIV:
            case NODECL_DIV_ASSIGNMENT:
                return "/";
            case NODECL_MOD:
            case NODECL_MOD_ASSIGNMENT:
                return "%";
            case NODECL_BITWISE_SHL:
            case NODECL_BITWISE_SHL_ASSIGNMENT:
                return "<<";
            case NODECL_BITWISE_SHR:
            case NODECL_BITWISE_SHR_ASSIGNMENT:
            case NODECL_ARITHMETIC_SHR:
            case NODECL_ARITHMETIC_SHR_ASSIGNMENT:
                return ">>";
            case NODECL_BITWISE_AND:
            case NODECL_BITWISE_AND_ASSIGNMENT:
                return "&";
            case NODECL_BITWISE_OR:
            case NODECL_BITWISE_OR_ASSIGNMENT:
                return "|";
            case NODECL_BITWISE_XOR:
            case NODECL_BITWISE_XOR_ASSIGNMENT:
                return "^";
            default:
                return "";
        }
    }

    Nodecl::NodeclBase Utils::deep_copy(Nodecl::NodeclBase orig, TL::ReferenceScope ref_scope, Utils::SymbolMap& map)
    {
        Nodecl::NodeclBase result;

        result = ::nodecl_deep_copy(orig.get_internal_nodecl(),
                ref_scope.get_scope().get_decl_context(),
                map.get_symbol_map());

        return result;
    }

    Nodecl::NodeclBase Utils::deep_copy(Nodecl::NodeclBase orig, TL::ReferenceScope ref_scope)
    {
        Utils::SimpleSymbolMap empty_map;
        return deep_copy(orig, ref_scope, empty_map);
    }

    namespace
    {
        bool is_in_top_level_list(Nodecl::NodeclBase list)
        {
            ERROR_CONDITION(!list.is<Nodecl::List>(), "Must be a list", 0);
            list = Nodecl::Utils::get_all_list_from_list_node(list.as<Nodecl::List>());

            Nodecl::TopLevel top_level = Nodecl::NodeclBase(CURRENT_COMPILED_FILE->nodecl).as<Nodecl::TopLevel>();
            Nodecl::List top_level_list = top_level.get_top_level().as<Nodecl::List>();

            return (list == top_level_list);
        }
    }

    void Utils::prepend_to_enclosing_top_level_location(Nodecl::NodeclBase current_location, Nodecl::NodeclBase n)
    {
        while (!current_location.is_null()
                && (!current_location.is<Nodecl::List>()
                || !is_in_top_level_list(current_location)))
        {
            current_location = current_location.get_parent();
        }

        ERROR_CONDITION(current_location.is_null(), "This should never be null", 0);

        // This is a list node inside the top level list
        Nodecl::List list = current_location.as<Nodecl::List>();

        Nodecl::List::iterator it = list.last();
        list.insert(it, n);
    }

    void Utils::append_to_enclosing_top_level_location(Nodecl::NodeclBase current_location, Nodecl::NodeclBase n)
    {
        while (!current_location.is_null()
                && (!current_location.is<Nodecl::List>()
                || !is_in_top_level_list(current_location)))
        {
            current_location = current_location.get_parent();
        }

        ERROR_CONDITION(current_location.is_null(), "This should never be null", 0);

        // This is a list node inside the top level list
        Nodecl::List list = current_location.as<Nodecl::List>();

        Nodecl::List::iterator it = list.last();
        list.insert(it+1, n);
    }

    TL::ObjectList<Nodecl::NodeclBase> Utils::get_declarations_of_entity_at_top_level(TL::Symbol symbol)
    {
        TL::ObjectList<Nodecl::NodeclBase> result;
        Nodecl::TopLevel top_level = Nodecl::NodeclBase(CURRENT_COMPILED_FILE->nodecl).as<Nodecl::TopLevel>();
        Nodecl::List list = top_level.get_top_level().as<Nodecl::List>();
        for (Nodecl::List::iterator it = list.begin();
                it != list.end();
                it++)
        {
            if (it->is<Nodecl::CxxDecl>()
                    && it->get_symbol() == symbol)
            {
                result.append(*it);
            }
        }
        return result;
    }

    TL::ObjectList<Nodecl::NodeclBase> Utils::get_definitions_of_entity_at_top_level(TL::Symbol symbol)
    {
        TL::ObjectList<Nodecl::NodeclBase> result;
        Nodecl::TopLevel top_level = Nodecl::NodeclBase(CURRENT_COMPILED_FILE->nodecl).as<Nodecl::TopLevel>();
        Nodecl::List list = top_level.get_top_level().as<Nodecl::List>();
        for (Nodecl::List::iterator it = list.begin();
                it != list.end();
                it++)
        {
            if (it->is<Nodecl::CxxDef>()
                    && it->get_symbol() == symbol)
            {
                result.append(*it);
            }
        }
        return result;
    }

    TL::ObjectList<Nodecl::NodeclBase> Utils::get_declarations_or_definitions_of_entity_at_top_level(TL::Symbol symbol)
    {
        TL::ObjectList<Nodecl::NodeclBase> result;
        Nodecl::TopLevel top_level = Nodecl::NodeclBase(CURRENT_COMPILED_FILE->nodecl).as<Nodecl::TopLevel>();
        Nodecl::List list = top_level.get_top_level().as<Nodecl::List>();
        for (Nodecl::List::iterator it = list.begin();
                it != list.end();
                it++)
        {
            if ((it->is<Nodecl::CxxDef>()
                        || it->is<Nodecl::CxxDecl>())
                    && it->get_symbol() == symbol)
            {
                result.append(*it);
            }
        }
        return result;
    }

    struct LabelVisitor : ExhaustiveVisitor<void>
    {
        Utils::SimpleSymbolMap &_symbol_map;
        TL::Scope _sc;
        LabelVisitor(Utils::SimpleSymbolMap& symbol_map, TL::ReferenceScope ref_scope)
            : _symbol_map(symbol_map), _sc(ref_scope.get_scope()) { }

        void insert_new_label_symbol(TL::Symbol sym)
        {
            TL::Counter &counter = TL::CounterManager::get_counter("label_visitor");

            std::string register_name, symbol_name;
            if (IS_FORTRAN_LANGUAGE)
            {
                std::string label_name = sym.get_name();

                for (std::string::iterator it = label_name.begin(); it != label_name.end(); it++)
                {
                    // If this is not a numeric label give up
                    if (!(('0' <= (*it)) &&
                                ((*it) <= '9')))
                    {
                        std::cerr << "NOT A NUMERIC LABEL! |" << label_name << "|" << std::endl;
                        return;
                    }
                }

                int x;
                {
                    std::stringstream ss;
                    ss.str(label_name);

                    ss >> x;
                }

                // FIXME - Make this more robust!
                // Add 10000 to this label
                x += 10000 + (int)counter;

                std::stringstream ss;
                ss << x;

                symbol_name = ss.str();
                register_name = ".label_" + symbol_name;
            }
            else
            {
                std::stringstream ss;
                ss << symbol_name << "_" << (int)counter;

                symbol_name = ss.str();
                register_name = symbol_name;
            }
            counter++;

            decl_context_t decl_context = _sc.get_decl_context();
            scope_entry_t * new_label = ::new_symbol(decl_context, decl_context.function_scope, uniquestr(register_name.c_str()));
            new_label->symbol_name = uniquestr(symbol_name.c_str());
            new_label->kind = SK_LABEL;
            new_label->value = nodecl_shallow_copy(sym.get_value().get_internal_nodecl());

            _symbol_map.add_map(sym, new_label);
        }

        void visit(const Nodecl::Symbol &node)
        {
            TL::Symbol sym = node.get_symbol();
            // FORMAT references
            if (IS_FORTRAN_LANGUAGE
                    && sym.is_label()
                    && !sym.get_value().is_null())
            {
                insert_new_label_symbol(sym);
            }
        }

        void visit(const Nodecl::LabeledStatement& stmt)
        {
            walk(stmt.get_statement());

            TL::Symbol sym = stmt.get_symbol();
            if (sym.is_label())
            {
                insert_new_label_symbol(sym);
            }
        }
    };

    Utils::LabelSymbolMap::LabelSymbolMap(
            Utils::SymbolMap* original_symbol_map, 
            Nodecl::NodeclBase code,
            TL::ReferenceScope ref_scope)
        : _orig_symbol_map(original_symbol_map)
    {
        LabelVisitor visitor(_current_map, ref_scope);
        visitor.walk(code);
    }

    void Utils::update_symbols(Nodecl::NodeclBase node, SymbolMap& m)
    {
        if (node.is_null())
            return;

        TL::Symbol sym = node.get_symbol();
        if (!sym.is_valid())
            return;

        node.set_symbol(m.map(sym));

        TL::ObjectList<Nodecl::NodeclBase> children = node.children();
        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
                it != children.end();
                it++)
        {
            update_symbols(*it, m);
        }


    }
}

namespace TL
{
    // This is actually what OpenMP expects
    // Lower bound and upper bound are closed ranges:
    //      [lower_bound, upper_bound] if step is positive
    //      [upper_bound, lower_bound] if step is negative
    void ForStatement::analyze_loop_header()
    {
        Nodecl::NodeclBase lc = this->get_loop_header();
        if (lc.is<Nodecl::RangeLoopControl>())
        {
            // This is trivially true for ranged loops
            Nodecl::RangeLoopControl loop_control = lc.as<Nodecl::RangeLoopControl>();

            // Empty loops are obviously not allowed
            if (loop_control.get_lower().is_null())
            {
                _is_omp_valid = false;
                return;
            }

            _induction_var = loop_control.get_symbol();
            _lower_bound = loop_control.get_lower().shallow_copy();
            _upper_bound = loop_control.get_upper().shallow_copy();
            _step = loop_control.get_step().shallow_copy();

            _is_omp_valid = true;
        }
        else if (lc.is<Nodecl::LoopControl>())
        {
            Nodecl::LoopControl loop_control = lc.as<Nodecl::LoopControl>();
            Nodecl::NodeclBase init_expr = loop_control.get_init();
            Nodecl::NodeclBase test_expr = loop_control.get_cond();
            Nodecl::NodeclBase incr_expr = loop_control.get_next();

            // init-expr must have the following form
            //
            //   _induction_var = lb
            //   integer-type   _induction_var = lb
            //   random-access-iterator _induction_var = lb    // CURRENTLY NOT SUPPORTED
            //   pointer-type _induction_var = lb

            _induction_var = TL::Symbol(NULL);

            // _induction_var = lb
            if (init_expr.is<Nodecl::Assignment>())
            {
                Nodecl::NodeclBase lhs = init_expr.as<Nodecl::Assignment>().get_lhs();
                if (lhs.is<Nodecl::Symbol>())
                {
                    _induction_var = lhs.get_symbol();
                }

                Nodecl::NodeclBase rhs = init_expr.as<Nodecl::Assignment>().get_rhs();
                _lower_bound = rhs.shallow_copy();
            }
            // T _induction_var = lb
            else if (init_expr.is<Nodecl::ObjectInit>())
            {
                _induction_var = init_expr.get_symbol();

                _lower_bound = _induction_var.get_value().shallow_copy();
            }
            else
            {
                _is_omp_valid = false;
                return;
            }

            if (!_induction_var.is_valid())
            {
                _is_omp_valid = false;
                return;
            }

            // test-expr must be
            //
            // _induction_var relational-op b
            // b relational-op _induction_var
            if ((test_expr.is<Nodecl::LowerThan>()
                        || test_expr.is<Nodecl::LowerOrEqualThan>()
                        || test_expr.is<Nodecl::GreaterThan>()
                        || test_expr.is<Nodecl::GreaterOrEqualThan>())
                    && (Nodecl::Utils::advance_conversions(test_expr.as<Nodecl::LowerThan>().get_lhs()).get_symbol() == _induction_var
                        || Nodecl::Utils::advance_conversions(test_expr.as<Nodecl::LowerThan>().get_rhs()).get_symbol() == _induction_var))

            {
                Nodecl::NodeclBase lhs = test_expr.as<Nodecl::LowerThan>().get_lhs();
                Nodecl::NodeclBase rhs = test_expr.as<Nodecl::LowerThan>().get_rhs();

                bool lhs_is_var = (Nodecl::Utils::advance_conversions(lhs).get_symbol() == _induction_var);

                if (test_expr.is<Nodecl::LowerThan>())
                {
                    if (lhs_is_var)
                    {
                        // x < E  this is like x <= (E - 1)
                        TL::Type t = lhs.get_type();

                        if (t.is_any_reference())
                            t = t.references_to();

                        if (rhs.is_constant())
                        {
                            _upper_bound = const_value_to_nodecl(
                                    const_value_sub(
                                        rhs.get_constant(),
                                        const_value_get_one(4, 1)));
                        }
                        else
                        {
                            _upper_bound = Nodecl::Minus::make(
                                    rhs.shallow_copy(),
                                    const_value_to_nodecl(const_value_get_one(4, 1)),
                                    t,
                                    rhs.get_filename(),
                                    rhs.get_line());
                        }
                    }
                    else
                    {
                        // E < x this is like x > E this is like x >= E + 1
                        TL::Type t = lhs.get_type();

                        if (t.is_any_reference())
                            t = t.references_to();

                        if (lhs.is_constant())
                        {
                            _upper_bound = const_value_to_nodecl(
                                    const_value_add(
                                        lhs.get_constant(),
                                        const_value_get_one(4, 1)));
                        }
                        else
                        {
                            _upper_bound = Nodecl::Add::make(
                                    lhs.shallow_copy(),
                                    const_value_to_nodecl(const_value_get_one(4, 1)),
                                    t,
                                    lhs.get_filename(),
                                    lhs.get_line());
                        }
                    }
                }
                else if (test_expr.is<Nodecl::LowerOrEqualThan>())
                {
                    if (lhs_is_var)
                    {
                        // x <= E
                        _upper_bound = rhs.shallow_copy();
                    }
                    else
                    {
                        // E <= x this is like x >= E
                        _upper_bound = lhs.shallow_copy();
                    }
                }
                else if (test_expr.is<Nodecl::GreaterThan>())
                {
                    if (lhs_is_var)
                    {
                        // x > E, this is like x >= E + 1
                        TL::Type t = rhs.get_type();

                        if (t.is_any_reference())
                            t = t.references_to();


                        if (rhs.is_constant())
                        {
                            _upper_bound = const_value_to_nodecl(
                                    const_value_add(
                                        rhs.get_constant(),
                                        const_value_get_one(4, 1)));
                        }
                        else
                        {
                            _upper_bound = Nodecl::Add::make(
                                    rhs.shallow_copy(),
                                    const_value_to_nodecl(const_value_get_one(4, 1)),
                                    t,
                                    rhs.get_filename(),
                                    rhs.get_line());
                        }
                    }
                    else
                    {
                        // E > x this is like x < E, this is like x <= E - 1
                        TL::Type t = lhs.get_type();

                        if (t.is_any_reference())
                            t = t.references_to();

                        if (lhs.is_constant())
                        {
                            _upper_bound = const_value_to_nodecl(
                                    const_value_sub(
                                        lhs.get_constant(),
                                        const_value_get_one(4, 1)));
                        }
                        else
                        {
                            _upper_bound = Nodecl::Minus::make(
                                    lhs.shallow_copy(),
                                    const_value_to_nodecl(const_value_get_one(4, 1)),
                                    t,
                                    lhs.get_filename(),
                                    lhs.get_line());
                        }
                    }
                }
                else if (test_expr.is<Nodecl::GreaterOrEqualThan>())
                {
                    if (lhs_is_var)
                    {
                        // x >= E
                        _upper_bound = rhs.shallow_copy();
                    }
                    else
                    {
                        // E >= x this is like x <= E
                        _upper_bound = lhs.shallow_copy();
                    }
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }
            }
            else
            {
                _is_omp_valid = false;
                return;
            }

            // incr-expr must have the following form
            // ++_induction_var
            if (incr_expr.is<Nodecl::Preincrement>()
                    && incr_expr.as<Nodecl::Preincrement>().get_rhs().get_symbol() == _induction_var)
            {
                _step = const_value_to_nodecl(const_value_get_one(4, 1));
            }
            // _induction_var++
            else if (incr_expr.is<Nodecl::Postincrement>()
                    && incr_expr.as<Nodecl::Postincrement>().get_rhs().get_symbol() == _induction_var)
            {
                _step = const_value_to_nodecl(const_value_get_one(4, 1));
            }
            // --_induction_var
            else if (incr_expr.is<Nodecl::Predecrement>()
                    && incr_expr.as<Nodecl::Predecrement>().get_rhs().get_symbol() == _induction_var)
            {
                _step = const_value_to_nodecl(const_value_get_minus_one(4, 1));
            }
            // _induction_var--
            else if (incr_expr.is<Nodecl::Postdecrement>()
                    && incr_expr.as<Nodecl::Postdecrement>().get_rhs().get_symbol() == _induction_var)
            {
                _step = const_value_to_nodecl(const_value_get_minus_one(4, 1));
            }
            // _induction_var += incr
            else if (incr_expr.is<Nodecl::AddAssignment>()
                    && incr_expr.as<Nodecl::AddAssignment>().get_lhs().get_symbol() == _induction_var)
            {
                _step = incr_expr.as<Nodecl::AddAssignment>().get_rhs().shallow_copy();
            }
            // _induction_var -= incr
            else if (incr_expr.is<Nodecl::MinusAssignment>()
                    && incr_expr.as<Nodecl::MinusAssignment>().get_lhs().get_symbol() == _induction_var)
            {
                Nodecl::NodeclBase rhs = incr_expr.as<Nodecl::AddAssignment>().get_rhs();

                TL::Type t = incr_expr.as<Nodecl::AddAssignment>().get_rhs().get_type();

                if (t.is_any_reference())
                    t = t.references_to();

                if (rhs.is_constant())
                {
                    _step = const_value_to_nodecl(const_value_neg(rhs.get_constant()));
                }
                else
                {
                    _step = Nodecl::Neg::make(
                            rhs,
                            t,
                            rhs.get_filename(),
                            rhs.get_line());
                }
            }
            // _induction_var = _induction_var + incr
            else if (incr_expr.is<Nodecl::Assignment>()
                    && incr_expr.as<Nodecl::Assignment>().get_lhs().get_symbol() == _induction_var
                    && incr_expr.as<Nodecl::Assignment>().get_rhs().is<Nodecl::Add>()
                    && incr_expr.as<Nodecl::Assignment>().get_rhs().as<Nodecl::Add>().get_lhs().get_symbol() == _induction_var)
            {
                _step = incr_expr.as<Nodecl::Assignment>().get_rhs().as<Nodecl::Add>().get_rhs().shallow_copy();
            }
            // _induction_var = incr + _induction_var
            else if (incr_expr.is<Nodecl::Assignment>()
                    && incr_expr.as<Nodecl::Assignment>().get_lhs().get_symbol() == _induction_var
                    && incr_expr.as<Nodecl::Assignment>().get_rhs().is<Nodecl::Add>()
                    && incr_expr.as<Nodecl::Assignment>().get_rhs().as<Nodecl::Add>().get_rhs().get_symbol() == _induction_var)
            {
                _step = incr_expr.as<Nodecl::Assignment>().get_rhs().as<Nodecl::Add>().get_lhs().shallow_copy();
            }
            // _induction_var = _induction_var - incr
            else if (incr_expr.is<Nodecl::Assignment>()
                    && incr_expr.as<Nodecl::Assignment>().get_lhs().get_symbol() == _induction_var
                    && incr_expr.as<Nodecl::Assignment>().get_rhs().is<Nodecl::Minus>()
                    && incr_expr.as<Nodecl::Assignment>().get_rhs().as<Nodecl::Minus>().get_lhs().get_symbol() == _induction_var)
            {
                Nodecl::NodeclBase rhs = incr_expr.as<Nodecl::Assignment>().get_rhs().as<Nodecl::Minus>().get_rhs();

                TL::Type t = rhs.get_type();

                if (t.is_any_reference())
                    t = t.references_to();

                if (rhs.is_constant())
                {
                    _step = const_value_to_nodecl(
                            const_value_neg(rhs.get_constant()));
                }
                else
                {
                    _step = Nodecl::Neg::make(
                            rhs.shallow_copy(),
                            t,
                            rhs.get_filename(),
                            rhs.get_line());
                }
            }
            else
            {
                _is_omp_valid = false;
                return;
            }
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        _is_omp_valid = true;
    }

    bool ForStatement::is_omp_valid_loop() const
    {
        return _is_omp_valid;
    }

    TL::Symbol ForStatement::get_induction_variable() const
    {
        return _induction_var;
    }

    Nodecl::NodeclBase ForStatement::get_lower_bound() const
    {
        return _lower_bound;
    }

    Nodecl::NodeclBase ForStatement::get_upper_bound() const
    {
        return _upper_bound;
    }

    Nodecl::NodeclBase ForStatement::get_step() const
    {
        return _step;
    }
}
