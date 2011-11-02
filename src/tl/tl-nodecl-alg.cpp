#include "tl-nodecl-alg.hpp"
#include "tl-nodecl-calc.hpp"
#include "tl-predicateutils.hpp"
#include "cxx-utils.h"

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
                return sym.get_scope().is_contained_in(_sc);
            }
    };

    TL::ObjectList<TL::Symbol> Utils::get_local_symbols(Nodecl::NodeclBase n)
    {
        IsLocalSymbol local(n);
        return get_all_symbols(n).filter(local);
    }

    TL::ObjectList<TL::Symbol> Utils::get_nonlocal_symbols(Nodecl::NodeclBase n)
    {
        IsLocalSymbol local(n);
        return get_all_symbols(n).filter(negate(local));
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
            TL::Scope _sc;

        public:
            IsLocalOcurrence(Nodecl::NodeclBase root)
                : _sc(root.retrieve_context())
            {
            }

            virtual bool do_(const Nodecl::Symbol& n) const
            {
                // If its scope is contained in the base node one, then it is
                // "local"
                return n.get_symbol().get_scope().is_contained_in(_sc);
            }
    };

    TL::ObjectList<Nodecl::Symbol> Utils::get_local_symbols_occurrences(Nodecl::NodeclBase n)
    {
        IsLocalOcurrence local(n);
        return get_all_symbols_occurrences(n).filter(local);
    }

    TL::ObjectList<Nodecl::Symbol> Utils::get_nonlocal_symbols_occurrences(Nodecl::NodeclBase n)
    {
        IsLocalOcurrence local(n);
        return get_all_symbols_occurrences(n).filter(negate(local));
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
        IsLocalOcurrence local(n);
        return get_all_symbols_first_occurrence(n).filter(negate(local));
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
        return hash_table(n.get_internal_nodecl());
    }
    
    bool Utils::Nodecl_comp::operator() (const Nodecl::NodeclBase& n1, const Nodecl::NodeclBase& n2) const
    {
        return equal_nodecls(n1, n2);
    }
    
    NodeclBase Utils::reduce_expression(NodeclBase n)
    {
        NodeclBase simplified_expr;
        if (n.is<Symbol>() || n.is<IntegerLiteral>() || n.is<FloatingLiteral>())
        {
            return n;
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
        else
        {
            if (n.is<Add>())
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
            else if (n.is<Derreference>())
            {   // It cannot be a simplificable expression
                simplified_expr = n;
            }
            else
            {
                internal_error("Node type '%s' while simplifying algebraic expression '%s' not yet implemented", 
                               ast_print_node_type(n.get_kind()), n.prettyprint().c_str());
            }
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
}

namespace TL
{
    bool ForStatement::is_regular_loop() const
    {
        internal_error("Not yet implemented", 0);
    }

    TL::Symbol ForStatement::get_induction_variable() const
    {
        internal_error("Not yet implemented", 0);
    }
}
