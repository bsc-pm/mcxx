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

#include <limits>

#include "cxx-cexpr.h"
#include "tl-ranges-common.hpp"

#include <climits>

namespace TL {
namespace Analysis {
namespace Utils {
    
namespace {
    const_value_t* one = const_value_get_one(/*bytes*/ 4, /*signed*/ 1);
    
    NBase get_max(const NBase& n1, const NBase& n2)
    {
        NBase result;
        if(n1.is<Nodecl::Analysis::PlusInfinity>())
        {
            result = n1;
        }
        else if(n2.is<Nodecl::Analysis::PlusInfinity>())
        {
            result = n2;
        }
        else if(n1.is<Nodecl::Analysis::MinusInfinity>())
        {
            result = n2;
        }
        else if(n2.is<Nodecl::Analysis::MinusInfinity>())
        {
            result = n1;
        }
        else if(n1.is_constant() && n2.is_constant())
        {
            const_value_t* n1_c = n1.get_constant();
            const_value_t* n2_c = n2.get_constant();
            if(const_value_is_positive(const_value_sub(n1_c, n2_c)))
                result = n1;
            else
                result = n2;
        }
        else
        {
            Nodecl::List exprs = Nodecl::List::make(n1, n2);
            result = Nodecl::Analysis::Maximum::make(exprs, n1.get_type());
        }
        return result;
    }
    
    NBase get_min(const NBase& n1, const NBase& n2)
    {
        NBase result;
        if(n1.is<Nodecl::Analysis::PlusInfinity>())
        {
            result = n2;
        }
        else if(n2.is<Nodecl::Analysis::PlusInfinity>())
        {
            result = n1;
        }
        else if(n1.is<Nodecl::Analysis::MinusInfinity>())
        {
            result = n1;
        }
        else if(n2.is<Nodecl::Analysis::MinusInfinity>())
        {
            result = n2;
        }
        else if(n1.is_constant() && n2.is_constant())
        {
            const_value_t* n1_c = n1.get_constant();
            const_value_t* n2_c = n2.get_constant();
            if(const_value_is_positive(const_value_sub(n1_c, n2_c)))
                result = n2;
            else
                result = n1;
        }
        else
        {
            Nodecl::List exprs = Nodecl::List::make(n1, n2);
            result = Nodecl::Analysis::Minimum::make(exprs, n1.get_type());
        }
        return result;
    }
}

    bool nodecl_is_Z_range(const NBase& n)
    {
        if (n.is<Nodecl::Range>())
        {
            const NBase& lb = n.as<Nodecl::Range>().get_lower();
            const NBase& ub = n.as<Nodecl::Range>().get_upper();
            if (lb.is<Nodecl::Analysis::MinusInfinity>()
                && ub.is<Nodecl::Analysis::PlusInfinity>())
                return true;
        }
        return false;
    }

    NBase range_addition(const NBase& r1, const NBase& r2)
    {
        if(!r1.is<Nodecl::Range>() || !r2.is<Nodecl::Range>())
        {
            return Nodecl::Add::make(r1, r2, r1.get_type());
        }
        
        NBase result;
        
        NBase r1_lb = r1.as<Nodecl::Range>().get_lower();
        NBase r1_ub = r1.as<Nodecl::Range>().get_upper();
        NBase r2_lb = r2.as<Nodecl::Range>().get_lower();
        NBase r2_ub = r2.as<Nodecl::Range>().get_upper();
        TL::Type t = r1_lb.get_type();
        
        NBase lb, ub;
        if(r1_lb.is_constant() && r2_lb.is_constant())
            lb = NBase(const_value_to_nodecl(const_value_add(r1_lb.get_constant(), r2_lb.get_constant())));
        else
            lb = Nodecl::Add::make(r1_lb, r2_lb, t);
        if(r1_ub.is_constant() && r2_ub.is_constant())
            ub = NBase(const_value_to_nodecl(const_value_add(r1_ub.get_constant(), r2_ub.get_constant())));
        else
            ub = Nodecl::Add::make(r1_ub, r2_ub, t);
        
        NBase one_nodecl = NBase(const_value_to_nodecl(one));
        result = Nodecl::Range::make(lb, ub, one_nodecl, t);

#ifdef RANGES_DEBUG
        std::cerr << "        Range Addition " << r1.prettyprint() << " - " << r2.prettyprint()
                  << " = " << result.prettyprint() << std::endl;
#endif

        return result;
    }
    
    NBase range_subtraction(const NBase& r1, const NBase& r2)
    {
        if(!r1.is<Nodecl::Range>() || !r2.is<Nodecl::Range>())
        {
            return Nodecl::Minus::make(r1, r2, r1.get_type());
        }
        
        NBase result;
        
        NBase r1_lb = r1.as<Nodecl::Range>().get_lower();
        NBase r1_ub = r1.as<Nodecl::Range>().get_upper();
        NBase r2_lb = r2.as<Nodecl::Range>().get_lower();
        NBase r2_ub = r2.as<Nodecl::Range>().get_upper();
        TL::Type t = r1_lb.get_type();
        
        NBase lb, ub;
        if (r1_lb.is_constant() && r2_lb.is_constant())
            lb = NBase(const_value_to_nodecl(const_value_sub(r1_lb.get_constant(), r2_lb.get_constant())));
        else if (r1_lb.is<Nodecl::Analysis::MinusInfinity>() && r2_lb.is<Nodecl::Analysis::MinusInfinity>())
            lb = r1_lb;
        else
            lb = Nodecl::Minus::make(r1_lb, r2_lb, t);
        if (r1_ub.is_constant() && r2_ub.is_constant())
            ub = NBase(const_value_to_nodecl(const_value_sub(r1_ub.get_constant(), r2_ub.get_constant())));
        else if (r1_ub.is<Nodecl::Analysis::PlusInfinity>() && r2_ub.is<Nodecl::Analysis::PlusInfinity>())
            ub = r1_ub;
        else
            ub = Nodecl::Minus::make(r1_ub, r2_ub, t);
        
        NBase one_nodecl = NBase(const_value_to_nodecl(one));
        result = Nodecl::Range::make(lb, ub, one_nodecl, t);

#ifdef RANGES_DEBUG
        std::cerr << "        Range Subtraction " << r1.prettyprint() << " - " << r2.prettyprint()
                  << " = " << result.prettyprint() << std::endl;
#endif

        return result;
    }
    
    
    // A[al, au] − B[bl, bu] =  ∅, A and B completely overlap or B contains A
    //                          A, A and B do not overlap
    //                          [al, bl] U [bu, au], A contains B
    //                          [al, bl], al < bl
    //                          [au, bu], al > bl
    // When the operation cannot be computed, we return the nodecl representing the operation
    NBase range_sub(const NBase& r1, const NBase& r2)
    {
        ERROR_CONDITION((!r1.is<Nodecl::Range>() || !r2.is<Nodecl::Range>()), 
                        "range_sub operation can only be applied to ranges, but parameters are '%s' and '%s'.\n", 
                        ast_print_node_type(r1.get_kind()), ast_print_node_type(r2.get_kind()));
        
        NBase result;
        
        NBase r1_lb = r1.as<Nodecl::Range>().get_lower();
        NBase r1_ub = r1.as<Nodecl::Range>().get_upper();
        NBase r2_lb = r2.as<Nodecl::Range>().get_lower();
        NBase r2_ub = r2.as<Nodecl::Range>().get_upper();
        TL::Type t = r1_lb.get_type();
        
        if (Nodecl::Utils::structurally_equal_nodecls(r1_lb, r2_lb)
                && Nodecl::Utils::structurally_equal_nodecls(r1_ub, r2_ub))
        {   // The two ranges are exactly the same
            result = Nodecl::Analysis::EmptyRange::make();
        }
        else if (r1_lb.is_constant() && r1_ub.is_constant() && r2_lb.is_constant() && r2_ub.is_constant())
        {   // Let's compare the constant values
            const_value_t* r1_lb_c = const_value_cast_to_signed_int_value(r1_lb.get_constant());
            const_value_t* r1_ub_c = const_value_cast_to_signed_int_value(r1_ub.get_constant());
            const_value_t* r2_lb_c = const_value_cast_to_signed_int_value(r2_lb.get_constant());
            const_value_t* r2_ub_c = const_value_cast_to_signed_int_value(r2_ub.get_constant());
            
            if( const_value_is_positive(const_value_sub(r1_lb_c, r2_ub_c)) || 
                const_value_is_positive(const_value_sub(r2_lb_c, r1_ub_c)) )
            {   // r1 and r2 do not overlap
                result = r1.shallow_copy();
            }
            else
            {
                const_value_t* lb_dif = const_value_sub( r1_lb_c, r2_lb_c );
                const_value_t* ub_dif = const_value_sub( r1_ub_c, r2_ub_c );
                if(const_value_is_zero(lb_dif) && const_value_is_zero(ub_dif))
                {   // r1 and r2 are the same range
                    result = Nodecl::Analysis::EmptyRange::make();
                }
                else 
                {
                    NBase one_nodecl = NBase(const_value_to_nodecl(one));
                    if((const_value_is_positive(lb_dif) || const_value_is_zero(lb_dif)) && 
                       (const_value_is_negative(ub_dif) || const_value_is_zero(ub_dif)))
                    {   // r2 contains r1
                        result = Nodecl::Analysis::EmptyRange::make();
                    }
                    else if((const_value_is_negative(lb_dif) || const_value_is_zero(lb_dif)) && 
                            (const_value_is_positive(ub_dif) || const_value_is_zero(ub_dif)))
                    {   // r2 is contained in r1
                        const_value_t* new_left_ub_c = const_value_sub(r2_lb_c, one);
                        Nodecl::Range left_range = 
                                Nodecl::Range::make(r1_lb.shallow_copy(), NBase(const_value_to_nodecl(new_left_ub_c)), 
                                                    one_nodecl.shallow_copy(), t);
                        const_value_t* new_right_lb_c = const_value_add(r2_ub_c, one);
                        Nodecl::Range right_range = 
                                Nodecl::Range::make(NBase(const_value_to_nodecl(new_right_lb_c)), r1_ub.shallow_copy(), 
                                                    one_nodecl.shallow_copy(), t);
                        if(const_value_is_zero(lb_dif))
                            result = right_range;
                        else if(const_value_is_zero(ub_dif))
                            result = left_range;
                        else
                            result = Nodecl::Analysis::RangeUnion::make( left_range, right_range, t );
                    }
                    else
                    {   // r1 and r2 overlap at some point
                        NBase lb, ub;
                        if(const_value_is_positive(lb_dif))
                        {   // lower bound is the same in both ranges
                            lb = NBase(const_value_to_nodecl(const_value_add(r2_ub_c, one)));
                            ub = r1_ub.shallow_copy();
                        }
                        else
                        {   // r2 lower bound is smaller than r1
                            lb = r1_lb.shallow_copy();
                            ub = NBase(const_value_to_nodecl(const_value_sub(r2_lb_c, one)));
                        }
                        result = Nodecl::Range::make(lb, ub, one_nodecl.shallow_copy(), t);
                    }
                }
            }
        }
        else
        {   // We are not able to synthesize the result
            result = Nodecl::Analysis::RangeSub::make(r1, r2, t);
        }

#ifdef RANGES_DEBUG
        std::cerr << "        Range Sub " << r1.prettyprint() << " - " << r2.prettyprint()
                  << " = " << result.prettyprint() << std::endl;
#endif

        return result;
    }
    
    // A[al, au] ∩ B[bl, bu] = (al<bl<au || al<bu<au) ? [max(al, bl), min(au, bu)]      -> overlap
    //                                                : ∅
    NBase range_intersection(const NBase& n, const NBase& m, Utils::CycleDirection dir)
    {
        NBase result;
        // If one element is the empty range, then the intersection is that element
        if (n.is<Nodecl::Analysis::EmptyRange>())
            result = n;
        else if (m.is<Nodecl::Analysis::EmptyRange>())
            result = m;
        // If one element is the range [-inf, +inf], then the intersection is the other,
        // because it is certainly enclosed in [-inf, +inf]
        else if (nodecl_is_Z_range(n))
            result = m;
        else if (nodecl_is_Z_range(m))
            result = n;
        // If the situation is more complicated, try some simplification
        else if(n.is<Nodecl::Range>() && m.is<Nodecl::Analysis::RangeUnion>())
        {   // Try to intersect with one part or the other of the union
            Nodecl::Analysis::RangeUnion tmp = m.as<Nodecl::Analysis::RangeUnion>();
            NBase intersect_lhs = range_intersection(n, tmp.get_lhs(), dir);
            NBase intersect_rhs = range_intersection(n, tmp.get_rhs(), dir);
            if (intersect_lhs.is<Nodecl::Analysis::EmptyRange>())
                result = intersect_rhs;
            else if (intersect_rhs.is<Nodecl::Analysis::EmptyRange>())
                result = intersect_lhs;
            else
                result = Nodecl::Analysis::RangeIntersection::make(n, m, n.get_type());
        }
        // If we are intersecting something that is not a range, just create the intersection node and return it
        else if(!n.is<Nodecl::Range>() || !m.is<Nodecl::Range>())
            result = Nodecl::Analysis::RangeIntersection::make(n, m, n.get_type());

        // If some intersection has been computed, return it now!
        if (!result.is_null())
        {
#ifdef RANGES_DEBUG
            std::cerr << "        Range Intersection " << n.prettyprint() << " ∩ " << m.prettyprint()
                      << " (" << dir.get_direction_as_str() << ") = " << result.prettyprint() << std::endl;
#endif
            return result;
        }

        ERROR_CONDITION((dir._cycle_direction & Utils::CycleDirection::POSITIVE) && 
                        (dir._cycle_direction & Utils::CycleDirection::NEGATIVE), 
                        "Cannot resolve the intersection of two ranges when positive and negative paths appear simultaneously\n", 0);

        ERROR_CONDITION((!n.is<Nodecl::Range>() || !m.is<Nodecl::Range>()),
                        "range_intersection operation can only be applied to ranges at this point, but parameters are '%s' and '%s'.\n",
                        ast_print_node_type(n.get_kind()), ast_print_node_type(m.get_kind()));

        NBase lb_b = n.as<Nodecl::Range>().get_lower();
        NBase ub_b = n.as<Nodecl::Range>().get_upper();
        NBase lb_p = m.as<Nodecl::Range>().get_lower();
        NBase ub_p = m.as<Nodecl::Range>().get_upper();
        TL::Type t = lb_b.get_type();
        NBase one_nodecl = NBase(const_value_to_nodecl(one));
        NBase lb, ub;
        if(dir._cycle_direction & Utils::CycleDirection::POSITIVE)
        {
            lb = get_max(lb_b, lb_p);
            ub = (ub_p.is<Nodecl::Analysis::PlusInfinity>() ? ub_b : ub_p);
        }
        else if(dir._cycle_direction & Utils::CycleDirection::NEGATIVE)
        {
            lb = (lb_p.is<Nodecl::Analysis::MinusInfinity>() ? lb_b : lb_p);
            ub = get_min(ub_b, ub_p);
        }
        else
        {
            lb = get_max(lb_b, lb_p);
            ub = get_min(ub_b, ub_p);
        }

        if (lb.is_constant() && ub.is_constant() && 
            const_value_is_positive(const_value_sub(lb.get_constant(), ub.get_constant())))
        {   // Check whether the range is consistent
            result = Nodecl::Analysis::EmptyRange::make();
        }
        else
        {
            result = Nodecl::Range::make(lb, ub, one_nodecl, t);
        }

#ifdef RANGES_DEBUG
        std::cerr << "        Range Intersection " << n.prettyprint() << " ∩ " << m.prettyprint()
                  << " (" << dir.get_direction_as_str() << ") = " << result.prettyprint() << std::endl;
#endif

        return result;
    }

    // A[al, au] ∪ B[bl, bu] = (al<bl<au || al<bu<au) ? [min(al, bl), max(au, bu)]  -> overlap
    //                                                : [al, au] ∪ [bl, bu]         -> cannot synthesize the result as a unique range
    NBase range_union(const NBase& n, const NBase& m)
    {
        NBase result;
        // If one element is the empty range, then the union is the other element
        if (n.is<Nodecl::Analysis::EmptyRange>())
            result = m.shallow_copy();
        else if (m.is<Nodecl::Analysis::EmptyRange>())
            result = n.shallow_copy();
        // If n or m is [-inf, +inf], then the union is [-inf, +inf]
        else if (nodecl_is_Z_range(n))
            result = n.shallow_copy();
        else if (nodecl_is_Z_range(m))
            result = m.shallow_copy();
        // Try simple case when the lb_n == ub_m+1 or vice-versa
        if (n.is<Nodecl::Range>() && m.is<Nodecl::Analysis::RangeUnion>())
        {
            const NBase& lhs = m.as<Nodecl::Analysis::RangeUnion>().get_lhs();
            const NBase& rhs = m.as<Nodecl::Analysis::RangeUnion>().get_rhs();
            const NBase& lhs_union = range_union(n, lhs);
            result = range_union(lhs_union, rhs);
        }
        else if (n.is<Nodecl::Analysis::RangeUnion>() && m.is<Nodecl::Range>())
        {
            const NBase& lhs = n.as<Nodecl::Analysis::RangeUnion>().get_lhs();
            const NBase& rhs = n.as<Nodecl::Analysis::RangeUnion>().get_rhs();
            const NBase& lhs_union = range_union(m, lhs);
            result = range_union(lhs_union, rhs);
        }
        // If we are intersecting something that is not a range, just create the intersection node and return it
        else if (n.is<Nodecl::Analysis::RangeIntersection>() || n.is<Nodecl::Analysis::RangeUnion>() ||
            m.is<Nodecl::Analysis::RangeIntersection>() || m.is<Nodecl::Analysis::RangeUnion>())
        {
            result = Nodecl::Analysis::RangeUnion::make( n.shallow_copy(), m.shallow_copy(), n.get_type() );
        }
        // If some intersection has been computed, return it now!
        if (!result.is_null())
        {
#ifdef RANGES_DEBUG
            std::cerr << "        Range Union " << n.prettyprint() << " ∪ " << m.prettyprint() << " = " << result.prettyprint() << std::endl;
#endif
            return result;
        }
        
        ERROR_CONDITION((!n.is<Nodecl::Range>() || !m.is<Nodecl::Range>()),
                        "range_union operation can only be applied to ranges at this point, but parameters are '%s' and '%s'.\n",
                        ast_print_node_type(n.get_kind()), ast_print_node_type(m.get_kind()));

        NBase n_lb = n.as<Nodecl::Range>().get_lower();
        NBase n_ub = n.as<Nodecl::Range>().get_upper();
        NBase m_lb = m.as<Nodecl::Range>().get_lower();
        NBase m_ub = m.as<Nodecl::Range>().get_upper();
        TL::Type t = n_lb.get_type();

        if ((n_lb == m_lb) && (n_ub == m_ub))
        {   // The two ranges are exactly the same
            result = n.shallow_copy();
        }
        else
        {
            // FIXME We are not taking into account the type of the variables. Assuming integers
            // Try to operate with constant values
            const_value_t *n_lb_c=NULL, *n_ub_c=NULL, *m_lb_c=NULL, *m_ub_c=NULL;
            if (n_lb.is<Nodecl::Analysis::MinusInfinity>())
                n_lb_c = const_value_get_integer(INT_MIN, /*num_bytes*/4, /*sign*/1);
            else if (n_lb.is_constant())
                n_lb_c = n_lb.get_constant();

            if (m_lb.is<Nodecl::Analysis::MinusInfinity>())
                m_lb_c = const_value_get_integer(INT_MIN, /*num_bytes*/4, /*sign*/1);
            else if (m_lb.is_constant())
                m_lb_c = m_lb.get_constant();

            if (n_ub.is<Nodecl::Analysis::PlusInfinity>())
                n_ub_c = const_value_get_integer(INT_MAX, /*num_bytes*/4, /*sign*/1);
            else if (n_ub.is_constant())
                n_ub_c = n_ub.get_constant();

            if (m_ub.is<Nodecl::Analysis::PlusInfinity>())
                m_ub_c = const_value_get_integer(INT_MAX, /*num_bytes*/4, /*sign*/1);
            else if (m_ub.is_constant())
                m_ub_c = m_ub.get_constant();

            if ((n_lb_c!=NULL) && (n_ub_c!=NULL) && (m_lb_c!=NULL) && (m_ub_c!=NULL))
            {
                NBase one_nodecl(const_value_to_nodecl(one));
                if (const_value_is_positive(const_value_sub(n_lb_c, m_ub_c))
                        || const_value_is_positive(const_value_sub(m_lb_c, n_ub_c)))
                {   // n and m do not overlap
                    if (t.is_integral_type())
                    {   // If the boundaries are contiguous, we can still merge the ranges
                        if (const_value_is_one(const_value_sub(n_lb_c, m_ub_c)))
                        {
                            result = Nodecl::Range::make(m_lb.shallow_copy(), n_ub.shallow_copy(), one_nodecl, t);
                        }
                        else if (const_value_is_one(const_value_sub(m_lb_c, n_ub_c)))
                        {
                            result = Nodecl::Range::make(n_lb.shallow_copy(), m_ub.shallow_copy(), one_nodecl, t);
                        }
                        else
                        {
                            result = Nodecl::Analysis::RangeUnion::make( n.shallow_copy(), m.shallow_copy(), t );
                        }
                    }
                    else
                    {
                        result = Nodecl::Analysis::RangeUnion::make( n.shallow_copy(), m.shallow_copy(), t );
                    }
                }
                else if (const_value_is_zero(const_value_sub(n_lb_c, m_lb_c))
                            && const_value_is_zero(const_value_sub(n_ub_c, m_ub_c)))
                {   // n and m are the same range
                    result = n.shallow_copy();
                }
                else
                {   // n and m overlap in some way
                    NBase lb = const_value_is_positive(const_value_sub(n_lb_c, m_lb_c)) ? m_lb.shallow_copy()
                                                                                        : n_lb.shallow_copy();
                    NBase ub = const_value_is_positive(const_value_sub(n_ub_c, m_ub_c)) ? n_ub.shallow_copy()
                                                                                        : m_ub.shallow_copy();
                    result = Nodecl::Range::make(lb, ub, one_nodecl, t);
                }
            }
            else
            {
                result = Nodecl::Analysis::RangeUnion::make(n.shallow_copy(), m.shallow_copy(), t);
            }
        }

#ifdef RANGES_DEBUG
        std::cerr << "        Range Union " << n.prettyprint() << " ∪ " << m.prettyprint() << " = " << result.prettyprint() << std::endl;
#endif
        return result;
    }

    Nodecl::Range range_value_add(const Nodecl::Range& r, const NBase& v)
    {
        NBase lb = r.get_lower();
        NBase ub = r.get_upper();
        Type t(lb.get_type());
        
        NBase new_lb, new_ub;
        // compute the lower bound
        if(lb.is<Nodecl::Analysis::MinusInfinity>() || lb.is<Nodecl::Analysis::PlusInfinity>())
            new_lb = lb;
        else if(lb.is_constant() && v.is_constant())
            new_lb = const_value_to_nodecl(const_value_add(lb.get_constant(), v.get_constant()));
        else
            new_lb = Nodecl::Add::make(lb, v, t);
        // compute the upper bound
        if(ub.is<Nodecl::Analysis::MinusInfinity>() || ub.is<Nodecl::Analysis::PlusInfinity>())
            new_ub = ub;
        else if(ub.is_constant() && v.is_constant())
            new_ub = const_value_to_nodecl(const_value_add(ub.get_constant(), v.get_constant()));
        else
            new_ub = Nodecl::Add::make(ub, v, t);
        
        Nodecl::Range result = Nodecl::Range::make(new_lb, new_ub, NBase(const_value_to_nodecl(one)), t);

#ifdef RANGES_DEBUG
        std::cerr << "        Range Value Add " << r.prettyprint() << " + " << v.prettyprint() << " = " << result.prettyprint() << std::endl;
#endif

        return result;
    }
    
    Nodecl::Range range_value_subtract(const Nodecl::Range& r, const NBase& v)
    {
        NBase lb = r.get_lower();
        NBase ub = r.get_upper();
        Type t(lb.get_type());
        
        NBase new_lb, new_ub;
        // compute the lower bound
        if(lb.is<Nodecl::Analysis::MinusInfinity>() || lb.is<Nodecl::Analysis::PlusInfinity>())
            new_lb = lb;
        else if(lb.is_constant() && v.is_constant())
            new_lb = const_value_to_nodecl(const_value_sub(lb.get_constant(), v.get_constant()));
        else
            new_lb = Nodecl::Minus::make(lb, v, t);
        // compute the upper bound
        if(ub.is<Nodecl::Analysis::MinusInfinity>() || ub.is<Nodecl::Analysis::PlusInfinity>())
            new_ub = ub;
        else if(ub.is_constant() && v.is_constant())
            new_ub = const_value_to_nodecl(const_value_sub(ub.get_constant(), v.get_constant()));
        else
            new_ub = Nodecl::Minus::make(ub, v, t);
        
        Nodecl::Range result = Nodecl::Range::make(new_lb, new_ub, NBase(const_value_to_nodecl(one)), t);

#ifdef RANGES_DEBUG
        std::cerr << "        Range Value Subtract " << r.prettyprint() << " - " << v.prettyprint()
                  << " = " << result.prettyprint() << std::endl;
#endif

        return result;
    }
    
    Nodecl::Range range_value_div(const Nodecl::Range& r, const NBase& v)
    {
        NBase lb = r.get_lower();
        NBase ub = r.get_upper();
        Type t(lb.get_type());

        NBase new_lb, new_ub;
        // compute the lower bound
        if (lb.is<Nodecl::Analysis::MinusInfinity>() || lb.is<Nodecl::Analysis::PlusInfinity>())
            new_lb = lb;
        else if (lb.is_constant() && v.is_constant())
            new_lb = const_value_to_nodecl(const_value_div(lb.get_constant(), v.get_constant()));
        else
            new_lb = Nodecl::Div::make(lb, v, t);
        // compute the upper bound
        if (ub.is<Nodecl::Analysis::MinusInfinity>() || ub.is<Nodecl::Analysis::PlusInfinity>())
            new_ub = ub;
        else if (ub.is_constant() && v.is_constant())
            new_ub = const_value_to_nodecl(const_value_div(ub.get_constant(), v.get_constant()));
        else
            new_ub = Nodecl::Div::make(ub, v, t);
        
        Nodecl::Range result = Nodecl::Range::make(new_lb, new_ub, NBase(const_value_to_nodecl(one)), t);

#ifdef RANGES_DEBUG
        std::cerr << "        Range Value Division " << r.prettyprint() << " / " << v.prettyprint()
                  << " = " << result.prettyprint() << std::endl;
#endif

        return result;
    }

    // ******************************************************************************************* //
    // ******************************* Range Analysis Constraints ******************************** //

    Constraint::Constraint()
        : _constr_sym(Symbol()), _constraint(NBase::null())
    {}

    Constraint::Constraint(const TL::Symbol& constr_sym, const NBase& constraint)
        : _constr_sym(constr_sym), _constraint(constraint)
    {}

    TL::Symbol Constraint::get_symbol() const 
    {
        return _constr_sym;
    }

    void Constraint::set_symbol(const TL::Symbol& s)
    {
        _constr_sym = s;
    }

    NBase Constraint::get_constraint() const 
    {
        return _constraint;
    }

    bool Constraint::operator!=(const Constraint& c) const
    {
        return ((this->_constr_sym != c._constr_sym)
                || !Nodecl::Utils::structurally_equal_nodecls(this->_constraint, c._constraint, /*skip_conversions*/true));
    }

    bool Constraint::operator==(const Constraint& c) const
    {
        return ((this->_constr_sym == c._constr_sym)
                && Nodecl::Utils::structurally_equal_nodecls(this->_constraint, c._constraint, /*skip_conversions*/true));
    }

    // ***************************** END Range Analysis Constraints ****************************** //
    // ******************************************************************************************* //

}
}
}
