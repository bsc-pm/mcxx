/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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
#include "tl-counters.hpp"
#include "tl-predicateutils.hpp"
#include "cxx-cexpr.h"
#include "cxx-nodecl-deep-copy.h"
#include "cxx-utils.h"
#include "cxx-graphviz.h"
#include "cxx-entrylist.h"
#include <algorithm>

namespace Nodecl
{
    static void get_all_symbols_rec(Nodecl::NodeclBase n, TL::ObjectList<TL::Symbol>& result)
    {
        if (n.is_null())
            return;

        if (n.has_symbol())
        {
            if (n.is<Nodecl::ObjectInit>())
            {
                get_all_symbols_rec(n.get_symbol().get_value(), result);
            }

            // Ignore the internal symbol which represents the C++ NULL constant
            if (n.get_symbol().get_name() != "__null")
            {
                result.insert(n.get_symbol());
            }
        }

        Nodecl::NodeclBase::Children children = n.children();

        for (Nodecl::NodeclBase::Children::iterator it = children.begin();
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

    struct IsLocalSymbol
    {
        private:
            TL::Scope _sc;

        public:
            IsLocalSymbol(Nodecl::NodeclBase root)
                : _sc(root.retrieve_context())
            {
            }

            bool operator()(const TL::Symbol& sym) const
            {
                // If its scope is contained in the base node one, then it is
                // "local"
                return sym.get_scope().scope_is_enclosed_by(_sc);
            }
    };

    struct IsNonLocalSymbol
    {
        private:
            TL::Scope _sc;

        public:
            IsNonLocalSymbol(Nodecl::NodeclBase root)
                : _sc(root.retrieve_context())
            {
            }

            bool operator()(const TL::Symbol& sym) const
            {
                // If its scope is not contained in the base node one, then it
                // is "nonlocal"
                return !sym.get_scope().scope_is_enclosed_by(_sc);
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

        if (n.is<Nodecl::Symbol>()
                // Ignore the internal symbol which represents the C++ NULL constant
                && n.as<Nodecl::Symbol>().get_symbol().get_name() != "__null")
        {
            result.append(n.as<Nodecl::Symbol>());
        }
        else if (n.is<Nodecl::ObjectInit>())
        {
            get_all_symbols_occurrences_rec(n.get_symbol().get_value(), result);
        }

        Nodecl::NodeclBase::Children children = n.children();

        for (Nodecl::NodeclBase::Children::iterator it = children.begin();
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

    struct IsLocalOcurrence
    {
        private:
            IsLocalSymbol _pred;

        public:
            IsLocalOcurrence(Nodecl::NodeclBase root)
                : _pred(root)
            {
            }

            bool operator()(const Nodecl::Symbol& n) const
            {
                return _pred(n.get_symbol());
            }
    };

    struct IsNonLocalOcurrence
    {
        private:
            IsNonLocalSymbol _pred;

        public:
            IsNonLocalOcurrence(Nodecl::NodeclBase root)
                : _pred(root)
            {
            }

            bool operator()(const Nodecl::Symbol& n) const
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

        if (n.is<Nodecl::Symbol>()
                // Ignore the internal symbol which represents the C++ NULL constant
                && n.as<Nodecl::Symbol>().get_symbol().get_name() != "__null")
        {
            result.insert<TL::Symbol>(n.as<Nodecl::Symbol>(),
                   &Nodecl::Symbol::get_symbol);
        }
        else if (n.is<Nodecl::ObjectInit>())
        {
            get_all_symbols_first_occurrence_rec(n.get_symbol().get_value(), result);
        }

        Nodecl::NodeclBase::Children children = n.children();
        for (Nodecl::NodeclBase::Children::iterator it = children.begin();
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

    static void get_all_memory_accesses_rec(Nodecl::NodeclBase n, bool in_ref, bool in_class_member,
                                            TL::ObjectList<Nodecl::NodeclBase>& result)
    {
        if (n.is_null())
            return;

        if (!in_ref && !in_class_member &&
            (n.is<Nodecl::Symbol>() || n.is<Nodecl::ObjectInit>()
                || n.is<Nodecl::PointerToMember>() || n.is<Nodecl::Dereference>()
                || n.is<Nodecl::ArraySubscript>() || n.is<Nodecl::ClassMemberAccess>()))
        {
            result.insert(n);
        }
        else if (n.is<Nodecl::Reference>())
        {   // Nothing to be done for &x
            // * &s       -> there is no load of s
            // * &a[i]    -> there is only a load of i
            // * &(p + q) -> there is a load of p and q
            in_ref = true;
        }

        if (n.is<Nodecl::ArraySubscript>())
        {
            Nodecl::ArraySubscript as = n.as<Nodecl::ArraySubscript>();
            if (!in_class_member)
            {
                Nodecl::NodeclBase subscripted = as.get_subscripted().no_conv();
                if (!in_ref && !subscripted.get_type().is_pointer())
                {    // a[...] (if a array) only access memory for the subscripts
                    get_all_memory_accesses_rec(subscripted, /*in_ref*/false, in_class_member, result);
                }
            }
            Nodecl::List subscripts = as.get_subscripts().as<Nodecl::List>( );
            for (Nodecl::List::iterator it = subscripts.begin(); it != subscripts.end(); it++)
            {
                get_all_memory_accesses_rec(it->no_conv(), /*in_ref*/false, /*in_class_member*/false, result);
            }

        }
        else
        {
            // Check if we have to take care only of the subscripts (in case we were taking care of everything so far)
            if (!in_class_member)
                if (n.is<Nodecl::ClassMemberAccess>())
                    in_class_member = true;

            Nodecl::NodeclBase::Children children = n.children();
            for (Nodecl::NodeclBase::Children::iterator it = children.begin();
                it != children.end(); it++)
            {
                get_all_memory_accesses_rec(it->no_conv(), in_ref, in_class_member, result);
            }
        }
    }

    TL::ObjectList<Nodecl::NodeclBase> Utils::get_all_memory_accesses(Nodecl::NodeclBase n)
    {
        TL::ObjectList<Nodecl::NodeclBase> obj_list;
        get_all_memory_accesses_rec(n, /*in ref*/false, /*only subscripts*/false, obj_list);
        return obj_list;
    }

    static int cmp_trees_rec(nodecl_t n1, nodecl_t n2, bool skip_conversion_nodes)
    {
        const bool n1_is_null = nodecl_is_null(n1);
        const bool n2_is_null = nodecl_is_null(n2);

        if((n1_is_null && n2_is_null) || 
                (nodecl_get_ast(n1) == nodecl_get_ast(n2)))
            return 0;

        if (n1_is_null == n2_is_null)
        {
            if(skip_conversion_nodes)
            {
                if(nodecl_get_kind(n1) == NODECL_CONVERSION)
                    n1 = nodecl_get_child(n1, 0);
                if(nodecl_get_kind(n2) == NODECL_CONVERSION)
                    n2 = nodecl_get_child(n2, 0);

                // Optimization: We assume that inside a NODECL_CONVERSION
                // there needs to be non-null nodecl
            }

            const node_t n1_kind = nodecl_get_kind(n1);
            const node_t n2_kind = nodecl_get_kind(n2);
                
            if (n1_kind == n2_kind) // kind
            {
                const scope_entry_t * const n1_symbol = nodecl_get_symbol(n1);
                const scope_entry_t * const n2_symbol = nodecl_get_symbol(n2);

                if  (n1_symbol == n2_symbol) // symbol
                {
                    const_value_t * n1_constant = nodecl_get_constant(n1);
                    const_value_t * n2_constant = nodecl_get_constant(n2);

                    // FIXME - Remove this special case when we quit comparing trees
                    if (n1_constant != NULL
                            && (const_value_is_object(n1_constant)
                                || const_value_is_address(n1_constant)))
                    {
                        // std::cerr << "(1.1) REMOVING " << const_value_to_str(n1_constant) << std::endl;
                        n1_constant = NULL;
                    }
                    if (n2_constant != NULL
                            && (const_value_is_object(n2_constant)
                                || const_value_is_address(n2_constant)))
                    {
                        // std::cerr << "(1.2) REMOVING " << const_value_to_str(n2_constant) << std::endl;
                        n2_constant = NULL;
                    }

                    if (n1_constant == n2_constant) // constant
                    {
                        // Everything looks equal in this single node, let's check our children
                        int equal = 0;
                        for (int i=0; (equal == 0) && (i < MCXX_MAX_AST_CHILDREN); i++)
                        {
                            const nodecl_t n1_child = nodecl_get_child(n1, i);
                            const nodecl_t n2_child = nodecl_get_child(n2, i);

                            if(nodecl_is_null(n1_child) &&
                                    nodecl_is_null(n2_child)) // Optimization: Skip recursive call.
                                continue;

                            equal = cmp_trees_rec(n1_child, n2_child, skip_conversion_nodes);
                        }

                        return equal;
                    }
                    else if (n1_constant < n2_constant) // constant
                    {
                        return -1;
                    }
                    else // constant
                    {
                        return 1;
                    }
                }
                else if (n1_symbol < n2_symbol) // symbol
                {
                    return -1;
                }
                else // symbol
                {
                    return 1;
                }
            }
            else if (n1_kind < n2_kind) // kind
            {
                return -1;
            }
            else // kind
            {
                return 1;
            }
        }
        else if (!n1_is_null)
        {
            return -1;
        }
        else
        {
            return 1;
        }
    }

    static bool equal_trees_rec(nodecl_t n1, nodecl_t n2, bool skip_conversion_nodes)
    {
        const bool n1_is_null = nodecl_is_null(n1);
        const bool n2_is_null = nodecl_is_null(n2);

        if((n1_is_null && n2_is_null) || 
                (nodecl_get_ast(n1) == nodecl_get_ast(n2)))
            return true;

        if (n1_is_null == n2_is_null)
        {
            if(skip_conversion_nodes)
            {
                if(nodecl_get_kind(n1) == NODECL_CONVERSION)
                    n1 = nodecl_get_child(n1, 0);
                if(nodecl_get_kind(n2) == NODECL_CONVERSION)
                    n2 = nodecl_get_child(n2, 0);

                // Optimization: We assume that inside a NODECL_CONVERSION
                // there needs to be non-null nodecl
            }

            const node_t n1_kind = nodecl_get_kind(n1);
            const node_t n2_kind = nodecl_get_kind(n2);
                
            if (n1_kind == n2_kind) // kind
            {
                const scope_entry_t * const n1_symbol = nodecl_get_symbol(n1);
                const scope_entry_t * const n2_symbol = nodecl_get_symbol(n2);

                if  (n1_symbol == n2_symbol) // symbol
                {
                    const_value_t * n1_constant = nodecl_get_constant(n1);
                    const_value_t * n2_constant = nodecl_get_constant(n2);

                    // FIXME - Remove this special case when we quit comparing trees
                    if (n1_constant != NULL
                            && (const_value_is_object(n1_constant)
                                || const_value_is_address(n1_constant)))
                    {
                        // std::cerr << "(2.1) REMOVING " << const_value_to_str(n1_constant) << std::endl;
                        n1_constant = NULL;
                    }
                    if (n2_constant != NULL
                            && (const_value_is_object(n2_constant)
                                || const_value_is_address(n2_constant)))
                    {
                        // std::cerr << "(2.2) REMOVING " << const_value_to_str(n2_constant) << std::endl;
                        n2_constant = NULL;
                    }

                    if (n1_constant == n2_constant) // constant
                    {
                        bool equal = true;
                        // Everything looks equal in this single node, let's check our children
                        for (int i=0; equal && i < MCXX_MAX_AST_CHILDREN; i++)
                        {
                            const nodecl_t n1_child = nodecl_get_child(n1, i);
                            const nodecl_t n2_child = nodecl_get_child(n2, i);

                            const bool n1_child_is_null = nodecl_is_null(n1_child);
                            const bool n2_child_is_null = nodecl_is_null(n2_child);

                            if(n1_child_is_null && n2_child_is_null) // Optimization: Skip recursive call.
                                continue;

                            // Different children structure
                            if(n1_child_is_null != n2_child_is_null)
                                return false;

                            equal = equal_trees_rec(n1_child, n2_child, skip_conversion_nodes);
                        }

                        return equal;
                    }
                }
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

    bool Utils::nodecl_is_literal( Nodecl::NodeclBase n )
    {
        bool res = false;
        if (n.is<Nodecl::BooleanLiteral>( ) || n.is<ComplexLiteral>() ||
            n.is<IntegerLiteral>( ) || n.is<FloatingLiteral>( ) || n.is<StringLiteral>( ))
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

    bool Utils::nodecl_is_modifiable_lvalue( Nodecl::NodeclBase n )
    {
        return n.get_type().is_lvalue_reference( );
    }

    bool Utils::nodecl_contains_nodecl_by_structure(
            const Nodecl::NodeclBase& haystack,
            const Nodecl::NodeclBase& needle)
    {
        SimpleStructuralNodeFinderVisitor finder(needle);
        finder.walk(haystack);
        return !finder._found_node.is_null();
    }

    bool Utils::nodecl_contains_nodecl_by_pointer(
            const Nodecl::NodeclBase& haystack,
            const Nodecl::NodeclBase& needle)
    {
        SimplePointerNodeFinderVisitor finder(needle);
        finder.walk(haystack);
        return !finder._found_node.is_null();
    }

    void nodecl_replace_nodecl_common(
            TL::ObjectList<Nodecl::NodeclBase>& target_nodes,
            const Nodecl::NodeclBase& replacement)
    {
        for(TL::ObjectList<Nodecl::NodeclBase>::iterator it =
                target_nodes.begin();
                it != target_nodes.end();
                it++)
        {
            Nodecl::NodeclBase target_node = *it;
            Nodecl::NodeclBase target_node_parent = target_node.get_parent();

            //Conversions!
            if (target_node_parent != Nodecl::NodeclBase::null() &&
                    !replacement.is<Nodecl::Symbol>() && // TODO:
                    target_node_parent.is<Nodecl::Conversion>())
            {
                Nodecl::Conversion parent_conv =
                    target_node_parent.as<Nodecl::Conversion>();
                
                TL::Type dst_type = parent_conv.get_type().no_ref();
                TL::Type src_type = replacement.get_type().no_ref();

                if (dst_type.is_same_type(src_type))
                {
                    target_node = target_node_parent;
                }
            }

            target_node.replace(replacement.shallow_copy());
        }
    }

    void Utils::nodecl_replace_nodecl_by_structure(
            const Nodecl::NodeclBase& haystack,
            const Nodecl::NodeclBase& needle,
            const Nodecl::NodeclBase& replacement)
    {
        CollectStructuralNodeFinderVisitor finder(needle);
        finder.walk(haystack);

        nodecl_replace_nodecl_common(finder._found_nodes, replacement);
    }

    void Utils::nodecl_replace_nodecl_by_pointer(
            const Nodecl::NodeclBase& haystack,
            const Nodecl::NodeclBase& needle,
            const Nodecl::NodeclBase& replacement)
    {
        // Is it necessary to use CollectPointerNodeFinderVisitor?
        // It will return only one node
        CollectPointerNodeFinderVisitor finder(needle);
        finder.walk(haystack);

        nodecl_replace_nodecl_common(finder._found_nodes, replacement);
    }

    bool Utils::dataref_contains_dataref( Nodecl::NodeclBase container, Nodecl::NodeclBase contained )
    {
        bool result = false;

        if( Nodecl::Utils::structurally_equal_nodecls( container, contained ) )
        {
            result = true;
        }
        else if( container.is<Nodecl::Conversion>( ) )
        {
            result = dataref_contains_dataref( container.as<Nodecl::Conversion>( ).get_nest( ), contained );
        }
        else if( contained.is<Nodecl::Conversion>( ) )
        {
            result = dataref_contains_dataref( container, contained.as<Nodecl::Conversion>( ).get_nest( ) );
        }
        else if( container.is<Nodecl::Dereference>( ) )
        {
            if( contained.is<Nodecl::ArraySubscript>( ) )
            {
                Nodecl::NodeclBase container_rhs = container.as<Nodecl::Dereference>( ).get_rhs( );
                Nodecl::ArraySubscript contained_array = contained.as<Nodecl::ArraySubscript>( );
                Nodecl::NodeclBase contained_subscripted = contained_array.get_subscripted( );
                if( Nodecl::Utils::structurally_equal_nodecls( container_rhs, contained_subscripted ) )
                {
                    Nodecl::List contained_subscripts = contained_array.get_subscripts( ).as<Nodecl::List>( );
                    if( ( contained_subscripts.size( ) == 1 ) &&
                        contained_subscripts[0].is_constant( ) &&
                        const_value_is_zero( contained_subscripts[0].get_constant( ) ) )
                    {   // container: *array, contained: array[0]
                        result = true;
                    }
                }
            }
        }
        else if( container.is<Nodecl::ArraySubscript>( ) )
        {
            if( contained.is<Nodecl::ArraySubscript>( ) )
            {   // Check the positions of the array that are accessed
                Nodecl::ArraySubscript container_array = container.as<Nodecl::ArraySubscript>( );
                Nodecl::ArraySubscript contained_array = contained.as<Nodecl::ArraySubscript>( );
                if( structurally_equal_nodecls( container_array.get_subscripted( ), contained_array.get_subscripted( ) ) )
                {
                    Nodecl::List container_subscripts = container_array.get_subscripts( ).as<Nodecl::List>( );
                    Nodecl::List contained_subscripts = contained_array.get_subscripts( ).as<Nodecl::List>( );
                    Nodecl::List::iterator it1 = container_subscripts.begin( );
                    Nodecl::List::iterator it2 = contained_subscripts.begin( );
                    for( ; it1 != container_subscripts.end( ) && it2 != contained_subscripts.end( ) && !result; ++it1, ++it2 )
                    {
                        result = dataref_contains_dataref( *it1, *it2 );
                    }
                }
            }
        }
        else if( container.is<Nodecl::ClassMemberAccess>( ) )
        {
            if (contained.is<Nodecl::ClassMemberAccess>())
            {
                Nodecl::NodeclBase lhs = contained.as<Nodecl::ClassMemberAccess>().get_lhs();
                result = dataref_contains_dataref(container, lhs);
            }
            else if (contained.is<Nodecl::ArraySubscript>())
            {
                Nodecl::NodeclBase subscripted = contained.as<Nodecl::ArraySubscript>().get_subscripted();
                result = dataref_contains_dataref(container, subscripted);
            }
        }
        else if( container.is<Nodecl::Symbol>( ) )
        {
            if( contained.is<Nodecl::Reference>( ) &&
                contained.as<Nodecl::Reference>( ).get_rhs( ).is<ArraySubscript>( ) )
            {
                Nodecl::List contained_subscripts = contained.as<Nodecl::Reference>( ).get_rhs( ).as<ArraySubscript>( ).get_subscripts( ).as<Nodecl::List>( );
                if( ( contained_subscripts.size( ) == 1 ) &&
                    contained_subscripts[0].is_constant( ) &&
                    const_value_is_zero( contained_subscripts[0].get_constant( ) ) )
                {   // container: array, contained: &array[0]
                    result = true;
                }
            }
            else if( contained.is<Nodecl::ClassMemberAccess>( ) )
            {
                result = dataref_contains_dataref( container, contained.as<Nodecl::ClassMemberAccess>( ).get_lhs( ) );
            }
        }

        return result;
    }

    bool Utils::nodecl_is_in_nodecl_list(
            const Nodecl::NodeclBase& n,
            const Nodecl::List& l,
            const bool skip_conversion_nodecls)
    {
        if (n.is<Nodecl::List>())
        {
            ERROR_CONDITION(!n.is<List>(), "Can't found a list found in a list", 0);
        }

        for (Nodecl::List::const_iterator it = l.begin(); it != l.end(); ++it)
        {
            if (structurally_equal_nodecls(n, *it, skip_conversion_nodecls))
            {
                return true;
            }
        }

        return false;
    }

    bool Utils::structurally_equal_nodecls(const Nodecl::NodeclBase& n1, const Nodecl::NodeclBase& n2,
            const bool skip_conversion_nodecls)
    {
        nodecl_t n1_ = n1.get_internal_nodecl();
        nodecl_t n2_ = n2.get_internal_nodecl();

        /*
        if (nodecl_is_list(n1_) || nodecl_is_list(n2_))
        {
            std::cerr << "warning: method 'equal_nodecls' is implemented to compare nodecls containing trees with "
                      << " no lists inside. The method returns false but they can be the same tree" << std::endl;
            return false;
        }
        */

        bool equals = equal_trees_rec(n1_, n2_, skip_conversion_nodecls);
        return equals;
    }

    int Utils::structurally_cmp_nodecls(Nodecl::NodeclBase n1, Nodecl::NodeclBase n2, bool skip_conversion_nodes)
    {
        nodecl_t n1_ = n1.get_internal_nodecl();
        nodecl_t n2_ = n2.get_internal_nodecl();

        return cmp_trees_rec(n1_, n2_, skip_conversion_nodes);
    }

    bool Utils::structurally_less_nodecls(Nodecl::NodeclBase n1, Nodecl::NodeclBase n2, bool skip_conversion_nodes)
    {
        nodecl_t n1_ = n1.get_internal_nodecl();
        nodecl_t n2_ = n2.get_internal_nodecl();

        return cmp_trees_rec(n1_, n2_, skip_conversion_nodes) < 0;
    }

    size_t Utils::Nodecl_hash::operator() (const Nodecl::NodeclBase& n) const
    {
        return nodecl_hash_table(n.get_internal_nodecl());
    }

    bool Utils::Nodecl_structural_equal::operator() (const Nodecl::NodeclBase& n1, const Nodecl::NodeclBase& n2) const
    {
        return structurally_equal_nodecls(n1, n2);
    }

    bool Utils::Nodecl_structural_less::operator() (const Nodecl::NodeclBase& n1, const Nodecl::NodeclBase& n2) const
    {
        return structurally_less_nodecls(n1, n2, /*skip_conversion_nodes*/true);
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

        const decl_context_t* decl_context = sc.get_decl_context();

        if (decl_context->block_scope != NULL)
        {
            result = decl_context->block_scope->related_entry;
        }
        else if (decl_context->function_scope != NULL)
        {
            result = decl_context->function_scope->related_entry;
        }

        return result;
    }

    Nodecl::NodeclBase Utils::get_enclosing_list(Nodecl::NodeclBase n)
    {
        while (!n.is_null() && !n.is<Nodecl::List>())
        {
            n = n.get_parent();
        }

        // We want the whole list
        if (!n.is_null())
            n =  get_all_list_from_list_node(n.as<Nodecl::List>());

        return n;
    }

    Nodecl::NodeclBase Utils::get_enclosing_node_in_list(Nodecl::NodeclBase n)
    {
        ERROR_CONDITION(n.is<Nodecl::List>(), "Node cannot be a list", 0);

        while (!n.get_parent().is_null() && !n.get_parent().is<Nodecl::List>())
        {
            n = n.get_parent();
        }

        ERROR_CONDITION(n.get_parent().is_null(), "The original node was not enclosed by any list", 0);

        return n;
    }

    void Utils::append_to_top_level_nodecl(Nodecl::NodeclBase n)
    {
        Nodecl::TopLevel top_level = Nodecl::NodeclBase(CURRENT_COMPILED_FILE->nodecl).as<Nodecl::TopLevel>();
        Nodecl::List list = top_level.get_top_level().as<Nodecl::List>();
        list.append(n);
    }

    namespace
    {
        void simple_replace(Nodecl::NodeclBase dest, Nodecl::NodeclBase src)
        {
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

        void update_locus(nodecl_t n, const locus_t* locus)
        {
            if (nodecl_is_null(n))
                return;

            if (!nodecl_is_list(n))
            {
                std::string internal_source = "MERCURIUM_INTERNAL_SOURCE";

                const locus_t* n_locus = nodecl_get_locus(n);

                // Only update if this comes from internal_source
                if (n_locus == NULL
                        || locus_get_filename(n_locus) == NULL
                        || (std::string(locus_get_filename(n_locus))
                            .substr(0, internal_source.size()) == internal_source))
                {
                    nodecl_set_locus(n, locus);
                }

                for (int i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
                {
                    update_locus(nodecl_get_child(n, i), locus);
                }
            }
            else
            {
                int num_items;
                nodecl_t* l = nodecl_unpack_list(n, &num_items);
                for (int i = 0; i < num_items; i++)
                {
                    update_locus(l[i], locus);
                }
                DELETE(l);
            }
        }
    }

    void Utils::replace(Nodecl::NodeclBase dest, Nodecl::NodeclBase src)
    {
        ERROR_CONDITION(src.is_null(), "Invalid node", 0);

        if (CURRENT_CONFIGURATION->line_markers)
        {
            update_locus(src.get_internal_nodecl(), dest.get_locus());
        }

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

    Nodecl::NodeclBase Utils::skip_contexts_and_lists(
            Nodecl::NodeclBase n)
    {
        while ((!n.is_null()) &&
            (n.is<Nodecl::Context>() ||
             n.is<Nodecl::List>()))
        {
            if (n.is<Nodecl::List>())
                n = n.as<Nodecl::List>().front();
            else if (n.is<Nodecl::Context>())
                n = n.as<Nodecl::Context>().
                    get_in_context();
        }

        return n;
    }

    bool Utils::is_in_list(Nodecl::NodeclBase n)
    {
        return (!n.get_parent().is_null()
                && n.get_parent().is<Nodecl::List>());
    }

    Nodecl::NodeclBase Utils::get_previous_sibling(const Nodecl::NodeclBase& n)
    {
        if (n.is_null()) return n;
        
        const Nodecl::NodeclBase parent = n.get_parent();

        if (parent.is_null()) return parent;
        else if (parent.is<Nodecl::List>())
        {
            Nodecl::NodeclBase child0 = parent.children()[0];
            if (child0.is<Nodecl::List>())
                return child0.as<Nodecl::List>().children()[1];
        }

        return Nodecl::NodeclBase::null();
    }

    void Utils::append_items_after(Nodecl::NodeclBase n, Nodecl::NodeclBase items)
    {
        if (!Utils::is_in_list(n))
        {
            n = Utils::get_enclosing_node_in_list(n);
        }

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

    bool Utils::is_nodecl_statement(const Nodecl::NodeclBase& n)
    {
        //TODO
        // ObjectInit is considered a special kind of Statement
        return n.is<Nodecl::ExpressionStatement>() ||
            n.is<Nodecl::IfElseStatement>() ||
            n.is<Nodecl::ForStatement>() ||
            n.is<Nodecl::WhileStatement>() ||
            n.is<Nodecl::CompoundStatement>() ||
            n.is<Nodecl::CaseStatement>() ||
            n.is<Nodecl::SwitchStatement>() ||
            n.is<Nodecl::DefaultStatement>() ||
            n.is<Nodecl::CaseStatement>() ||
            n.is<Nodecl::GotoStatement>() ||
            n.is<Nodecl::ReturnStatement>() ||
            n.is<Nodecl::ObjectInit>();
    }

    Nodecl::NodeclBase get_nesting_statement(const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& obj_init_context)
    {
        Nodecl::NodeclBase target_stmt = n;
        while (!target_stmt.is_null() && !Utils::is_nodecl_statement(target_stmt))
        {
            target_stmt = target_stmt.get_parent();
        }

        // ObjectInit value?
        if (target_stmt.is_null())
        {
            if (obj_init_context.is_null())
                internal_error("Nodecl::Utils::get_nesting_statement: target_stmt is null and obj_init_context is null", 0);

            TL::ObjectList<Nodecl::NodeclBase> obj_init_list =
                Utils::nodecl_get_all_nodecls_of_kind<Nodecl::ObjectInit>(obj_init_context);

            for (TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = obj_init_list.begin();
                    it != obj_init_list.end();
                    it++)
            {
                TL::Symbol sym = it->get_symbol();
                Nodecl::NodeclBase init = sym.get_value();

                if(!init.is_null())
                {
                    if (Utils::nodecl_contains_nodecl_by_pointer(init, n))
                    {
                        target_stmt = *it;
                        break;
                    }
                }
            }
        }
        
        if (target_stmt.is_null())
            internal_error("Nodecl::Utils::get_nesting_statement: target_stmt is null", 0);

        return target_stmt;
    }


    void Utils::prepend_sibling_statement(const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& new_stmt,
            const Nodecl::NodeclBase& obj_init_context)
    {
        Nodecl::NodeclBase target_stmt = get_nesting_statement(n, obj_init_context);
        target_stmt.prepend_sibling(new_stmt);
    }

    void Utils::append_sibling_statement(const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& new_stmt,
            const Nodecl::NodeclBase& obj_init_context)
    {
        Nodecl::NodeclBase target_stmt = get_nesting_statement(n, obj_init_context);
        target_stmt.append_sibling(new_stmt);
    }

    void Utils::prepend_items_before(Nodecl::NodeclBase n, Nodecl::NodeclBase items)
    {
        if (!Utils::is_in_list(n))
        {
            n = Utils::get_enclosing_node_in_list(n);
        }

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
            last_it = list.last();
        }
    }

    void Nodecl::Utils::prepend_items_in_nested_compound_statement(
            const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& items)
    {
        Nodecl::CompoundStatement node = 
            nodecl_get_first_nodecl_of_kind<Nodecl::CompoundStatement>(n).
            as<Nodecl::CompoundStatement>();

        ERROR_CONDITION(node.is_null(), "CompoundStatement is null", 0);

        Nodecl::NodeclBase statements = node.get_statements();
        ERROR_CONDITION(!statements.is_null() && !statements.is<List>(), "Unexpected node", 0);

        Nodecl::List stmts_list;
        if (statements.is<List>())
            stmts_list = statements.as<List>();

        stmts_list.prepend(items);

        if (statements.is_null())
            node.set_statements(stmts_list);
    }

    void Nodecl::Utils::append_items_in_nested_compound_statement(
            const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& items)
    {
        Nodecl::CompoundStatement node = 
            nodecl_get_first_nodecl_of_kind<Nodecl::CompoundStatement>(n).
            as<Nodecl::CompoundStatement>();

        ERROR_CONDITION(node.is_null(), "CompoundStatement is null", 0);

        Nodecl::NodeclBase statements = node.get_statements();
        ERROR_CONDITION(!statements.is_null() && !statements.is<List>(), "Unexpected node", 0);

        Nodecl::List stmts_list;
        if (statements.is<List>())
            stmts_list = statements.as<List>();

        stmts_list.append(items);

        if (statements.is_null())
            node.set_statements(stmts_list);
    }

    void Utils::prepend_to_top_level_nodecl(Nodecl::NodeclBase n)
    {
        Nodecl::TopLevel top_level = Nodecl::NodeclBase(CURRENT_COMPILED_FILE->nodecl).as<Nodecl::TopLevel>();
        Nodecl::List list = top_level.get_top_level().as<Nodecl::List>();
        list.prepend(n);
    }

    Nodecl::NodeclBase Utils::advance_conversions(Nodecl::NodeclBase n)
    {
        return n.no_conv();
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
        template <typename Type, typename Map>
        void fill_deep_copy_map(Type orig, Type copied, void *info)
        {
            Map &m = *static_cast<Map*>(info);
            m[orig] = copied;
        }
    }

    Nodecl::NodeclBase Utils::deep_copy(Nodecl::NodeclBase orig,
            TL::ReferenceScope ref_scope,
            Utils::SymbolMap& map,
            Nodecl::Utils::NodeclDeepCopyMap& nodecl_deep_copy_map,
            Nodecl::Utils::SymbolDeepCopyMap& symbol_deep_copy_map)
    {
        Nodecl::NodeclBase result;

        nodecl_deep_copy_map_t* internal_nodecl_deep_copy_map = nodecl_deep_copy_map_new();
        symbol_deep_copy_map_t* internal_symbol_deep_copy_map = symbol_deep_copy_map_new();

        result = ::nodecl_deep_copy_compute_maps(orig.get_internal_nodecl(),
                ref_scope.get_scope().get_decl_context(),
                map.get_symbol_map(),
                internal_nodecl_deep_copy_map,
                internal_symbol_deep_copy_map);

        nodecl_deep_copy_map_traverse(internal_nodecl_deep_copy_map,
                &nodecl_deep_copy_map,
                &fill_deep_copy_map<nodecl_t, Nodecl::Utils::NodeclDeepCopyMap>);

        symbol_deep_copy_map_traverse(internal_symbol_deep_copy_map,
                &symbol_deep_copy_map,
                &fill_deep_copy_map<scope_entry_t*, Nodecl::Utils::SymbolDeepCopyMap>);

        nodecl_deep_copy_map_free(internal_nodecl_deep_copy_map);
        symbol_deep_copy_map_free(internal_symbol_deep_copy_map);

        return result;
    }

    Nodecl::NodeclBase Utils::deep_copy(Nodecl::NodeclBase orig,
            TL::ReferenceScope ref_scope,
            NodeclDeepCopyMap& nodecl_deep_copy_map,
            SymbolDeepCopyMap& symbol_deep_copy_map)
    {
        Utils::SimpleSymbolMap empty_map;
        return deep_copy(orig, ref_scope, empty_map, nodecl_deep_copy_map, symbol_deep_copy_map);
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

    void Utils::prepend_to_enclosing_top_level_location(Nodecl::NodeclBase current_location, Nodecl::NodeclBase items)
    {
        while (!current_location.is_null()
                && (!current_location.is<Nodecl::List>()
                || !is_in_top_level_list(current_location)))
        {
            current_location = current_location.get_parent();
        }

        if (!items.is<Nodecl::List>())
        {
            items = Nodecl::List::make(items);
        }
        Nodecl::List list_items = items.as<Nodecl::List>();

        ERROR_CONDITION(current_location.is_null(), "This should never be null", 0);
        ERROR_CONDITION(!current_location.is<Nodecl::List>(), "Thist must be a list", 0);

        // This is a list node inside the top level list
        Nodecl::List list = current_location.as<Nodecl::List>();

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

    void Utils::append_to_enclosing_top_level_location(Nodecl::NodeclBase current_location, Nodecl::NodeclBase items)
    {
        while (!current_location.is_null()
                && (!current_location.is<Nodecl::List>()
                || !is_in_top_level_list(current_location)))
        {
            current_location = current_location.get_parent();
        }

        if (!items.is<Nodecl::List>())
        {
            items = Nodecl::List::make(items);
        }
        Nodecl::List list_items = items.as<Nodecl::List>();

        ERROR_CONDITION(current_location.is_null(), "This should never be null", 0);
        ERROR_CONDITION(!current_location.is<Nodecl::List>(), "Thist must be a list", 0);

        // This is a list node inside the top level list
        Nodecl::List list = current_location.as<Nodecl::List>();

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

    void Utils::add_statements_at_beginning_of_function(Nodecl::NodeclBase current_location, Nodecl::NodeclBase new_stmts)
    {
        TL::Symbol symbol_function = Nodecl::Utils::get_enclosing_function(current_location);
        ERROR_CONDITION(!symbol_function.is_valid(), "No symbol function found", 0);
        ERROR_CONDITION(symbol_function.get_function_code().is_null(),
                "Function does not have function code", 0);

        Nodecl::NodeclBase ctx = symbol_function
            .get_function_code()
            .as<Nodecl::FunctionCode>()
            .get_statements();

        Nodecl::NodeclBase stmts = ctx.as<Nodecl::Context>().get_in_context();
        ERROR_CONDITION(stmts.is_null(), "Not possible", 0);

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            Nodecl::List l = stmts.as<Nodecl::List>();
            ERROR_CONDITION(l.size() != 1, "Invalid list", 0);
            Nodecl::NodeclBase compound = l[0];
            ERROR_CONDITION(!compound.is<Nodecl::CompoundStatement>(), "Invalid node", 0);
            l = compound
                .as<Nodecl::CompoundStatement>()
                .get_statements()
                .as<Nodecl::List>();
            ERROR_CONDITION(l.is_null(), "Not possible", 0);

            Nodecl::NodeclBase first_stmt = l[0];
            first_stmt.prepend_sibling(new_stmts);
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            Nodecl::List l = stmts.as<Nodecl::List>();
            ERROR_CONDITION(l.size() != 1, "Invalid list", 0);

            Nodecl::NodeclBase first_stmt = l[0];
            first_stmt.prepend_sibling(new_stmts);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
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

        void insert_new_label_symbol(TL::Symbol sym, bool is_numeric_label)
        {
            TL::Counter &counter = TL::CounterManager::get_counter("label_visitor");

            std::string register_name, symbol_name;

            const decl_context_t* decl_context = _sc.get_decl_context();
            const decl_context_t* program_unit_context = decl_context->current_scope->related_entry->related_decl_context;

            if (IS_FORTRAN_LANGUAGE
                    && is_numeric_label)
            {
                std::string label_name = sym.get_name();

                for (std::string::iterator it = label_name.begin(); it != label_name.end(); it++)
                {
                    // If this is not a numeric label give up
                    ERROR_CONDITION(
                            (!(('0' <= (*it)) &&
                               ((*it) <= '9'))),
                            "'%s' is not a numeric label!\n", label_name.c_str());
                }

                int x;
                {
                    std::stringstream ss;
                    ss.str(label_name);

                    ss >> x;
                }

                bool repeated = true;

                while (repeated)
                {
                    // Add 10000 to this label
                    // and check if the name has already been used
                    int new_x = x + 10000 + (int)counter;
                    counter++;

                    ERROR_CONDITION(new_x > 99999, "Cannot generate a new temporary label", 0);

                    std::stringstream ss;
                    ss << new_x;

                    symbol_name = ss.str();
                    register_name = ".label_" + symbol_name;

                    scope_entry_list_t* entry_list = ::query_name_str_flags(
                            program_unit_context,
                            uniquestr(register_name.c_str()),
                            NULL,
                            DF_ONLY_CURRENT_SCOPE);

                    if (entry_list == NULL)
                    {
                        repeated = false;
                    }
                    else
                    {
                        ::entry_list_free(entry_list);
                    }
                }
            }
            else
            {
                std::stringstream ss;
                ss << sym.get_name() << "_" << (int)counter;
                counter++;

                symbol_name = ss.str();
                register_name = symbol_name;
            }

            scope_entry_t* new_label = NULL;
            if (IS_FORTRAN_LANGUAGE)
            {
                // Labels in Fortran live in the program unit context
                new_label = ::new_symbol(program_unit_context, program_unit_context->current_scope,
                        uniquestr(register_name.c_str()));
            }
            else
            {
                new_label = ::new_symbol(decl_context, decl_context->function_scope, uniquestr(register_name.c_str()));
            }
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
                insert_new_label_symbol(sym, /* is_numeric_label */ true);
            }
        }

        void visit(const Nodecl::LabeledStatement& stmt)
        {
            walk(stmt.get_statement());

            TL::Symbol sym = stmt.get_symbol();
            if (sym.is_label())
            {
                insert_new_label_symbol(sym, /* is_numeric_label*/ true);
            }
        }

        void visit(const Nodecl::ForStatement& stmt)
        {
            walk(stmt.get_statement());
            if (!stmt.get_loop_name().is_null())
            {
                insert_new_label_symbol(stmt.get_loop_name().get_symbol(), /* is_numeric_label*/ false);
            }
        }

        void visit(const Nodecl::WhileStatement& stmt)
        {
            walk(stmt.get_statement());
            if (!stmt.get_loop_name().is_null())
            {
                insert_new_label_symbol(stmt.get_loop_name().get_symbol(), /* is_numeric_label*/ false);
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

        Nodecl::NodeclBase::Children children = node.children();
        for (Nodecl::NodeclBase::Children::iterator it = children.begin();
                it != children.end();
                it++)
        {
            update_symbols(*it, m);
        }


    }

    Nodecl::ArraySubscript Utils::linearize_array_subscript(const Nodecl::ArraySubscript& n)
    {
        int i;
        Nodecl::List indexes = n.get_subscripts().as<Nodecl::List>();
        int num_dimensions = indexes.size();

        TL::ObjectList<Nodecl::NodeclBase> sizes;

        // If already linearized, return
        if (num_dimensions == 1)
            return n.shallow_copy().as<Nodecl::ArraySubscript>();

        TL::Type subscripted_type = n.get_subscripted().get_type();

        for(i=0; i<num_dimensions; i++)
        {
            if(subscripted_type.is_pointer() && (i == 0))
            {
                // Put a NULL
                sizes.append(Nodecl::NodeclBase::null());
                subscripted_type = subscripted_type.points_to();
            }
            else if (subscripted_type.is_array())
            {
                if (!subscripted_type.array_has_size())
                {
                    internal_error("Linearize_array_subscript: it does not have size", 0);
                }

                sizes.append(subscripted_type.array_get_size());
                subscripted_type = subscripted_type.array_element();
            }
            else
            {
                internal_error("Linearize_array_subscript: it is not array type or pointer", 0);
            }
        }

        Nodecl::List::iterator it_indexes = indexes.begin();
        TL::ObjectList<Nodecl::NodeclBase>::iterator it_sizes = sizes.begin();

        Nodecl::NodeclBase new_linearized_subscript;

        // Horner algorithm
        while (it_indexes != indexes.end())
        {
            // First one is special
            if (it_indexes == indexes.begin())
            {
                new_linearized_subscript = it_indexes->shallow_copy();
            }
            else
            {
                Nodecl::Mul dim_offset =
                    Nodecl::Mul::make(
                            it_sizes->shallow_copy(),
                            new_linearized_subscript.shallow_copy(),
                            get_ptrdiff_t_type());
                
                if (it_sizes->is_constant() &&
                        new_linearized_subscript.is_constant())
                    dim_offset.set_constant(const_value_mul(
                                it_sizes->get_constant(),
                                new_linearized_subscript.get_constant()));


                new_linearized_subscript = Nodecl::Add::make(
                        dim_offset,
                        it_indexes->shallow_copy(),
                        get_ptrdiff_t_type());

                if (dim_offset.is_constant() && it_indexes->is_constant())
                    new_linearized_subscript.set_constant(const_value_add(
                                dim_offset.get_constant(),
                                it_indexes->get_constant()));
            }

            it_indexes++;
            it_sizes++;
        }

        // Subscripted
        Nodecl::NodeclBase new_subscripted = n.get_subscripted().shallow_copy();

        // Dereferencing subscripted for num_dimensions > 1
        TL::Type deref_type = new_subscripted.get_type().basic_type().
            get_pointer_to();

        new_subscripted = Nodecl::Conversion::make(
                new_subscripted.shallow_copy(),
                deref_type);
        new_subscripted.set_text("C");

        Nodecl::ArraySubscript result_array =
            ArraySubscript::make(new_subscripted.shallow_copy(),
                    Nodecl::List::make(new_linearized_subscript.shallow_copy()),
                    n.get_type(),
                    n.get_locus());

        result_array.set_constant(n.get_constant());

        return result_array;
    }

    bool Utils::list_contains_nodecl_by_structure(
            const TL::ObjectList<Nodecl::NodeclBase>& container,
            const NodeclBase& contained)
    {
        for(TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = container.begin();
                it != container.end();
                it ++)
        {
            if (structurally_equal_nodecls(contained, *it, true))
            {
                return true;
            }
        }

        return false;
    }

    TL::ObjectList<Nodecl::NodeclBase>::iterator
        Utils::list_get_nodecl_by_structure(
            TL::ObjectList<Nodecl::NodeclBase>& container,
            const NodeclBase& contained)
    {
        for(TL::ObjectList<Nodecl::NodeclBase>::iterator it = container.begin();
                it != container.end();
                it ++)
        {
            if (structurally_equal_nodecls(contained, *it, true))
            {
                return it;
            }
        }

        return container.end();
    }

    TL::ObjectList<Nodecl::NodeclBase> Utils::get_strings_as_expressions(
            const TL::ObjectList<std::string>& string_list,
            const Nodecl::NodeclBase& ref_scope)
    {
        TL::ObjectList<Nodecl::NodeclBase> nodecl_list;

        for (TL::ObjectList<std::string>::const_iterator it = string_list.begin();
                it != string_list.end();
                it++)
        {
            const std::string &variable(*it);
            TL::Source src;
            src
                << "#line " << ref_scope.get_line() << " \"" << ref_scope.get_filename() << "\"\n"
                << TL::pad_to_column(ref_scope.get_column())
                << variable
                ;

            Nodecl::NodeclBase var_tree = src.parse_expression(ref_scope.retrieve_context());
            nodecl_list.append(var_tree);
        }

        return nodecl_list;
    }

    // ********************************************************************************* //
    // *************** Visitor looking for a nodecl contained in a scope *************** //

    template <class Comparator>
    void Utils::SimpleNodeFinderVisitor<Comparator>::generic_finder(
            const Nodecl::NodeclBase& n)
    {
        if( _comparator( n, _needle ) )
        {
            _found_node = n;
        }
        else
        {
            Nodecl::NodeclBase::Children children = n.children();

            for(Nodecl::NodeclBase::Children::iterator it = children.begin();
                    it != children.end();
                    it++)
            {
                if (!it->is_null())
                {
                    walk(*it);
                    if (!_found_node.is_null())
                        break;
                }
            }
        }
    }

    template <class Comparator>
    void Utils::SimpleNodeFinderVisitor<Comparator>::unhandled_node(
            const Nodecl::NodeclBase& n)
    {
        generic_finder(n);
    }

    template <class Comparator>
    void Utils::SimpleNodeFinderVisitor<Comparator>::visit(
            const Nodecl::ObjectInit& n)
    {
        generic_finder(n);
        if(_found_node.is_null())
        {
            TL::Symbol sym = n.get_symbol( );
            Nodecl::NodeclBase val = sym.get_value( );

            if(!val.is_null( ))
                walk(val);
        }
    }

    template <class Comparator>
    void Utils::CollectNodeFinderVisitor<Comparator>::generic_finder(
            const Nodecl::NodeclBase& n)
    {
        if( _comparator( n, _needle ) )
        {
            _found_nodes.append(n);
        }
        else
        {
            Nodecl::NodeclBase::Children children = n.children();

            for(Nodecl::NodeclBase::Children::iterator it = children.begin();
                    it != children.end();
                    it++)
            {
                if (!it->is_null())
                {
                    walk(*it);
                }
            }
        }
    }

    template <class Comparator>
    void Utils::CollectNodeFinderVisitor<Comparator>::unhandled_node(
            const Nodecl::NodeclBase& n)
    {
        generic_finder(n);
    }

    template <class Comparator>
    void Utils::CollectNodeFinderVisitor<Comparator>::visit(
            const Nodecl::ObjectInit& n)
    {
        generic_finder(n);
            
        TL::Symbol sym = n.get_symbol( );
        Nodecl::NodeclBase val = sym.get_value( );

        if( !val.is_null( ) )
            walk(val);
    }


    // ************* END visitor looking for a nodecl contained in a scope ************* //
    // ********************************************************************************* //

}




namespace TL
{
    // This is actually what OpenMP expects
    // Lower bound and upper bound are closed ranges:
    //      [lower_bound, upper_bound] if step is positive
    //      [upper_bound, lower_bound] if step is negative
    template <typename CopyPolicy>
    void ForStatementHelper<CopyPolicy>::analyze_loop_header()
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

            _induction_var = loop_control.get_induction_variable();
            _lower_bound = CopyPolicy::shallow_copy(loop_control.get_lower());
            _upper_bound = CopyPolicy::shallow_copy(loop_control.get_upper());
            _step = CopyPolicy::shallow_copy(loop_control.get_step());

            _is_omp_valid = true;

            _loop_trend = UNKNOWN_LOOP;
        }
        else if (lc.is<Nodecl::LoopControl>())
        {
            Nodecl::LoopControl loop_control = lc.as<Nodecl::LoopControl>();
            Nodecl::List init_expr_list = loop_control.get_init().as<Nodecl::List>();
            Nodecl::NodeclBase test_expr = loop_control.get_cond();
            Nodecl::NodeclBase incr_expr = loop_control.get_next();

            // init-expr must have the following form
            //
            //   _induction_var = lb
            //   integer-type   _induction_var = lb
            //   random-access-iterator _induction_var = lb    // CURRENTLY NOT SUPPORTED
            //   pointer-type _induction_var = lb

            _induction_var = Nodecl::NodeclBase::null();

            _induction_variable_in_separate_scope = false;

            if (init_expr_list.size() != 1)
            {
                _is_omp_valid = false;
                return;
            }

            Nodecl::NodeclBase init_expr = init_expr_list.front();

            // _induction_var = lb
            if (init_expr.is<Nodecl::Assignment>())
            {
                Nodecl::NodeclBase lhs = init_expr.as<Nodecl::Assignment>().get_lhs();
                if (lhs.is<Nodecl::Symbol>())
                {
                    _induction_var = lhs;
                }

                Nodecl::NodeclBase rhs = init_expr.as<Nodecl::Assignment>().get_rhs();
                _lower_bound = CopyPolicy::shallow_copy(rhs);
            }
            // T _induction_var = lb
            else if (init_expr.is<Nodecl::ObjectInit>())
            {
                _induction_variable_in_separate_scope = true;
                _induction_var = init_expr;

                _lower_bound = CopyPolicy::shallow_copy(_induction_var.get_symbol().get_value());
            }
            else
            {
                _is_omp_valid = false;
                return;
            }

            if (_induction_var.is_null())
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
                    && (test_expr.as<Nodecl::LowerThan>().get_lhs().no_conv().get_symbol()
                        == _induction_var.get_symbol()
                        || test_expr.as<Nodecl::LowerThan>().get_rhs().no_conv().get_symbol()
                        == _induction_var.get_symbol()))

            {
                Nodecl::NodeclBase lhs = test_expr.as<Nodecl::LowerThan>().get_lhs();
                Nodecl::NodeclBase rhs = test_expr.as<Nodecl::LowerThan>().get_rhs();

                bool lhs_is_var = (lhs.no_conv().get_symbol() == _induction_var.get_symbol());

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
                            _upper_bound = CopyPolicy::new_node(
                                    const_value_to_nodecl(
                                    const_value_sub(
                                        rhs.get_constant(),
                                        const_value_get_one(4, 1))));
                        }
                        else
                        {
                            _upper_bound =
                                CopyPolicy::new_node(
                                        Nodecl::Minus::make(
                                            CopyPolicy::shallow_copy(rhs),
                                            CopyPolicy::new_node(const_value_to_nodecl(const_value_get_one(4, 1))),
                                            t,
                                            rhs.get_locus()));

                            _upper_bound.set_is_type_dependent(t.is_dependent());
                        }
                        _loop_trend = STRICTLY_INCREASING_LOOP;
                    }
                    else
                    {
                        // E < x this is like x > E this is like x >= E + 1
                        TL::Type t = lhs.get_type();

                        if (t.is_any_reference())
                            t = t.references_to();

                        if (lhs.is_constant())
                        {
                            _upper_bound = CopyPolicy::new_node(
                                    const_value_to_nodecl(
                                        const_value_add(
                                            lhs.get_constant(),
                                            const_value_get_one(4, 1))));
                        }
                        else
                        {
                            _upper_bound =
                                CopyPolicy::new_node(
                                        Nodecl::Add::make(
                                            CopyPolicy::shallow_copy(lhs),
                                            CopyPolicy::new_node(const_value_to_nodecl(const_value_get_one(4, 1))),
                                            t,
                                            lhs.get_locus()));

                            _upper_bound.set_is_type_dependent(t.is_dependent());
                        }
                        _loop_trend = STRICTLY_DECREASING_LOOP;
                    }
                }
                else if (test_expr.is<Nodecl::LowerOrEqualThan>())
                {
                    if (lhs_is_var)
                    {
                        // x <= E
                        _upper_bound = CopyPolicy::shallow_copy(rhs);
                        _loop_trend = STRICTLY_INCREASING_LOOP;
                    }
                    else
                    {
                        // E <= x this is like x >= E
                        _upper_bound = CopyPolicy::shallow_copy(lhs);
                        _loop_trend = STRICTLY_DECREASING_LOOP;
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
                            _upper_bound = CopyPolicy::new_node(
                                    const_value_to_nodecl(
                                    const_value_add(
                                        rhs.get_constant(),
                                        const_value_get_one(4, 1))));
                        }
                        else
                        {
                            _upper_bound = CopyPolicy::new_node(
                                    Nodecl::Add::make(
                                        CopyPolicy::shallow_copy(rhs),
                                        CopyPolicy::new_node(const_value_to_nodecl(const_value_get_one(4, 1))),
                                        t,
                                        rhs.get_locus()));

                            _upper_bound.set_is_type_dependent(t.is_dependent());
                        }

                        _loop_trend = STRICTLY_DECREASING_LOOP;
                    }
                    else
                    {
                        // E > x this is like x < E, this is like x <= E - 1
                        TL::Type t = lhs.get_type();

                        if (t.is_any_reference())
                            t = t.references_to();

                        if (lhs.is_constant())
                        {
                            _upper_bound = CopyPolicy::new_node(
                                    const_value_to_nodecl(
                                        const_value_sub(
                                            lhs.get_constant(),
                                            const_value_get_one(4, 1))));
                        }
                        else
                        {
                            _upper_bound = 
                                CopyPolicy::new_node(
                                        Nodecl::Minus::make(
                                            CopyPolicy::shallow_copy(lhs),
                                            CopyPolicy::new_node(const_value_to_nodecl(const_value_get_one(4, 1))),
                                            t,
                                            lhs.get_locus()));

                            _upper_bound.set_is_type_dependent(t.is_dependent());
                        }
                        _loop_trend = STRICTLY_INCREASING_LOOP;
                    }
                }
                else if (test_expr.is<Nodecl::GreaterOrEqualThan>())
                {
                    if (lhs_is_var)
                    {
                        // x >= E
                        _upper_bound = CopyPolicy::shallow_copy(rhs);
                        _loop_trend = STRICTLY_DECREASING_LOOP;
                    }
                    else
                    {
                        // E >= x this is like x <= E
                        _upper_bound = CopyPolicy::shallow_copy(lhs);
                        _loop_trend = STRICTLY_INCREASING_LOOP;
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
                    && incr_expr.as<Nodecl::Preincrement>().get_rhs().no_conv().get_symbol()
                    == _induction_var.get_symbol())
            {
                _step = CopyPolicy::new_node(const_value_to_nodecl(const_value_get_one(4, 1)));
            }
            // _induction_var++
            else if (incr_expr.is<Nodecl::Postincrement>()
                    && incr_expr.as<Nodecl::Postincrement>().get_rhs().no_conv().get_symbol()
                    == _induction_var.get_symbol())
            {
                _step = CopyPolicy::new_node(const_value_to_nodecl(const_value_get_one(4, 1)));
            }
            // --_induction_var
            else if (incr_expr.is<Nodecl::Predecrement>()
                    && incr_expr.as<Nodecl::Predecrement>().get_rhs().no_conv().get_symbol()
                    == _induction_var.get_symbol())
            {
                _step = CopyPolicy::new_node(const_value_to_nodecl(const_value_get_minus_one(4, 1)));
            }
            // _induction_var--
            else if (incr_expr.is<Nodecl::Postdecrement>()
                    && incr_expr.as<Nodecl::Postdecrement>().get_rhs().no_conv().get_symbol()
                    == _induction_var.get_symbol())
            {
                _step = CopyPolicy::new_node(const_value_to_nodecl(const_value_get_minus_one(4, 1)));
            }
            // _induction_var += incr
            else if (incr_expr.is<Nodecl::AddAssignment>()
                    && incr_expr.as<Nodecl::AddAssignment>().get_lhs().no_conv().get_symbol()
                    == _induction_var.get_symbol())
            {
                _step = CopyPolicy::shallow_copy(incr_expr.as<Nodecl::AddAssignment>().get_rhs());
            }
            // _induction_var -= incr
            else if (incr_expr.is<Nodecl::MinusAssignment>()
                    && incr_expr.as<Nodecl::MinusAssignment>().get_lhs().no_conv().get_symbol()
                    == _induction_var.get_symbol())
            {
                Nodecl::NodeclBase rhs = incr_expr.as<Nodecl::AddAssignment>().get_rhs();

                TL::Type t = incr_expr.as<Nodecl::AddAssignment>().get_rhs().get_type();

                if (t.is_any_reference())
                    t = t.references_to();

                if (rhs.is_constant())
                {
                    _step = CopyPolicy::new_node(
                            const_value_to_nodecl(const_value_neg(rhs.get_constant())));
                }
                else
                {
                    _step = CopyPolicy::new_node(
                            Nodecl::Neg::make(
                                rhs,
                                t,
                                rhs.get_locus()));

                    _step.set_is_type_dependent(t.is_dependent());
                }
            }
            // _induction_var = _induction_var + incr
            else if (incr_expr.is<Nodecl::Assignment>()
                    && incr_expr.as<Nodecl::Assignment>().get_lhs().no_conv().get_symbol()
                    == _induction_var.get_symbol()
                    && incr_expr.as<Nodecl::Assignment>().get_rhs().no_conv().is<Nodecl::Add>()
                    && incr_expr.as<Nodecl::Assignment>().get_rhs().no_conv()
                        .as<Nodecl::Add>().get_lhs().no_conv().get_symbol() == _induction_var.get_symbol())
            {
                _step = CopyPolicy::shallow_copy(incr_expr.as<Nodecl::Assignment>().get_rhs().as<Nodecl::Add>().get_rhs());
            }
            // _induction_var = incr + _induction_var
            else if (incr_expr.is<Nodecl::Assignment>()
                    && incr_expr.as<Nodecl::Assignment>().get_lhs().no_conv().get_symbol()
                    == _induction_var.get_symbol()
                    && incr_expr.as<Nodecl::Assignment>().get_rhs().no_conv().is<Nodecl::Add>()
                    && incr_expr.as<Nodecl::Assignment>().get_rhs().no_conv()
                        .as<Nodecl::Add>().get_rhs().no_conv().get_symbol() == _induction_var.get_symbol())
            {
                _step = CopyPolicy::shallow_copy(incr_expr.as<Nodecl::Assignment>().get_rhs().as<Nodecl::Add>().get_lhs());
            }
            // _induction_var = _induction_var - incr
            else if (incr_expr.is<Nodecl::Assignment>()
                    && incr_expr.as<Nodecl::Assignment>().get_lhs().no_conv().get_symbol()
                    == _induction_var.get_symbol()
                    && incr_expr.as<Nodecl::Assignment>().get_rhs().no_conv().is<Nodecl::Minus>()
                    && incr_expr.as<Nodecl::Assignment>().get_rhs().no_conv()
                        .as<Nodecl::Minus>().get_lhs().no_conv().get_symbol() == _induction_var.get_symbol())
            {
                Nodecl::NodeclBase rhs = incr_expr.as<Nodecl::Assignment>().get_rhs().as<Nodecl::Minus>().get_rhs();

                TL::Type t = rhs.get_type();

                if (t.is_any_reference())
                    t = t.references_to();

                if (rhs.is_constant())
                {
                    _step = CopyPolicy::new_node(
                            const_value_to_nodecl(
                            const_value_neg(rhs.get_constant())));
                }
                else
                {
                    _step = CopyPolicy::new_node(
                            Nodecl::Neg::make(
                                CopyPolicy::shallow_copy(rhs),
                                t,
                                rhs.get_locus()));

                    _step.set_is_type_dependent(t.is_dependent());
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

    bool ForStatementHelperBase::is_omp_valid_loop() const
    {
        return _is_omp_valid;
    }

    TL::Symbol ForStatementHelperBase::get_induction_variable() const
    {
        return _induction_var.get_symbol();
    }

    bool ForStatementHelperBase::induction_variable_in_separate_scope() const
    {
        return _induction_variable_in_separate_scope;
    }

    Nodecl::NodeclBase ForStatementHelperBase::get_lower_bound() const
    {
        return _lower_bound;
    }

    Nodecl::NodeclBase ForStatementHelperBase::get_upper_bound() const
    {
        return _upper_bound;
    }

    Nodecl::NodeclBase ForStatementHelperBase::get_step() const
    {
        return _step;
    }

    bool ForStatementHelperBase::is_strictly_increasing_loop() const
    {
        ERROR_CONDITION(!_is_omp_valid, "The loop is not OpenMP/OmpSs conforming", 0);

        ERROR_CONDITION(_loop_trend == UNKNOWN_LOOP,
                "impossible to determine whether this loop is increasing or not", 0);

        return _loop_trend == STRICTLY_INCREASING_LOOP;
    }

    template void ForStatementHelper<UsualCopyPolicy>::analyze_loop_header();
    template void ForStatementHelper<NoNewNodePolicy>::analyze_loop_header();


    LoopControlAdapter::LoopControlAdapter(
        Nodecl::NodeclBase lc) : _lc(lc)
    {
    }

    Nodecl::NodeclBase LoopControlAdapter::get_cond()
    {
        if (_lc.is<Nodecl::LoopControl>())
        {
            return _lc.as<Nodecl::LoopControl>().get_cond();
        }
        else if (_lc.is<Nodecl::RangeLoopControl>())
        {
            Nodecl::RangeLoopControl rlc =
                _lc.as<Nodecl::RangeLoopControl>();

            ERROR_CONDITION(!rlc.get_step().is_constant(),
                    "We need a constant step", 0);

            Nodecl::NodeclBase cond_node;

            if (const_value_is_positive(rlc.get_step().get_constant()))
            {
                return Nodecl::LowerOrEqualThan::make(
                        rlc.get_induction_variable().get_symbol()
                        .make_nodecl(/* lvalue_ref */ true),
                        rlc.get_upper().shallow_copy(),
                        TL::Type::get_bool_type());
            }
            else if (const_value_is_negative(rlc.get_step().get_constant()))
            {
                return Nodecl::GreaterOrEqualThan::make(
                        rlc.get_induction_variable().get_symbol()
                        .make_nodecl(/* lvalue_ref */ true),
                        rlc.get_upper().shallow_copy(),
                        TL::Type::get_bool_type());
            }
            else
            {
                internal_error("Code unreachable", 0);
            }
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    } 

    Nodecl::NodeclBase LoopControlAdapter::get_next()
    {
        if (_lc.is<Nodecl::LoopControl>())
        {
            return _lc.as<Nodecl::LoopControl>().get_next();
        }
        else if (_lc.is<Nodecl::RangeLoopControl>())
        {
            Nodecl::RangeLoopControl rlc =
                _lc.as<Nodecl::RangeLoopControl>();

            return Nodecl::Assignment::make(
                    rlc.get_induction_variable().get_symbol()
                    .make_nodecl(/* lvalue_ref */ true),
                    Nodecl::Add::make(
                        rlc.get_step().shallow_copy(),
                        rlc.get_induction_variable().get_symbol()
                        .make_nodecl(/* lvalue_ref */ true),
                        rlc.get_induction_variable().get_symbol().get_type()),
                    rlc.get_induction_variable().get_symbol()
                    .get_type().no_ref().get_lvalue_reference_to());
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
}

// #################
//  DEBUG FUNCTIONS
// #################

void deb_print_ast(Nodecl::NodeclBase n)
{
    ast_dump_graphviz(n.get_internal_nodecl().tree, stderr);
}

std::string deb_print_type(TL::Type type)
{
    return type.get_simple_declaration(CURRENT_COMPILED_FILE->global_decl_context, "");
}

std::string deb_print_type(const Nodecl::NodeclBase& n)
{
    return deb_print_type(n.get_type());
}



