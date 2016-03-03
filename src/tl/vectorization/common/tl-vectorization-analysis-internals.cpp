/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#include "tl-vectorization-analysis-internals.hpp"

#include "tl-expression-evolution-visitor.hpp"
#include "tl-suitable-visitor.hpp"
#include "tl-analysis-internals.hpp"

#include "cxx-cexpr.h"

namespace TL
{
namespace Vectorization
{
    bool is_adjacent_access_internal(
            Analysis::Node* const scope_node,
            Analysis::Node* const n_node,
            const Nodecl::NodeclBase& n,
            Analysis::ExtensibleGraph* const pcfg,
            std::set<Nodecl::NodeclBase> visited_nodes)
    {
        bool result = false;

        if(n.is<Nodecl::ArraySubscript>())
        {
            result = true;

            Nodecl::List subscript = n.as<Nodecl::ArraySubscript>()
                .get_subscripts().as<Nodecl::List>();
            Nodecl::List::iterator it = subscript.begin();

            for(; it != subscript.end() - 1 && result; ++it )
            {   
                // All dimensions but the less significant must be constant
                if(!is_uniform_internal(scope_node, n_node, *it, pcfg))
                {
                    result = false;
                }
            }
            // Esto de aquí arriba no lo hace ya el is_adjacent_access de abajo?

            if(result)
            {   
                Nodecl::NodeclBase last_dim_n = *it;

                // The less significant dimension must be accessed by an (+/-)c +/- IV, where c is a constant
                // If the subscript is another ArraySubscript, then it is not adjacent
                if (last_dim_n.is<Nodecl::ArraySubscript>())
                {
                    result = false;
                }
                else
                {
                    ExpressionEvolutionVisitor expr_evolution_info(
                            scope_node, n_node, pcfg, visited_nodes);
                    expr_evolution_info.walk(last_dim_n);
                    result = expr_evolution_info.is_adjacent_access( );
                }
            }
        }
        else if (n.is<Nodecl::Dereference>())
        {
            Nodecl::NodeclBase deref_element = n.as<Nodecl::Dereference>().get_rhs();

            ExpressionEvolutionVisitor expr_evolution_info(
                    scope_node, n_node, pcfg, visited_nodes);
            expr_evolution_info.walk(deref_element);
            result = expr_evolution_info.is_adjacent_access( );
        }

        return result;
    }

    int get_pointer_alignment(const Nodecl::NodeclBase &scope,
            const Nodecl::NodeclBase &n,
            const map_nodecl_int_t& aligned_expressions,
            VectorizationAnalysisInterface* analysis)
    {
        if (n.is<Nodecl::Symbol>())
        {
            Nodecl::Symbol n_sym = n.as<Nodecl::Symbol>();
            TL::Symbol tl_sym = n_sym.get_symbol();
            TL::Type sym_type = tl_sym.get_type();

            // ERROR_CONDITION(!(sym_type.is_pointer() || sym_type.is_array()),
            //         "SuitableVisitor: %s is neither a pointer nor
            //         array\n",
            //         tl_sym.get_name().c_str());

            for (const auto &aligned_expr : aligned_expressions)
            {
                if (aligned_expr.first.is<Nodecl::Symbol>()
                    && aligned_expr.first.as<Nodecl::Symbol>().get_symbol()
                           == tl_sym)
                {
                    return aligned_expr.second;
                }
            }

            Nodecl::FunctionCode scope_function;
            if (scope.is<Nodecl::FunctionCode>())
                scope_function = scope.as<Nodecl::FunctionCode>();
            else
                scope_function = Nodecl::Utils::get_enclosing_function(scope)
                                     .get_function_code()
                                     .as<Nodecl::FunctionCode>();

            return analysis->get_assume_aligned_attribute(scope_function,
                                                           n_sym);
        }
        else if (n.is<Nodecl::ArraySubscript>())
        {
            Nodecl::ArraySubscript n_array = n.as<Nodecl::ArraySubscript>();

            Nodecl::NodeclBase n_subscripted = n_array.get_subscripted();
            Nodecl::NodeclBase n_subscripts = n_array.get_subscripts();

            for (const auto &aligned_expr : aligned_expressions)
            {
                if (aligned_expr.first.is<Nodecl::ArraySubscript>())
                {
                    Nodecl::ArraySubscript aligned_expr_array
                        = aligned_expr.first.as<Nodecl::ArraySubscript>();

                    Nodecl::NodeclBase ae_subscripted
                        = aligned_expr_array.get_subscripted();
                    Nodecl::List ae_subscripts_list
                        = aligned_expr_array.get_subscripts()
                              .as<Nodecl::List>();

                    ERROR_CONDITION(
                        ae_subscripts_list.size() != 1,
                        "Unsupported aligned expression range: %s. Size != 1",
                        aligned_expr_array.prettyprint().c_str());
                    ERROR_CONDITION(
                        !ae_subscripts_list.front().is<Nodecl::Range>(),
                        "Unsupported aligned expression: %s. Range expected",
                        aligned_expr_array.prettyprint().c_str());
                    ERROR_CONDITION(
                        !ae_subscripted.is<Nodecl::Symbol>(),
                        "Unsupported aligned expression: %s. Symbol expected",
                        aligned_expr_array.prettyprint().c_str());

                    Nodecl::Range ae_range
                        = ae_subscripts_list.front().as<Nodecl::Range>();
                    ERROR_CONDITION(
                        !ae_range.get_lower().is_constant(),
                        "Unsupported aligned expression range (lower): %s",
                        ae_range.prettyprint().c_str());
                    ERROR_CONDITION(
                        !ae_range.get_upper().is_null(),
                        "Unsupported aligned expression range (upper): %s",
                        ae_range.prettyprint().c_str());

                    if (Nodecl::Utils::structurally_equal_nodecls(
                            ae_subscripted, n_subscripted, true))
                    {
                        return aligned_expr.second;
                    }
                }
            }

            return -1; // Unaligned
        }

        internal_error("Unexpected expression as aligned expression", 0);

        /*
                int alignment = tl_sym.get_type().get_alignment_of();

                std::cerr << "----> ALIG " << tl_sym.get_name() << ": " <<
           alignment
           << std::endl;

                // __attribute__((aligned(X)))
                ObjectList<TL::GCCAttribute> gcc_attrbs =
           tl_sym.get_gcc_attributes();
                for(ObjectList<TL::GCCAttribute>::const_iterator it =
           gcc_attrbs.begin();
                        it != gcc_attrbs.end();
                        it++)
                {
                    std::cerr << "----> " << it->get_attribute_name() <<
           std::endl;
                }
        */
        // There is no alignment info about the subscripted symbol
        // Assume unaligned
    }

    bool is_aligned_access(const Nodecl::NodeclBase &scope,
                           const Nodecl::ArraySubscript &n,
                           const map_nodecl_int_t &aligned_expressions,
                           const objlist_nodecl_t &suitable_expressions,
                           unsigned int vec_factor,
                           unsigned int target_alignment,
                           int &alignment_module,
                           VectorizationAnalysisInterface *analysis)
    {
        int i;
        int alignment;
        alignment_module = -1;

        // We assume that alignment == 0 means that alignment
        // is
        // not required by the ISA
        if (target_alignment == 0)
            return false;

        TL::Type n_type = n.get_type().no_ref();
        unsigned int target_alignment_in_elements
            = target_alignment / n_type.get_size();

        Nodecl::NodeclBase subscripted = n.get_subscripted();
        TL::Type element_type = subscripted.get_type();

        subscripted = Nodecl::Utils::advance_conversions(subscripted);

        // Linearized multidimensional arrays
        int field_offset = 0;

        if (subscripted.is<Nodecl::ClassMemberAccess>())
        {
            Nodecl::NodeclBase rhs
                = subscripted.as<Nodecl::ClassMemberAccess>().get_member();
            ERROR_CONDITION(!rhs.is<Nodecl::Symbol>(),
                            "invalid rhs in class member acces",
                            0);
            bool field_is_array = rhs.get_symbol().get_type().is_array();

            while (subscripted.is<Nodecl::ClassMemberAccess>())
            {
                rhs = subscripted.as<Nodecl::ClassMemberAccess>().get_member();
                ERROR_CONDITION(!rhs.is<Nodecl::Symbol>(),
                                "invalid rhs in class member acces",
                                0);

                if (field_is_array)
                {
                    field_offset
                        += rhs.as<Nodecl::Symbol>().get_symbol().get_offset();
                }
                subscripted
                    = subscripted.as<Nodecl::ClassMemberAccess>().get_lhs();
            }
        }

        // Double pointers
        // if (!subscripted.is<Nodecl::Symbol>())
        //    return false;

        alignment = get_pointer_alignment(
            scope, subscripted, aligned_expressions, analysis);

        if (alignment == -1)
        {
            // There is no alignment info about the subscripted symbol
            // Assume unaligned
            return false;
        }

        alignment += field_offset;

        Nodecl::List subscripts = n.get_subscripts().as<Nodecl::List>();
        int num_subscripts = subscripts.size();

        // Get dimension sizes
        std::vector<int> dimension_sizes(/* n = */ num_subscripts - 1,
                                         /* val = */ 0);

        SuitableVisitor suitable_visitor(scope,
                                         suitable_expressions,
                                         target_alignment_in_elements,
                                         vec_factor,
                                         analysis);

        for (i = 0; i < (num_subscripts - 1);
             i++) // Skip the first one. It does not have size
        {
            // Iterate on array subscript type
            if (element_type.is_array())
            {
                element_type = element_type.array_element();
            }
            else if (element_type.is_pointer())
            {
                element_type = element_type.points_to();
            }
            else
            {
                WARNING_MESSAGE(
                    "Array subscript does not have array type or pointer to "
                    "array "
                    "type",
                    0);
                return false;
            }

            if (!element_type.array_has_size())
            {
                WARNING_MESSAGE("Array type does not have size", 0);
                return false;
            }

            // Compute dimension alignment
            Nodecl::NodeclBase dimension_size_node
                = element_type.array_get_size();

            // If VLA, get the actual size
            if (dimension_size_node.is<Nodecl::Symbol>()
                && dimension_size_node.get_symbol().is_saved_expression())
            {
                dimension_size_node
                    = dimension_size_node.get_symbol().get_value().no_conv();
            }

            int dimension_size = -1;
            if (dimension_size_node.is_constant())
            {
                dimension_size = const_value_cast_to_signed_int(
                                     dimension_size_node.get_constant())
                                 * n_type.get_size();
            }
            // If dimension size is suitable
            else if (suitable_visitor.walk(dimension_size_node)
                     == (int) target_alignment_in_elements)
            {
                dimension_size = target_alignment_in_elements;
            }
            if (VERBOSE)
                printf("Dim %d, size %d\n", i, dimension_size);

            dimension_sizes[i] = dimension_size;
        }

        int it_alignment = -1;
        Nodecl::List::iterator it = subscripts.begin();
        // Multiply dimension sizes by indexes
        for (i = 0; it != subscripts.end(); i++)
        {
            it_alignment = suitable_visitor.walk(*it);

            it++;
            if (it == subscripts.end())
                break; // Last dimmension does not have to be multiplied

            // a[i][j][k] -> i -> i*J*K
            for (int j = i; j < (num_subscripts - 1); j++)
            {
                /*
                   if( ( is_suitable_constant( dimension_sizes[j] ) ) ||
                   is_suitable_constant( it_alignment ) )
                   {
                   it_alignment = 0;
                   }
                   else
                 */
                //                    if( ( dimension_sizes[j] == -1 ) || (
                //                    it_alignment == -1 ) )
                if ((dimension_sizes[j] != -1))
                {
                    if (it_alignment == -1)
                        it_alignment = dimension_sizes[j];
                    else
                        it_alignment *= dimension_sizes[j];
                }
                else
                {
                    it_alignment = -1;
                }
            }

            if (it_alignment == -1)
            {
                return false;
            }

            alignment += it_alignment;
        }

        if (it_alignment == -1)
        {
            return false;
        }

        // Add adjacent dimension
        alignment += it_alignment * n_type.get_size();

        alignment_module = alignment % target_alignment;
        if (alignment_module == 0)
        {
            return true;
        }

        return false;
    }

    bool is_aligned_access(const Nodecl::NodeclBase &scope,
                           const Nodecl::Dereference &n,
                           const map_nodecl_int_t &aligned_expressions,
                           const objlist_nodecl_t &suitable_expressions,
                           unsigned int vec_factor,
                           unsigned int target_alignment,
                           int &alignment_module,
                           VectorizationAnalysisInterface *analysis)
    {
        int alignment;
        alignment_module = -1;

        Nodecl::NodeclBase deref_element = n.get_rhs().no_conv();

        ERROR_CONDITION(!deref_element.is<Nodecl::Symbol>(),
                        "Unexpected Derefence to kind %s\n",
                        ast_print_node_type(deref_element.get_kind()));

        alignment = get_pointer_alignment(
                scope, deref_element, aligned_expressions, analysis);

        if (alignment == -1)
        {
            // There is no alignment info about the subscripted symbol
            // Assume unaligned
            return false;
        }

        alignment_module = alignment % target_alignment;
        if (alignment_module == 0)
        {
            return true;
        }

        return false;
    }

    bool is_simd_aligned_access_internal(
            const Nodecl::NodeclBase& scope,
            const Nodecl::NodeclBase& n,
            const map_nodecl_int_t& aligned_expressions,
            const objlist_nodecl_t& suitable_expressions,
            int vec_factor, int alignment,
            int& alignment_module,
            VectorizationAnalysisInterface* analysis)
    {
        if(n.is<Nodecl::ArraySubscript>())
        {
            return is_aligned_access(scope,
                                     n.as<Nodecl::ArraySubscript>(),
                                     aligned_expressions,
                                     suitable_expressions,
                                     vec_factor,
                                     alignment,
                                     alignment_module,
                                     analysis);
        }
        else if (n.is<Nodecl::Dereference>())
        {
            return is_aligned_access(scope,
                                     n.as<Nodecl::Dereference>(),
                                     aligned_expressions,
                                     suitable_expressions,
                                     vec_factor,
                                     alignment,
                                     alignment_module,
                                     analysis);
        }
        else
        {
            std::cerr << "warning: returning false for is_simd_aligned_access when asking for nodecl '"
                      << n.prettyprint( ) << ". Not supported" << std::endl;
            return false;
        }
    }

    bool is_suitable_expression_internal(
        const Nodecl::NodeclBase &scope,
        const Nodecl::NodeclBase &n,
        const objlist_nodecl_t &suitable_expressions,
        unsigned int suitable_factor,
        unsigned int vec_factor,
        int &suitable_module,
        VectorizationAnalysisInterface *analysis)
    {
        bool result = false;

        SuitableVisitor sa_v(scope,
                             suitable_expressions,
                             suitable_factor,
                             vec_factor,
                             analysis);
        int suitable_value = sa_v.walk( n );

        suitable_module = (suitable_value == -1) ?
                              suitable_value :
                              (suitable_value % suitable_factor);

        if (suitable_module == 0)
            result = true;

        return result;
    }
}
}
