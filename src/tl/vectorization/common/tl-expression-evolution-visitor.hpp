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

#ifndef TL_EXPRESSION_EVOLUTION_VISITOR_HPP
#define TL_EXPRESSION_EVOLUTION_VISITOR_HPP

#include "tl-expression-evolution-visitor.hpp"

#include "tl-analysis-interface.hpp"
#include "tl-induction-variables-data.hpp"
#include "tl-extensible-graph.hpp"

#include "tl-nodecl-visitor.hpp"
//#include "tl-objectlist.hpp"
//#include "tl-omp.hpp"

#include <set>

namespace TL {
namespace Vectorization {

    class LIBTL_CLASS ExpressionEvolutionVisitor : public Nodecl::NodeclVisitor<bool>
    {
        private:
            Analysis::Node* _scope;
            const Analysis::NodeclSet _killed;                      /* All killed variables in the containing loop */
            Analysis::ExtensibleGraph* _pcfg;
            Analysis::Node* _scope_node;                            /* Scope from which the node is being analyzed */
            Analysis::Node* _n_node;                                /* Node in the PCFG containing the nodecl being analyzed */
            bool _is_adjacent_access;
            std::set<Nodecl::NodeclBase> _adjacency_visited_nodes;

            /*
            bool variable_is_iv( const Nodecl::NodeclBase& n );
               bool node_uses_iv( Node* node );
               bool node_stmts_depend_on_iv( Node* node, int recursion_level,
               std::map<Node*, std::set<int> >& visits,
               std::set<Nodecl::Symbol, Nodecl::Utils::Nodecl_structural_less>& visited_syms );
               bool definition_depends_on_iv( const Nodecl::NodeclBase& n, Node* node );
               bool var_is_iv_dependent_in_scope_backwards( const Nodecl::Symbol& n, Node* current,
               int recursion_level, std::map<Node*, std::set<int> >& visits,
               std::set<Nodecl::Symbol, Nodecl::Utils::Nodecl_structural_less>& visited_syms );
               bool var_is_iv_dependent_in_scope_forward( const Nodecl::Symbol& n, Node* current,
               int recursion_level, std::map<Node*, std::set<int> >& visits,
               std::set<Nodecl::Symbol, Nodecl::Utils::Nodecl_structural_less>& visited_syms );
               bool visit_binary_node( const Nodecl::NodeclBase& lhs, const Nodecl::NodeclBase& rhs );
               bool visit_unary_node( const Nodecl::NodeclBase& rhs );
             */

        public:
            // *** Constructor *** //
            ExpressionEvolutionVisitor(Analysis::Node* scope,
                    Analysis::Node* pcfg_node, Analysis::ExtensibleGraph* pcfg,
                    std::set<Nodecl::NodeclBase> adjacency_visited_nodes =
                    std::set<Nodecl::NodeclBase>());

            // *** Consultants *** //
            bool is_adjacent_access( );
            //bool depends_on_induction_vars( );
            //bool var_is_iv_dependent_in_scope( const Nodecl::Symbol& n );

            // *** Visiting methods *** //
            Ret unhandled_node( const Nodecl::NodeclBase& n );
            Ret join_list( ObjectList<bool>& list );

            Ret visit( const Nodecl::Add& n );
            Ret visit( const Nodecl::ArraySubscript& n );
            Ret visit( const Nodecl::Assignment& n );
            Ret visit( const Nodecl::BitwiseShl& n );
            Ret visit( const Nodecl::BitwiseShr& n );
            Ret visit( const Nodecl::BooleanLiteral& n );
            Ret visit( const Nodecl::ComplexLiteral& n );
            Ret visit( const Nodecl::Conversion& n );
            Ret visit( const Nodecl::Div& n );
            Ret visit( const Nodecl::FloatingLiteral& n );
            Ret visit( const Nodecl::FunctionCall& n );
            Ret visit( const Nodecl::IntegerLiteral& n );
            Ret visit( const Nodecl::LowerThan& n );
            Ret visit( const Nodecl::MaskLiteral& n );
            Ret visit( const Nodecl::Minus& n );
            Ret visit( const Nodecl::Mul& n );
            Ret visit( const Nodecl::Neg& n );
            Ret visit( const Nodecl::PointerToMember& n );
            Ret visit( const Nodecl::Postdecrement& n );
            Ret visit( const Nodecl::Postincrement& n );
            Ret visit( const Nodecl::Power& n );
            Ret visit( const Nodecl::Predecrement& n );
            Ret visit( const Nodecl::Preincrement& n );
            Ret visit( const Nodecl::Reference& n );
            Ret visit( const Nodecl::Sizeof& n );
            Ret visit( const Nodecl::StringLiteral& n );
            Ret visit( const Nodecl::Symbol& n );
    };
}
}

#endif // TL_EXPRESSION_EVOLUTION_VISITOR_HPP
