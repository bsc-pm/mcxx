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

#ifndef TL_SUITABLE_ALIGNMENT_VISITOR_HPP
#define TL_SUITABLE_ALIGNMENT_VISITOR_HPP

#include "tl-suitable-alignment-visitor.hpp"

#include "tl-induction-variables-data.hpp"
#include "tl-extensible-graph.hpp"

#include "tl-nodecl-visitor.hpp"
//#include "tl-objectlist.hpp"
//#include "tl-omp.hpp"

namespace TL {
namespace Vectorization{

    class LIBTL_CLASS SuitableAlignmentVisitor : public Nodecl::NodeclVisitor<int>
    {
    private:
        const Nodecl::NodeclBase _scope;
        const TL::ObjectList<Nodecl::NodeclBase> _suitable_expressions;
        const int _unroll_factor;
        const int _type_size;
        const int _alignment;

        bool is_suitable_expression( Nodecl::NodeclBase n );
        bool is_suitable_constant( int n );

    public:
        // *** Constructor *** //
        SuitableAlignmentVisitor( const Nodecl::NodeclBase& scope,
                                  const TL::ObjectList<Nodecl::NodeclBase>& suitable_expressions,
                                  int unroll_factor, int type_size, int alignment );

        // *** Visiting methods *** //
        Ret join_list( ObjectList<int>& list );
        bool is_aligned_access( const Nodecl::ArraySubscript& n,
                const std::map<TL::Symbol, int> aligned_expressions );

        Ret visit( const Nodecl::Add& n );
        Ret visit( const Nodecl::ArraySubscript& n );
        Ret visit( const Nodecl::BitwiseShl& n );
        Ret visit( const Nodecl::BitwiseShr& n );
        Ret visit( const Nodecl::Conversion& n );
        Ret visit( const Nodecl::IntegerLiteral& n );
        Ret visit( const Nodecl::Neg& n );
        Ret visit( const Nodecl::Minus& n );
        Ret visit( const Nodecl::Mul& n );
        Ret visit( const Nodecl::ParenthesizedExpression& n );
        Ret visit( const Nodecl::Symbol& n );

        Ret unhandled_node(const Nodecl::NodeclBase& n);
    };
}
}

#endif // TL_SUITABLE_ALIGNMENT_VISITOR_HPP
