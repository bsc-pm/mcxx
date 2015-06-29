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

#ifndef TL_STRENGTH_REDUCE_HPP
#define TL_STRENGTH_REDUCE_HPP

#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL
{
    namespace Optimizations
    {
        class StrengthReduction : public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                bool _fast_math;

            public:
                StrengthReduction(bool fast_math);

                virtual void visit(const Nodecl::ObjectInit& n);

                virtual void visit(const Nodecl::Mul& n);
                virtual void visit(const Nodecl::Div& n);
                virtual void visit(const Nodecl::Mod& n);
                virtual void visit(const Nodecl::VectorAdd& n);
                virtual void visit(const Nodecl::VectorMinus& n);
                virtual void visit(const Nodecl::VectorMul& n);
                virtual void visit(const Nodecl::VectorDiv& n);
                virtual void visit(const Nodecl::VectorMod& n);
                virtual void visit(const Nodecl::VectorSqrt& n);
        };
    }
}

#endif // TL_STRENGTH_REDUCE_HPP
