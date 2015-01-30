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

#ifndef TL_VECTORIZER_REPORT_HPP
#define TL_VECTORIZER_REPORT_HPP

#include "tl-nodecl-visitor.hpp"
#include "tl-nodecl-base.hpp"


namespace TL
{
namespace Vectorization
{
    class VectorizerReport : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            int _vloads;
            int _aligned_vloads;
            int _unaligned_vloads;

            int _vstores;
            int _aligned_vstores;
            int _unaligned_vstores;

            int _vgathers;
            int _vscatters;

            int _vpromotions;

        public:
            VectorizerReport();

            void reset_report();
            void print_report(const Nodecl::NodeclBase& n);
            void visit(const Nodecl::ObjectInit& n);

            void visit(const Nodecl::VectorLoad& n);
            void visit(const Nodecl::VectorStore& n);

            void visit(const Nodecl::VectorGather& n);
            void visit(const Nodecl::VectorScatter& n);

            void visit(const Nodecl::VectorPromotion& n);
    };
}
}

#endif //TL_VECTORIZER_REPORT_HPP

