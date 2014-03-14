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

#include "tl-vectorizer-visitor-preprocessor.hpp"

namespace TL
{
    namespace Vectorization
    {
        VectorizerVisitorPreprocessor::VectorizerVisitorPreprocessor()
        {
        }

        void VectorizerVisitorPreprocessor::visit(const Nodecl::AddAssignment& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::Assignment assignment =
                Nodecl::Assignment::make(
                        n.get_lhs().shallow_copy(),
                        Nodecl::Add::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            n.get_type(),
                            n.get_locus()),
                        n.get_type(),
                        n.get_locus());

            n.replace(assignment);
        }

        void VectorizerVisitorPreprocessor::visit(const Nodecl::MinusAssignment& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::Assignment assignment =
                Nodecl::Assignment::make(
                        n.get_lhs().shallow_copy(),
                        Nodecl::Minus::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            n.get_type(),
                            n.get_locus()),
                        n.get_type(),
                        n.get_locus());

            n.replace(assignment);
        }

        void VectorizerVisitorPreprocessor::visit(const Nodecl::MulAssignment& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::Assignment assignment =
                Nodecl::Assignment::make(
                        n.get_lhs().shallow_copy(),
                        Nodecl::Mul::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            n.get_type(),
                            n.get_locus()),
                        n.get_type(),
                        n.get_locus());

            n.replace(assignment);
        }

        void VectorizerVisitorPreprocessor::visit(const Nodecl::DivAssignment& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::Assignment assignment =
                Nodecl::Assignment::make(
                        n.get_lhs().shallow_copy(),
                        Nodecl::Div::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            n.get_type(),
                            n.get_locus()),
                        n.get_type(),
                        n.get_locus());

            n.replace(assignment);
        }

        void VectorizerVisitorPreprocessor::visit(const Nodecl::ModAssignment& n)
        {
            walk(n.get_lhs());
            walk(n.get_rhs());

            const Nodecl::Assignment assignment =
                Nodecl::Assignment::make(
                        n.get_lhs().shallow_copy(),
                        Nodecl::Mod::make(
                            n.get_lhs().shallow_copy(),
                            n.get_rhs().shallow_copy(),
                            n.get_type(),
                            n.get_locus()),
                        n.get_type(),
                        n.get_locus());

            n.replace(assignment);
        }

        void VectorizerVisitorPreprocessor::visit(const Nodecl::ArraySubscript& n)
        {
//            Nodecl::NodeclBase new_array_subscript = Nodecl::Utils::linearize_array_subscript(n);
//            n.replace(new_array_subscript);
        }
    }
}
