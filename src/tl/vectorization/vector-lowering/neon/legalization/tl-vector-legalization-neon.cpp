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

#include "tl-vector-legalization-neon.hpp"

#include "tl-vectorization-common.hpp"
#include "tl-vectorization-utils.hpp"

#include "tl-optimizations.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-source.hpp"


namespace TL { namespace Vectorization {

    NeonVectorLegalization::NeonVectorLegalization()
    {
        std::cerr << "--- NEON legalization phase ---" << std::endl;

    }

    void NeonVectorLegalization::visit(const Nodecl::FunctionCode& n)
    {
        bool contains_vector_nodes = TL::Vectorization::Utils::contains_vector_nodes(n);

        if (contains_vector_nodes)
        {
            // Initialize analisys
            TL::Optimizations::canonicalize_and_fold(
                    n, /*_fast_math_enabled*/ false);

            _analysis = new VectorizationAnalysisInterface(
                    n, Analysis::WhichAnalysis::REACHING_DEFS_ANALYSIS);

            walk(n.get_statements());
        }
    }

    void NeonVectorLegalization::visit(const Nodecl::ObjectInit& n)
    {
        TL::Source intrin_src;

        if(n.has_symbol())
        {
            TL::Symbol sym = n.get_symbol();

            // Vectorizing initialization
            Nodecl::NodeclBase init = sym.get_value();
            if(!init.is_null())
            {
                walk(init);
            }
        }
    }

    void NeonVectorLegalization::visit(const Nodecl::VectorConversion& n)
    {
        walk(n.get_nest());

    }

    void NeonVectorLegalization::visit(const Nodecl::VectorAssignment& n)
    {
        if (n.get_mask() != Nodecl::NodeclBase::null() &&
                _analysis->has_been_defined(n.get_lhs()))
        {
            ((Nodecl::VectorAssignment)n).set_has_been_defined(
                Nodecl::HasBeenDefinedFlag::make());
        }

        walk(n.get_lhs());
        walk(n.get_rhs());
        walk(n.get_mask());
        walk(n.get_has_been_defined());
    }

    void NeonVectorLegalization::visit(const Nodecl::VectorLoad& n)
    {
        const Nodecl::NodeclBase rhs = n.get_rhs();
        const Nodecl::NodeclBase mask = n.get_mask();
        const Nodecl::List flags = n.get_flags().as<Nodecl::List>();

        walk(rhs);
        walk(mask);
        walk(flags);
    }

    void NeonVectorLegalization::visit(
            const Nodecl::VectorStore& n)
    {
        const Nodecl::NodeclBase lhs = n.get_lhs();
        const Nodecl::NodeclBase rhs = n.get_rhs();
        const Nodecl::NodeclBase mask = n.get_mask();
        const Nodecl::List flags = n.get_flags().as<Nodecl::List>();

        walk(lhs);
        walk(rhs);
        walk(mask);
        walk(flags);
    }

} }
