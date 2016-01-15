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


#ifndef TL_LOWER_REDUCTIONS_HPP
#define TL_LOWER_REDUCTIONS_HPP

#include "tl-lowering-utils.hpp"

#include "tl-omp.hpp"

namespace TL { namespace Intel {

    TL::Symbol declare_reduction_pack(const TL::ObjectList<TL::Symbol> &sym,
            Nodecl::NodeclBase location);

    TL::Symbol emit_callback_for_reduction(
            CombinerISA combiner_isa,
            TL::ObjectList<Nodecl::OpenMP::ReductionItem> &reduction_items,
            TL::Type reduction_pack_type,
            Nodecl::NodeclBase location,
            TL::Symbol current_function);

    TL::Symbol emit_callback_for_reduction_scalar(
            TL::ObjectList<Nodecl::OpenMP::ReductionItem> &reduction_items,
            TL::Type reduction_pack_type,
            Nodecl::NodeclBase location,
            TL::Symbol current_function);


    struct SIMDReductionPair
    {
        TL::Symbol horizontal_combiner;
        TL::Symbol vertical_combiner;
    };

    SIMDReductionPair emit_callback_for_reduction_simd(
            CombinerISA isa,
            Nodecl::OpenMP::ReductionItem &reduction_item,
            Nodecl::NodeclBase location,
            TL::Symbol current_function);

    TL::Symbol emit_array_of_reduction_simd_functions(
            const TL::ObjectList<SIMDReductionPair>& pairs,
            Nodecl::NodeclBase location,
            TL::Symbol current_function);

    void update_reduction_uses(Nodecl::NodeclBase node,
            const TL::ObjectList<Nodecl::OpenMP::ReductionItem>& reduction_items,
            TL::Symbol reduction_pack_symbol);

    struct ReplaceInOutMaster : Nodecl::ExhaustiveVisitor<void>
    {
        TL::Symbol _field;
        TL::Symbol _orig_omp_in, _reduction_pack_symbol;
        TL::Symbol _orig_omp_out, _reduced_symbol;

        ReplaceInOutMaster(
                TL::Symbol field,
                TL::Symbol orig_omp_in,  TL::Symbol reduction_pack_symbol,
                TL::Symbol orig_omp_out, TL::Symbol reduced_symbol);

        virtual void visit(const Nodecl::Symbol& node);
    };
} }

#endif // TL_LOWER_REDUCTIONS_HPP
