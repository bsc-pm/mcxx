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

#ifndef TL_OMP_SIMD_VISITOR_HPP
#define TL_OMP_SIMD_VISITOR_HPP

#include "tl-vector-isa-descriptor.hpp"
#include "tl-vectorizer.hpp"


namespace TL
{
namespace OpenMP
{
class SimdProcessingBase
{
  protected:
    TL::Vectorization::Vectorizer &_vectorizer;

    const TL::Vectorization::VectorIsaDescriptor& _vector_isa_desc;
    bool _fast_math_enabled;
    bool _overlap_in_place;

    SimdProcessingBase(Vectorization::VectorInstructionSet simd_isa,
                       bool fast_math_enabled,
                       bool svml_enabled,
                       bool only_adjacent_accesses,
                       bool only_aligned_accesses,
                       bool overlap_in_place);
};

class SimdVisitor : public Nodecl::ExhaustiveVisitor<void>,
                    public SimdProcessingBase
{
  protected:
    void common_simd_function(const Nodecl::OpenMP::SimdFunction &simd_node,
                              const bool masked_version);

  public:
    SimdVisitor(Vectorization::VectorInstructionSet simd_isa,
                bool fast_math_enabled,
                bool svml_enabled,
                bool only_adjacent_accesses,
                bool only_aligned_accesses,
                bool overlap_in_place);
    ~SimdVisitor();

    virtual void visit(const Nodecl::FunctionCode &func_code);
    virtual void visit(const Nodecl::OpenMP::Simd &simd_node);
    virtual void visit(const Nodecl::OpenMP::SimdFor &simd_node);
    virtual void visit(const Nodecl::OpenMP::SimdFunction &simd_node);

    virtual void visit(const Nodecl::TemplateFunctionCode &func_code);
};

class SimdPreregisterVisitor : public Nodecl::ExhaustiveVisitor<void>,
                               public SimdProcessingBase
{
  protected:
    void common_simd_function_preregister(
        const Nodecl::OpenMP::SimdFunction &simd_node,
        const bool masked_version);

  public:
    SimdPreregisterVisitor(Vectorization::VectorInstructionSet simd_isa,
                           bool fast_math_enabled,
                           bool svml_enabled,
                           bool only_adjacent_accesses,
                           bool only_aligned_accesses,
                           bool overlap_in_place);
    ~SimdPreregisterVisitor();

    virtual void visit(const Nodecl::OpenMP::SimdFunction &simd_node);
};

class FunctionDeepCopyFixVisitor : public Nodecl::ExhaustiveVisitor<void>
{
  private:
    const TL::Symbol &_orig_symbol;
    const TL::Symbol &_new_symbol;

  public:
    FunctionDeepCopyFixVisitor(const TL::Symbol &orig_symbol,
                               const TL::Symbol &new_symbol);

    virtual void visit(const Nodecl::Symbol &n);
};
}
}

#endif // TL_OMP_SIMD_VISITOR_HPP
