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

#include "tl-vector-isa-descriptor.hpp"

namespace TL
{
namespace Vectorization
{
VectorIsaDescriptor::VectorIsaDescriptor(const std::string& id,
                                           unsigned int vector_length,
                                           unsigned int mask_size_elements,
                                           MaskingSupport masking_supported)
    : _id(id),
      _vector_length(vector_length),
      _mask_size_elements(mask_size_elements),
      _masking_supported(masking_supported)
{
}

const std::string& VectorIsaDescriptor::get_id() const
{
    return _id;
}

bool VectorIsaDescriptor::support_masking() const
{
    if (_masking_supported == SUPPORT_MASKING) return true;
    else if (_masking_supported == DONT_SUPPORT_MASKING) return false;
    else
    {
       fatal_error("Unexpected value for masking supported"); 
    }
}

unsigned int VectorIsaDescriptor::get_mask_max_elements() const
{
    return _mask_size_elements;
}

SimdIsa::SimdIsa(const std::string &id,
                 unsigned int vector_length,
                 unsigned int mask_size_elements,
                 MaskingSupport masking_supported)
    : VectorIsaDescriptor(
          id, vector_length, mask_size_elements, masking_supported)
{
}

unsigned int SimdIsa::get_vec_factor_from_type(const TL::Type target_type) const
{
    if (target_type.is_void()) // Assume void == float
    {
        return TL::Type::get_float_type().get_size();
    }
    else
    {
        return _vector_length / target_type.get_size();
    }
}

unsigned int SimdIsa::get_vec_factor_for_type(const TL::Type target_type,
                                              unsigned int input_vec_factor) const
{
    unsigned int raw_vec_factor = get_vec_factor_from_type(target_type);

    if (input_vec_factor >= raw_vec_factor) 
        return input_vec_factor;
    else
    {
        return raw_vec_factor;
    }
}

unsigned int SimdIsa::get_memory_alignment_in_bytes() const
{
    return _vector_length;
}


VectorIsa::VectorIsa(const std::string &id,
                     unsigned int vector_length,
                     unsigned int mask_size_elements,
                     MaskingSupport masking_supported)
    : VectorIsaDescriptor(id, vector_length, mask_size_elements, masking_supported)
{
}

unsigned int VectorIsa::get_vec_factor_from_type(
    const TL::Type target_type) const
{
    return _vector_length;
}

unsigned int VectorIsa::get_vec_factor_for_type(
    const TL::Type target_type, unsigned int input_vec_factor) const
{
    return input_vec_factor;
}

unsigned int VectorIsa::get_memory_alignment_in_bytes() const
{
    return 0; // No alignment restrictions
}

namespace {
    // id, vector length, mask size in elements, masking support
    SimdIsa sse42("smp", 16, 0, DONT_SUPPORT_MASKING);
    SimdIsa avx2("avx2", 32, 0, DONT_SUPPORT_MASKING);
    SimdIsa knc("knc", 64, 16, SUPPORT_MASKING);
    SimdIsa knl("knl", 64, 16, SUPPORT_MASKING);
    SimdIsa neon("neon", 16, 0, DONT_SUPPORT_MASKING);
    VectorIsa romol("romol", 64, 64, SUPPORT_MASKING); // vector length in elements
}


VectorIsaDescriptor &get_vector_isa_description(const VectorInstructionSet isa)
{
    switch (isa)
    {
        case SSE4_2_ISA:
            return sse42;
        case AVX2_ISA:
            return avx2;
        case KNC_ISA:
            return knc;
        case KNL_ISA:
            return knl;
        case NEON_ISA:
            return neon;
        case ROMOL_ISA:
            return romol;
        default:
            fatal_error("SIMD: Unsupported SIMD ISA: %d", isa);
    }
}

}
}

