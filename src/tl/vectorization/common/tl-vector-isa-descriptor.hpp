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

#ifndef TL_VECTOR_ISA_DESCRIPTOR_HPP
#define TL_VECTOR_ISA_DESCRIPTOR_HPP

#include "tl-vectorization-common.hpp"
#include "tl-type.hpp"

namespace TL
{
namespace Vectorization
{

enum MaskingSupport
{
    SUPPORT_MASKING,
    DONT_SUPPORT_MASKING,
};

class VectorIsaDescriptor
{
  protected:
    // In bytes for SIMD, in elements for RoMoL
    // Please, do not implement a _vector_length getter!
    // Use get_vec_factor_from_type
    const std::string _id;
    const unsigned int _vector_length;
    const unsigned int _mask_size_elements;
    const MaskingSupport _masking_supported;

    VectorIsaDescriptor(const std::string &id,
                         unsigned int vector_length,
                         unsigned int mask_size_elements,
                         MaskingSupport masking_supported);

  public:
    const std::string& get_id() const;
    bool support_masking() const;
    unsigned int get_mask_max_elements() const;

    virtual unsigned int get_vec_factor_from_type(
        const TL::Type target_type) const = 0;
    virtual unsigned int get_vec_factor_for_type(
        const TL::Type target_type, unsigned int input_vec_factor) const = 0;

    virtual unsigned int get_memory_alignment_in_bytes() const = 0;
};

class SimdIsa : public VectorIsaDescriptor
{
  public:
    SimdIsa(const std::string& id,
            unsigned int vector_length,
            unsigned int mask_size_elements,
            MaskingSupport masking_supported);

    unsigned int get_vec_factor_from_type(const TL::Type target_type) const;
    unsigned int get_vec_factor_for_type(const TL::Type target_type,
                                         unsigned int input_vec_factor) const;

    unsigned int get_memory_alignment_in_bytes() const;
};

class VectorIsa : public VectorIsaDescriptor
{
  public:
    VectorIsa(const std::string& id,
              unsigned int vector_length,
              unsigned int mask_size_elements,
              MaskingSupport masking_supported);

    unsigned int get_vec_factor_from_type(const TL::Type target_type) const;
    unsigned int get_vec_factor_for_type(const TL::Type target_type,
                                         unsigned int input_vec_factor) const;

    unsigned int get_memory_alignment_in_bytes() const;
};

VectorIsaDescriptor &get_vector_isa_description(const VectorInstructionSet isa);

}
}

#endif // TL_VECTOR_ISA_DESCRIPTOR_HPP
