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

#include "tl-vectorizer-vector-reduction.hpp"

#include "tl-vectorization-utils.hpp"

namespace TL
{
    namespace Vectorization
    {
        VectorizerVectorReduction::VectorizerVectorReduction(const VectorizerEnvironment& environment)
            : _environment(environment)
        {
        }

        namespace
        {
            std::string canonicalize_reduction_name(const std::string& red_name)
            {
                if (IS_C_LANGUAGE)
                    return red_name;

                // C++
                std::string operator_ = "operator ";
                if (red_name.substr(0, operator_.size()) == operator_)
                {
                    // "operator #" -> "#"
                    return red_name.substr(operator_.size());
                }
                else
                {
                    return red_name;
                }
            }
        }

        bool VectorizerVectorReduction::is_supported_reduction(bool is_builtin,
                const std::string& reduction_name,
                const TL::Type& reduction_type)
        {
            if(is_builtin)
            {
                std::string red_name = canonicalize_reduction_name(reduction_name);

                // TODO: Improve implementation without using strings
                if (_environment._vec_isa_desc.get_id().compare("smp") == 0)
                {
                    if((red_name.compare("+") == 0) ||
                            (red_name.compare("-") == 0))
                    {
                        if(reduction_type.is_signed_int())
                        {
                            return true;
                        }
                        else if(reduction_type.is_float())
                        {
                            return true;
                        }
                        else if (reduction_type.is_double())
                        {
                            return true;
                        }
                    }
                }
                else if (_environment._vec_isa_desc.get_id().compare("avx2")
                         == 0)
                {
                    if((red_name.compare("+") == 0) ||
                            (red_name.compare("-") == 0))
                    {
                        if(reduction_type.is_signed_int())
                        {
                            return true;
                        }
                        else if(reduction_type.is_float())
                        {
                            return true;
                        }
                        else if (reduction_type.is_double())
                        {
                            return true;
                        }
                    }
                }
                else if (_environment._vec_isa_desc.get_id().compare("knc")
                         == 0)
                {
                    if((red_name.compare("+") == 0) ||
                            (red_name.compare("-") == 0))
                    {
                        if(reduction_type.is_signed_int())
                        {
                            return true;
                        }
                        else if(reduction_type.is_float())
                        {
                            return true;
                        }
                        else if (reduction_type.is_double())
                        {
                            return true;
                        }
                    }
                }
                else if (_environment._vec_isa_desc.get_id().compare("romol")
                         == 0)
                {
                    if((red_name.compare("+") == 0) ||
                            (red_name.compare("-") == 0))
                    {
                        if(reduction_type.is_signed_int())
                        {
                            return true;
                        }
                        else if(reduction_type.is_float())
                        {
                            return true;
                        }
                        else if (reduction_type.is_double())
                        {
                            return true;
                        }
                    }
                }
            }

            return false;
        }

        void VectorizerVectorReduction::vectorize_reduction(const TL::Symbol& scalar_symbol,
                TL::Symbol& vector_symbol,
                const Nodecl::NodeclBase& reduction_initializer,
                const std::string& reduction_name,
                const TL::Type& reduction_type,
                Nodecl::List& pre_nodecls,
                Nodecl::List& post_nodecls)
        {
            // Step1: ADD REDUCTION SYMBOLS
            vector_symbol.set_value(Nodecl::VectorPromotion::make(
                        reduction_initializer.shallow_copy(),
                        Utils::get_null_mask(),
                        vector_symbol.get_type()));

            // Add new ObjectInit with the initialization
            Nodecl::ObjectInit reduction_object_init =
                Nodecl::ObjectInit::make(vector_symbol);

            pre_nodecls.append(reduction_object_init);

            std::string red_name = canonicalize_reduction_name(reduction_name);

            // Step2: ADD VECTOR REDUCTION INSTRUCTIONS
            if(red_name.compare("+") == 0)
            {
                Nodecl::ExpressionStatement post_reduction_stmt =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::AddAssignment::make(
                                scalar_symbol.make_nodecl(true),
                                Nodecl::VectorReductionAdd::make(
                                    vector_symbol.make_nodecl(true),
                                    Utils::get_null_mask(),
                                    scalar_symbol.get_type()),
                            scalar_symbol.get_type()));

                post_nodecls.append(post_reduction_stmt);
            }
            else if (red_name.compare("-") == 0)
            {
                Nodecl::ExpressionStatement post_reduction_stmt =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::AddAssignment::make(
                                scalar_symbol.make_nodecl(true),
                                Nodecl::VectorReductionMinus::make(
                                    vector_symbol.make_nodecl(true),
                                    Utils::get_null_mask(),
                                    scalar_symbol.get_type()),
                                scalar_symbol.get_type()));

                post_nodecls.append(post_reduction_stmt);
            }
            else
            {
                internal_error("Code unreachable reduction_name='%s'", reduction_name.c_str());
            }
        }
    }
}

