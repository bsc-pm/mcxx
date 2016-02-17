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

#include "tl-vectorizer-overlap-common.hpp"
#include "tl-vectorization-utils.hpp"
#include "tl-expression-reduction.hpp"
#include "tl-optimizations.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL
{
namespace Vectorization
{
    void OverlapGroup::compute_basic_properties()
    {
        const Nodecl::NodeclBase& front_load =
            _loads.front();

        _vector_type = front_load.get_type().no_ref().get_unqualified_type();
        _basic_type = front_load.get_type().no_ref().basic_type();
        _is_set_in_place_update_pre = false;

        compute_inter_iteration_overlap();
    }
    
    void OverlapGroup::compute_inter_iteration_overlap()
    {
        Nodecl::Add next_iv =
            Nodecl::Add::make(_loop_ind_var.shallow_copy(),
                    _loop_ind_var_step.shallow_copy(),
                    _loop_ind_var.get_type());

        for(objlist_nodecl_t::const_iterator next_it = _loads.begin();
                next_it != _loads.end();
                next_it++)
        {
            Nodecl::VectorLoad next_vl = next_it->shallow_copy().
                as<Nodecl::VectorLoad>();

            Nodecl::Utils::nodecl_replace_nodecl_by_structure(
                    next_vl, _loop_ind_var, next_iv);

            if (overlaps(next_vl, false /*consider aligned adjacent accesses */))
            {
                _inter_it_overlap = true;         // By overlap-related property
                return;
            }
        }

        _inter_it_overlap = false;
    }

    bool OverlapGroup::overlaps(const Nodecl::VectorLoad& vector_load,
            const bool consider_aligned_adjacent_accesses)
    {
        int VF = vector_load.get_type().vector_num_elements();

        Nodecl::NodeclBase vl_subscripts =
            Utils::get_vector_load_subscript(vector_load);

        //TODO: Get aligned vector version for unaligned vector loads
        //      and use them in the UntaryReduct

        for(objlist_nodecl_t::iterator it = _loads.begin();
                it != _loads.end();
                it++)
        {
            Nodecl::NodeclBase it_subscripts =
                Utils::get_vector_load_subscript(
                        it->as<Nodecl::VectorLoad>());

            Nodecl::Minus minus = Nodecl::Minus::make(
                    vl_subscripts.no_conv().shallow_copy(),
                    it_subscripts.no_conv().shallow_copy(),
                    vl_subscripts.get_type());

            TL::Optimizations::UnitaryReductor unitary_reductor;
            unitary_reductor.reduce(minus);

            VECTORIZATION_DEBUG()
            {
                std::cerr << "Difference: " << vl_subscripts.prettyprint()
                    << " MINUS " << it_subscripts.prettyprint()
                    << " = "
                    << minus.prettyprint() << " DIFFERENCE IS" << (minus.is_constant() ? " " : " NOT ") << "CONSTANT"
                    << std::endl;
            }

            if (consider_aligned_adjacent_accesses)
            {
                if (minus.is_constant() && 
                        abs(const_value_cast_to_signed_int(minus.get_constant())) <= VF)
                    return true;
            }
            else
            {
                if (minus.is_constant() && 
                        abs(const_value_cast_to_signed_int(minus.get_constant())) < VF)
                    return true;
            }
        }

        return false;
    }


    void OverlapGroup::compute_leftmost_rightmost_vloads(
            const Vectorization::VectorizerEnvironment& environment,
            const int max_registers)
    {
        leftmost_rightmost_strategy(environment, 
                true /* aligned vl strategy */);
        compute_num_registers(environment);
        _aligned_strategy = true;

        if (max_registers != 0 && _num_registers > max_registers)
        {
            leftmost_rightmost_strategy(environment,
                    false /* unaligned vl strategy */);
            _aligned_strategy = false;
            compute_num_registers(environment);
        }

        ERROR_CONDITION(max_registers != 0 && _num_registers > max_registers,
                "Register limit is too low to apply overlap optimization", 0);
    }

    void OverlapGroup::leftmost_rightmost_strategy(
            const Vectorization::VectorizerEnvironment& environment,
            const bool aligned_strategy)
    {
        Nodecl::VectorLoad first_vload = 
            _loads.begin()->as<Nodecl::VectorLoad>();

        Nodecl::NodeclBase first_subscript =
            Utils::get_vector_load_subscript(first_vload);

        VECTORIZATION_DEBUG()
        {
            std::cerr << "First subscript: " 
                << first_subscript.prettyprint()
                << std::endl;
        }

        int min_offset = 0;
        int max_offset = 0;

        Nodecl::VectorLoad min_vload = first_vload;
        Nodecl::VectorLoad max_vload = first_vload;

        // Find the leftmost (min) vload and the rightmost (max)
        // and compute their offsets (num elements) from the first_subscript
        for(objlist_nodecl_t::const_iterator load_it =
                _loads.begin();
                load_it != _loads.end();
                load_it++)
        {
            Nodecl::NodeclBase load_subscript =
                Utils::get_vector_load_subscript(
                        load_it->as<Nodecl::VectorLoad>());

            Nodecl::Minus shifted_elements = Nodecl::Minus::make(
                    load_subscript.shallow_copy(),
                    first_subscript.shallow_copy(),
                    load_subscript.get_type());

            TL::Optimizations::UnitaryReductor unitary_reductor;
            unitary_reductor.reduce(shifted_elements);

            if (shifted_elements.is_constant())
            {
                int offset = const_value_cast_to_signed_int(
                        shifted_elements.get_constant());

                if (offset < min_offset)
                {
                    min_offset = offset;
                    min_vload = load_it->as<Nodecl::VectorLoad>();
                }

                if (offset > max_offset)
                {
                    max_offset = offset;
                    max_vload = load_it->as<Nodecl::VectorLoad>();
                }
            }
        }

        _leftmost_code_vload = min_vload;
        _rightmost_code_vload = max_vload;
      
        // ALIGNED STRATEGY 
        if (aligned_strategy)
        {
            // min_vload = the leftmost aligned load
            Nodecl::List flags = min_vload.get_flags().as<Nodecl::List>();

            bool aligned = !(flags.find_first<Nodecl::AlignedFlag>().is_null());

            if (!aligned)
            {
                Nodecl::NodeclBase alignment_node = flags.find_first<Nodecl::AlignmentInfo>();
                if (alignment_node.is_null())
                    fatal_error("Overlap error (MIN): There is no alignment info for %s",
                            min_vload.prettyprint().c_str());

                int alignment = const_value_cast_to_signed_int(alignment_node.get_constant());

                int min_vload_type_size = min_vload.get_type().basic_type().get_size();
                int negative_num_elements = alignment/min_vload_type_size;
                
                VECTORIZATION_DEBUG()
                {
                    std::cerr << "OVERLAP ALIGNMENT: " << alignment 
                        << " negative offset " << negative_num_elements
                        << " num elements" << std::endl;
                }

                // New flags
                Nodecl::List new_flags = flags.shallow_copy().as<Nodecl::List>();
                new_flags.append(Nodecl::AlignedFlag::make());
                Nodecl::Utils::remove_from_enclosing_list(
                        new_flags.find_first<Nodecl::AlignmentInfo>());

                // New aligned array
                Nodecl::ArraySubscript new_array = Utils::get_scalar_memory_access(
                        min_vload).shallow_copy().as<Nodecl::ArraySubscript>();
                Nodecl::NodeclBase subscript = new_array.get_subscripts().
                    as<Nodecl::List>().front().no_conv();

                const_value_t* const_int = const_value_get_signed_int(
                        negative_num_elements);
                const_value_t* neg_int = const_value_neg(const_int);

                Nodecl::Neg neg = Nodecl::Neg::make(
                        const_value_to_nodecl(const_int),
                        subscript.get_type(),
                        subscript.get_locus());
                neg.set_constant(neg_int);


                Nodecl::NodeclBase new_subscript;
                if (subscript.is_constant())
                {
                    new_subscript = const_value_to_nodecl(
                            const_value_add(neg_int,
                                subscript.get_constant()));
                }
                else
                {
                    new_subscript = Nodecl::Add::make(
                            subscript.shallow_copy(),
                            neg,
                            subscript.get_type(),
                            subscript.get_locus());
                }

                subscript.replace(new_subscript);

                Nodecl::VectorLoad aligned_vector_load =
                    Nodecl::VectorLoad::make(
                            Nodecl::Reference::make(
                                new_array.shallow_copy(),
                                new_array.get_type().get_pointer_to(),
                                new_array.get_locus()),
                            min_vload.get_mask().shallow_copy(),
                            new_flags,
                            min_vload.get_type(),
                            min_vload.get_locus());

                min_vload = aligned_vector_load;
                min_offset = min_offset - negative_num_elements;

                Optimizations::canonicalize_and_fold(
                        min_vload, false /*fast math*/);
            }

            // max_vload = the rightmost aligned load
            flags = max_vload.get_flags().as<Nodecl::List>();
            aligned = !(flags.find_first<Nodecl::AlignedFlag>().is_null());

            if (!aligned)
            {
                Nodecl::NodeclBase alignment_node = flags.find_first<Nodecl::AlignmentInfo>();
                if (alignment_node.is_null())
                    fatal_error("Overlap error (MAX): There is no alignment info for %s",
                            max_vload.prettyprint().c_str());

                int alignment = const_value_cast_to_signed_int(alignment_node.get_constant());

                int max_vload_type_size = max_vload.get_type().basic_type().get_size();
                int positive_num_elements = environment._vec_factor -
                    alignment/max_vload_type_size;

                VECTORIZATION_DEBUG()
                {
                    std::cerr << "OVERLAP ALIGNMENT: " << alignment 
                        << " positive offset " << positive_num_elements
                        << " num elements" << std::endl;
                }

                // New flags ***************************************
                Nodecl::List new_flags = flags.shallow_copy().as<Nodecl::List>();
                new_flags.append(Nodecl::AlignedFlag::make());
                Nodecl::Utils::remove_from_enclosing_list(
                        new_flags.find_first<Nodecl::AlignmentInfo>());

                // New aligned array
                Nodecl::ArraySubscript new_array = Utils::get_scalar_memory_access(
                        max_vload).shallow_copy().as<Nodecl::ArraySubscript>();
                Nodecl::NodeclBase subscript = new_array.get_subscripts().
                    as<Nodecl::List>().front().no_conv();

                const_value_t* const_int = const_value_get_signed_int(
                        positive_num_elements);

                Nodecl::NodeclBase new_subscript;
                if (subscript.is_constant())
                {
                    new_subscript = const_value_to_nodecl(
                            const_value_add(const_int,
                                subscript.get_constant()));
                }
                else
                {
                    new_subscript = Nodecl::Add::make(
                            subscript.shallow_copy(),
                            const_value_to_nodecl(const_int),
                            subscript.get_type(),
                            subscript.get_locus());
                }

                subscript.replace(new_subscript);

                Nodecl::VectorLoad aligned_vector_load =
                    Nodecl::VectorLoad::make(
                            Nodecl::Reference::make(
                                new_array.shallow_copy(),
                                new_array.get_type().get_pointer_to(),
                                new_array.get_locus()),
                            max_vload.get_mask().shallow_copy(),
                            new_flags,
                            max_vload.get_type(),
                            max_vload.get_locus());

                max_vload = aligned_vector_load;
                max_offset = max_offset + positive_num_elements;

                Optimizations::canonicalize_and_fold(
                        max_vload, false /*fast math*/);
            }
        }
        // UNALIGNED STRATEGY
        else
        {
            Nodecl::NodeclBase leftmost_index =
                Utils::get_vector_load_subscript(min_vload);
            Nodecl::NodeclBase rightmost_index =
                Utils::get_vector_load_subscript(max_vload);

            Nodecl::Minus minus = Nodecl::Minus::make(
                    rightmost_index.no_conv().shallow_copy(),
                    leftmost_index.no_conv().shallow_copy(),
                    leftmost_index.get_type());

            TL::Optimizations::UnitaryReductor unitary_reductor;
            unitary_reductor.reduce(minus);

            const_value_t* mod = const_value_mod(
                    minus.get_constant(),
                    const_value_get_signed_int(
                        environment._vec_factor));

            if (!const_value_is_zero(mod))
            {
                int positive_num_elements = environment._vec_factor -
                    const_value_cast_to_signed_int(mod);
                
                // Max vload flags == Min vload flags
                Nodecl::List new_flags =
                    min_vload.get_flags().shallow_copy().as<Nodecl::List>();

                // New array
                Nodecl::ArraySubscript new_array = Utils::get_scalar_memory_access(
                        max_vload).shallow_copy().as<Nodecl::ArraySubscript>();
                Nodecl::NodeclBase subscript = new_array.get_subscripts().
                    as<Nodecl::List>().front().no_conv();

                const_value_t* const_int = const_value_get_signed_int(
                        positive_num_elements);

                Nodecl::NodeclBase new_subscript;
                if (subscript.is_constant())
                {
                    new_subscript = const_value_to_nodecl(
                            const_value_add(const_int,
                                subscript.get_constant()));
                }
                else
                {
                    new_subscript = Nodecl::Add::make(
                            subscript.shallow_copy(),
                            const_value_to_nodecl(const_int),
                            subscript.get_type(),
                            subscript.get_locus());
                }

                subscript.replace(new_subscript);

                Nodecl::VectorLoad vector_load =
                    Nodecl::VectorLoad::make(
                            Nodecl::Reference::make(
                                new_array.shallow_copy(),
                                new_array.get_type().get_pointer_to(),
                                new_array.get_locus()),
                            max_vload.get_mask().shallow_copy(),
                            new_flags,
                            max_vload.get_type(),
                            max_vload.get_locus());

                max_vload = vector_load;
                max_offset = max_offset + positive_num_elements;

                Optimizations::canonicalize_and_fold(
                        max_vload, false /*fast math*/);
            }
        }

        _leftmost_group_vload = min_vload;
        _rightmost_group_vload = max_vload;

        if (aligned_strategy)
        {
            VECTORIZATION_DEBUG()
            {
                std::cerr << "ALIGNED STRATEGY: " << std::endl;
            }
        }
        else
        {
            VECTORIZATION_DEBUG()
            {
                std::cerr << "UNALIGNED STRATEGY: " << std::endl;
            }
        }

        VECTORIZATION_DEBUG()
        {
            std::cerr << "Min index is " << _leftmost_group_vload.prettyprint()
                << " with " << min_offset << " offset" << std::endl;
            std::cerr << "Max index is " << _rightmost_group_vload.prettyprint()
                << " with " << max_offset << " offset" << std::endl;
        }
    }

    void OverlapGroup::compute_num_registers(
            const Vectorization::VectorizerEnvironment& environment)
    {
        Nodecl::NodeclBase leftmost_index =
            Utils::get_vector_load_subscript(_leftmost_group_vload);
        Nodecl::NodeclBase rightmost_index =
            Utils::get_vector_load_subscript(_rightmost_group_vload);

        Nodecl::Minus minus = Nodecl::Minus::make(
                rightmost_index.no_conv().shallow_copy(),
                leftmost_index.no_conv().shallow_copy(),
                leftmost_index.get_type());

        TL::Optimizations::UnitaryReductor unitary_reductor;
        unitary_reductor.reduce(minus);

        const_value_t* mod = const_value_mod(
                minus.get_constant(),
                const_value_get_signed_int(
                    environment._vec_factor));
        const_value_t* div = const_value_div(
                minus.get_constant(),
                const_value_get_signed_int(
                    environment._vec_factor));

        VECTORIZATION_DEBUG()
        {
            std::cerr << "Rightmost: " << rightmost_index.prettyprint()
                << " MINUS Leftmost: " << leftmost_index.prettyprint()
                << " = "
                << minus.prettyprint()
                << ". Mod = " << const_value_cast_to_signed_int(mod)
                << ". Div = " << const_value_cast_to_signed_int(div)
                << std::endl;
        }

        ERROR_CONDITION(!const_value_is_zero(mod),
                "Leftmost and Rightmost are not multiple of VL", 0);

        _num_registers = const_value_cast_to_signed_int(div) + 1;
    }
 


    objlist_ogroup_t get_overlap_groups(
            const objlist_nodecl_t& vector_loads,
            const int min_group_loads,
            const int max_group_registers,
            const int max_groups,
            const Nodecl::NodeclBase& loop_ind_var,
            const Nodecl::NodeclBase& loop_ind_var_step,
            const bool consider_aligned_adjacent_accesses)
    {
        objlist_ogroup_t ogroups;
        
        objlist_nodecl_t unique_vector_loads;

        // Remove repeated
        for (auto& vl : vector_loads)
        {
            if (!Nodecl::Utils::list_contains_nodecl_by_structure(
                        unique_vector_loads, vl))
            {
                unique_vector_loads.append(vl);
            }
        }

        for (auto& target_load : unique_vector_loads)
        {
            Nodecl::NodeclBase target_subscripted = Utils::get_vector_load_subscripted(
                   target_load.as<Nodecl::VectorLoad>()); 
            TL::Symbol target_symbol = Utils::get_subscripted_symbol(target_subscripted);

            Nodecl::VectorLoad target_load_copy =
                target_load.shallow_copy().as<Nodecl::VectorLoad>();

            bool og_found = false;
            for(auto& it_ogroup : ogroups)
            {
                if (Utils::get_subscripted_symbol(it_ogroup._subscripted) != target_symbol)
                    continue;
                
                if(it_ogroup.overlaps(target_load_copy,
                            consider_aligned_adjacent_accesses))
                {
                    //std::cerr << target_load_copy.prettyprint() << " overlap!"<< std::endl;
                    target_load.replace(target_load_copy);
                    it_ogroup._loads.append(target_load);

                    og_found = true;
                    break;
                }
            }

            if (!og_found) // NEW GROUP
            {
                OverlapGroup ogroup;

                VECTORIZATION_DEBUG()
                {
                    std::cerr << "Building a new Overlap Group for "
                        << target_load_copy.prettyprint()
                        << std::endl;
                }

                target_load.replace(target_load_copy);
   
                ogroup._loads.append(target_load);
                ogroup._loop_ind_var = loop_ind_var;
                ogroup._loop_ind_var_step = loop_ind_var_step;
                ogroup._subscripted = target_subscripted;

                ogroups.append(ogroup);
            }
        }

        std::cerr << "Overlap Groups Summary:" << std::endl;
        std::cerr << "    - Total groups = " << ogroups.size() << ":" << std::endl;

        VECTORIZATION_DEBUG()
        {
            for(const auto& ogroup : ogroups)
            {
                std::cerr << Utils::get_subscripted_symbol(ogroup._subscripted).get_name() <<
                    "(" << ogroup._loads.size() << ") ";
            }
        }
 
        // TODO: Merge overlaped groups
        /*
        std::cerr << std::endl << 
            "    - Groups after merging: "
            << ogroups.size() << " ";

        for(const auto& ogroup : ogroups)
        {
            std::cerr << ogroup._subscripted.prettyprint() <<
                "(" << ogroup->_loads.size() << ") ";
        }
        */
 
        // TODO: Length
        //if (group._loads.size() >= min_group_size)
        //    result.append(group);
        
        for(objlist_ogroup_t::iterator it_ogroup =
                ogroups.begin();
                it_ogroup != ogroups.end();)
        {
            if (it_ogroup->_loads.size() < 
                    (unsigned int) min_group_loads)
            {
                ogroups.erase(it_ogroup);
            }
            else
            {
                it_ogroup++;
            }
        }

        std::cerr << std::endl << "    - Groups after cardinality (" <<
           min_group_loads << ") = " << ogroups.size()
            << ": " << std::endl;

        VECTORIZATION_DEBUG()
        {
            for(const auto& ogroup : ogroups)
            {
                std::cerr << Utils::get_subscripted_symbol(ogroup._subscripted).get_name() <<
                    "(" << ogroup._loads.size() << ") ";
            }
            std::cerr << std::endl;
        }

        return ogroups;
    }

}}


