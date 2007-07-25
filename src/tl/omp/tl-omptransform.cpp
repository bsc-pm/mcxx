/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "tl-omptransform.hpp"

namespace TL
{
    OpenMPTransform::OpenMPTransform()
        : already_initialized(false), parallel_nesting(0), transaction_nesting(0)
    {
        // Set phase info
        set_phase_name("Nanos 4 OpenMP implementation");
        set_phase_description("Implementation of OpenMP for Nanos 4 runtime");

        // Register parameters for this phase
        register_parameter("nanos_new_interface", 
                "If set to '1' will use the new interface for all parallel constructs",
                nanos_new_interface_str,
                "0").connect(functor(&OpenMPTransform::set_parallel_interface, *this));
        register_parameter("instrument",
                "Enables mintaka instrumentation if set to '1'",
                enable_mintaka_instr_str,
                "0")/*.connect(functor(&OpenMPTransform::set_instrumentation, *this))*/;

        // No signals for these as their values are passed to the object initialization function
        register_parameter("function_filter_name", 
                "File for filtering function calls within #pragma transaction",
                function_filter_name_str,
                "./functions_to_replace_call_filter");
        register_parameter("function_filter_mode",
                "Filter mode for filtering function calls within '#pragma transaction'. It can be 'normal' or 'inverted'",
                function_filter_mode_str,
                "normal");
    }

    void OpenMPTransform::set_parallel_interface(const std::string& str)
    {
        enable_nth_create = 0;
        if (str == "1"
                || str == "yes"
                || str == "true")
        {
            enable_nth_create = 1;
        }
        else if (str != "0"
                && str != "no"
                && str != "false")
        {
            std::cerr 
                << get_phase_name() << ": Invalid value '" << str << "'" <<
                "for parameter 'nanos_new_interface'. Old interface will be used for parallel spawns." 
                << std::endl;
        }
    }

    void OpenMPTransform::set_instrumentation(const std::string& str)
    {
        enable_mintaka_instr = 0;
        if (str == "1"
                || str == "yes"
                || str == "true")
        {
            enable_mintaka_instr = 1;
        }
        else if (str != "0"
                && str != "no"
                && str != "false")
        {
            std::cerr 
                << get_phase_name() << ": Invalid value '" << str << "'" <<
                "for parameter 'instrument'. Instrumentation disabled" 
                << std::endl;
        }
    }

    bool OpenMPTransform::instrumentation_requested()
    {
        return enable_mintaka_instr;
    }

    OpenMPTransform::~OpenMPTransform()
    {
        // This is needed since "init" is a virtual method
    }

    void OpenMPTransform::init()
    {
        // This function is called in OpenMPPhase::run. The user
        // can register here the handlers that will be called for
        // every construction (in preorder and postorder)
        //
        // Register the handlers (callbacks) for every construction
        if (already_initialized)
            return;
        already_initialized = true;

        function_filter.init(function_filter_name_str, function_filter_mode_str);

        // #pragma omp parallel
        on_parallel_pre.connect(functor(&OpenMPTransform::parallel_preorder, *this));
        on_parallel_post.connect(functor(&OpenMPTransform::parallel_postorder, *this));

        // #pragma omp parallel for
        on_parallel_for_pre.connect(functor(&OpenMPTransform::parallel_for_preorder, *this));
        on_parallel_for_post.connect(functor(&OpenMPTransform::parallel_for_postorder, *this));

        // #pragma omp for
        on_for_pre.connect(functor(&OpenMPTransform::for_preorder, *this));
        on_for_post.connect(functor(&OpenMPTransform::for_postorder, *this));

        // #pragma omp parallel sections 
        on_parallel_sections_pre.connect(functor(&OpenMPTransform::parallel_sections_preorder, *this));
        on_parallel_sections_post.connect(functor(&OpenMPTransform::parallel_sections_postorder, *this));

        // #pragma omp sections
        on_sections_pre.connect(functor(&OpenMPTransform::sections_preorder, *this));
        on_sections_pre.connect(functor(&OpenMPTransform::sections_postorder, *this));

        // #pragma omp section
        on_section_post.connect(functor(&OpenMPTransform::section_postorder, *this));

        // #pragma omp barrier
        on_barrier_post.connect(functor(&OpenMPTransform::barrier_postorder, *this));

        // #pragma omp atomic
        on_atomic_post.connect(functor(&OpenMPTransform::atomic_postorder, *this));

        // #pragma omp ordered
        on_ordered_post.connect(functor(&OpenMPTransform::ordered_postorder, *this));

        // #pragma omp master
        on_master_post.connect(functor(&OpenMPTransform::master_postorder, *this));

        // #pragma omp single
        on_single_post.connect(functor(&OpenMPTransform::single_postorder, *this));

        // #pragma omp parallel single
        on_parallel_single_pre.connect(functor(&OpenMPTransform::parallel_single_preorder, *this));
        on_parallel_single_post.connect(functor(&OpenMPTransform::parallel_single_postorder, *this));

        // #pragma omp critical
        on_critical_post.connect(functor(&OpenMPTransform::critical_postorder, *this));

        // #pragma omp flush
        on_flush_post.connect(functor(&OpenMPTransform::flush_postorder, *this));

        // #pragma omp threadprivate
        on_threadprivate_post.connect(functor(&OpenMPTransform::threadprivate_postorder, *this));

        // #pragma omp task
        on_custom_construct_post["task"].connect(functor(&OpenMPTransform::task_postorder, *this));

        // #pragma omp directive taskwait
        on_custom_construct_post["taskwait"].connect(functor(&OpenMPTransform::taskwait_postorder, *this));

        // #pragma omp directive taskgroup
        on_custom_construct_post["taskgroup"].connect(functor(&OpenMPTransform::taskgroup_postorder, *this));

        // #pragma omp directive taskyield
        on_custom_construct_post["taskyield"].connect(functor(&OpenMPTransform::taskyield_postorder, *this));

        // --- Transactional world --
        // #pragma omp construct transaction
        on_custom_construct_pre["transaction"].connect(functor(&OpenMPTransform::transaction_preorder, *this));
        on_custom_construct_post["transaction"].connect(functor(&OpenMPTransform::transaction_postorder, *this));
        
		// #pragma omp directive retry
		on_custom_construct_post["retry"].connect(functor(&OpenMPTransform::retry_postorder, *this));

        // #pragma omp construct preserve
        on_custom_construct_post["preserve"].connect(functor(&OpenMPTransform::preserve_postorder, *this));
        // --- End of transactional world --

        // --- Experimental directives ---
        // #pragma omp construct while
        on_custom_construct_pre["task_while"].connect(functor(&OpenMPTransform::task_while_preorder, *this));
        on_custom_construct_post["task_while"].connect(functor(&OpenMPTransform::task_while_postorder, *this));

        on_custom_construct_pre["task_for"].connect(functor(&OpenMPTransform::task_for_preorder, *this));
        on_custom_construct_post["task_for"].connect(functor(&OpenMPTransform::task_for_postorder, *this));
        // --- End of experimental directives ---

    }
}

EXPORT_PHASE(TL::OpenMPTransform);
