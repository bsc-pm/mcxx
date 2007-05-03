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
        : parallel_nesting(0), transaction_nesting(0)
    {
    }

    bool OpenMPTransform::instrumentation_requested()
    {
        return (ExternalVars::get("instrument", "0") == "1");
    }

    OpenMPTransform::~OpenMPTransform()
    {
        // This is needed since "init" is a virtual method
    }

    void OpenMPTransform::init()
    {
        if (ExternalVars::get("nanos_new_interface", "0") != "1")
        {
            std::cerr << "OpenMP: Using old interface 'nthf_create_1s_vp_' for parallel spawn." << std::endl;
            std::cerr << "OpenMP: Use '--variable=nanos_new_interface:1' to enable the newer interface" << std::endl;
        }

        // This function is called in OpenMPPhase::run. The user
        // can register here the handlers that will be called for
        // every construction (in preorder and postorder)
        //
        // Register the handlers (callbacks) for every construction

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

        // #pragma omp construct transaction
        on_custom_construct_pre["transaction"].connect(functor(&OpenMPTransform::transaction_preorder, *this));
        on_custom_construct_post["transaction"].connect(functor(&OpenMPTransform::transaction_postorder, *this));

		// #pragma omp directive retry
		on_custom_construct_post["retry"].connect(functor(&OpenMPTransform::retry_postorder, *this));
    }
}

EXPORT_PHASE(TL::OpenMPTransform);
