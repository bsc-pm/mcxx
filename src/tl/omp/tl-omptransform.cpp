/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
    namespace Nanos4
    {
        OpenMPTransform::OpenMPTransform()
            : num_parallels(0),
            parallel_nesting(0),
            transaction_nesting(0),
            stm_log_file_opened(false),
            enable_mintaka_instr(false), 
            enable_nth_create(true), 
            disable_restrict_pointers(false),
            use_memcpy_always(true),
            stm_global_lock_enabled(false)
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
                    "0").connect(functor(&OpenMPTransform::set_instrumentation, *this));
            register_parameter("disable_restrict",
                    "Disables restricted pointers in outlines if set to '1'", 
                    disable_restrict_str,
                    "0").connect(functor(&OpenMPTransform::set_disable_restrict_pointers, *this));

            C_LANGUAGE()
            {
                use_memcpy_always = true;
            }
            CXX_LANGUAGE()
            {
                register_parameter("use_memcpy_always",
                        "In C++, always use 'memcpy' when copying arrays if set to '1'. In C, 'memcpy' is always used.",
                        use_memcpy_always_str,
                        "0").connect(functor(&OpenMPTransform::set_use_memcpy_always, *this));
            }

            // STM options
            register_parameter("STM_global_lock",
                    "Enables global lock interface for '#pragma omp transaction' regions. Disables STM memory tracking.",
                    stm_global_lock_enabled_str,
                    "0").connect(functor(&OpenMPTransform::set_stm_global_lock, *this));

            // No signals for these as their values are passed to the object initialization function
            register_parameter("STM_replace_functions_file", 
                    "Filter file of STM-replaced function calls",
                    stm_replace_functions_file,
                    "./stm_replace_functions_file");
            register_parameter("STM_replace_functions_mode",
                    "Filter mode when STM-replacing function calls. It can be 'normal' or 'inverted'",
                    stm_replace_functions_mode,
                    "normal");
            register_parameter("STM_wrap_functions_file",
                    "Filter file of STM-wrapped functions",
                    stm_wrap_functions_file,
                    "./stm_wrap_functions_file");
            register_parameter("STM_wrap_functions_mode",
                    "Filter mode when STM-wrapping functions. It can be either 'normal' or 'inverted'",
                    stm_wrap_functions_mode,
                    "normal");

            // Register callbacks for constructs and directives
            //
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
            on_sections_post.connect(functor(&OpenMPTransform::sections_postorder, *this));

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

            // #pragma omp critical
            on_critical_post.connect(functor(&OpenMPTransform::critical_postorder, *this));

            // #pragma omp flush
            on_flush_post.connect(functor(&OpenMPTransform::flush_postorder, *this));

            // #pragma omp threadprivate
            on_threadprivate_post.connect(functor(&OpenMPTransform::threadprivate_postorder, *this));

            // OMP 3.0 tasks
            on_task_construct_pre.connect(functor(&OpenMPTransform::task_preorder, *this));
            on_task_construct_post.connect(functor(&OpenMPTransform::task_postorder, *this));

            // // #pragma omp taskwait
            on_taskwait_post.connect(functor(&OpenMPTransform::taskwait_postorder, *this));

            // #pragma omp taskgroup
            register_directive("taskgroup");
            on_custom_construct_post["taskgroup"].connect(functor(&OpenMPTransform::taskgroup_postorder, *this));

            // #pragma omp taskyield
            register_directive("taskyield");
            on_custom_construct_post["taskyield"].connect(functor(&OpenMPTransform::taskyield_postorder, *this));
            // End of OMP 3.0 tasks

            // --- Transactional world --
            // #pragma omp transaction
            register_construct("transaction");
            on_custom_construct_pre["transaction"].connect(functor(&OpenMPTransform::stm_transaction_preorder, *this));
            on_custom_construct_post["transaction"].connect(functor(&OpenMPTransform::stm_transaction_postorder, *this));

            // #pragma omp retry
            register_directive("retry");
            on_custom_construct_post["retry"].connect(functor(&OpenMPTransform::stm_retry_postorder, *this));

            // #pragma omp preserve
            register_construct("preserve");
            on_custom_construct_post["preserve"].connect(functor(&OpenMPTransform::stm_preserve_postorder, *this));

            // #pragma omp adf
            register_construct("adf");
            on_custom_construct_pre["adf"].connect(functor(&OpenMPTransform::adf_task_preorder, *this));
            on_custom_construct_post["adf"].connect(functor(&OpenMPTransform::adf_task_postorder, *this));
            // --- End of transactional world --
        }

        void OpenMPTransform::set_disable_restrict_pointers(const std::string& str)
        {
            disable_restrict_pointers = false;
            parse_boolean_option(/* Parameter name */ "disable_restrict", 
                    /* Given value */ str, 
                    /* Computed bool */ disable_restrict_pointers, 
                    /* Error message */  "Restrict pointers will be enabled");
        }

        void OpenMPTransform::set_parallel_interface(const std::string& str)
        {
            enable_nth_create = false;
            parse_boolean_option(/* Parameter name */ "nanos_new_interface", 
                    /* Given value */ str, 
                    /* Computed bool */ enable_nth_create, 
                    /* Error message */  "Old interface will be used for parallel spawns");
        }

        void OpenMPTransform::set_instrumentation(const std::string& str)
        {
            enable_mintaka_instr = false;
            parse_boolean_option(/* Parameter name */ "instrument", 
                    /* Given value */ str, 
                    /* Computed bool */ enable_mintaka_instr, 
                    /* Error message */  "Instrumentation disabled");
        }

        void OpenMPTransform::set_stm_global_lock(const std::string& str)
        {
            stm_global_lock_enabled = false;
            parse_boolean_option(/* Parameter name */ "STM_global_lock", 
                    /* Given value */ str, 
                    /* Computed bool */ stm_global_lock_enabled, 
                    /* Error message */  "STM global lock disabled");
        }

        void OpenMPTransform::set_use_memcpy_always(const std::string& str)
        {
            use_memcpy_always = false;
            parse_boolean_option(/* Parameter name */ "use_memcpy_always",
                    /* Given value */ str,
                    /* Compiler bool */ use_memcpy_always,
                    /* Error message */ "Will use plain assignment when copying arrays");
        }

        bool OpenMPTransform::instrumentation_requested()
        {
            return enable_mintaka_instr;
        }

        OpenMPTransform::~OpenMPTransform()
        {
            // This is needed since "init" is a virtual method
        }

        void OpenMPTransform::init(DTO& dto)
        {
            // This function is called in OpenMPPhase::run
            ObjectList<std::string> keys = dto.get_keys();

            if (keys.contains(SERIALIZED_FUNCTIONS_INFO))
            {
                RefPtr<Object> obj = dto[SERIALIZED_FUNCTIONS_INFO];
                serialized_functions_info = RefPtr<SerializedFunctionsInfo>::cast_dynamic(obj);
            }
        }
        
        void OpenMPTransform::run(DTO& dto)
        {
            // Call the OpenMPPhase::run
            OpenMPPhase::run(dto);
        }
    }
}

EXPORT_PHASE(TL::Nanos4::OpenMPTransform);
