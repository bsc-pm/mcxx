/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#ifndef TL_OMP_NANOX_HPP
#define TL_OMP_NANOX_HPP

#include "tl-omp.hpp"

namespace TL
{
namespace Nanox
{
    class OMPTransform : public OpenMP::OpenMPPhase
    {
        public:
            OMPTransform();
            virtual void phase_cleanup(DTO& data_flow);
            
        private:
            void parallel_postorder(PragmaCustomConstruct ctr);
            void parallel_for_postorder(PragmaCustomConstruct ctr);
            void task_postorder(PragmaCustomConstruct ctr);
            void taskwait_postorder(PragmaCustomConstruct ctr);
            void single_postorder(PragmaCustomConstruct ctr);
            void for_postorder(PragmaCustomConstruct ctr);
            void atomic_postorder(PragmaCustomConstruct ctr);
            void threadprivate_postorder(PragmaCustomConstruct ctr);
            void barrier_postorder(PragmaCustomConstruct ctr);
            void flush_postorder(PragmaCustomConstruct ctr);
            void critical_postorder(PragmaCustomConstruct ctr);
            void master_postorder(PragmaCustomConstruct ctr);

            // Functions for Reductions
            void declare_reduction_preorder(PragmaCustomConstruct ctr);

            void sections_preorder(PragmaCustomConstruct ctr);
            void sections_postorder(PragmaCustomConstruct ctr);

            void section_preorder(PragmaCustomConstruct ctr);
            void section_postorder(PragmaCustomConstruct ctr);

            void target_preorder(PragmaCustomConstruct ctr);
            void target_postorder(PragmaCustomConstruct ctr);
            std::map<std::string, bool> _registered_slicer;

            void unimplemented_yet(PragmaCustomConstruct construct);

            // Phase data
            bool _enable_instrumentation;
            std::string _enable_instrumentation_str;
            void set_instrumentation(const std::string& str);
            
            bool _compiler_alignment;
            std::string _compiler_alignment_str;
            void set_compiler_alignment(const std::string& str);

            std::string _do_not_create_translation_str;
            bool _do_not_create_translation_fun;
            void set_translation_function_flag(const std::string& str);
            
            std::string _no_nanox_calls_str;
            bool _no_nanox_calls;
            void set_no_nanox_calls_flag(const std::string& str);

            // Data that does not last between files
            ObjectList<Symbol> _converted_vlas;
            std::set<std::string> _lock_names;

            // Support
            Source get_single_guard(const std::string&);

            // Temporary data during traversal
            struct SectionInfo
            {
                AST_t placeholder;
                SectionInfo() : placeholder(NULL) { }
            };
            typedef ObjectList<SectionInfo> SectionInfoList;

            ObjectList<SectionInfoList> _section_info;

            virtual void run(TL::DTO& dto);

            void add_openmp_initializer(TL::DTO& dto);

            static Source get_wait_completion(Source arg, bool do_flush, AST_t ref_tree);
            static Source get_barrier_code(AST_t ref_tree);

            std::string _static_weak_symbols_str;
            bool _static_weak_symbols;
            void set_weaks_as_statics(const std::string& str);
    };

    const std::string NANOX_OUTLINE_COUNTER("nanox_outline_counter");

    Type compute_replacement_type_for_vla(Type type, 
            ObjectList<Source>::iterator dim_names_begin,
            ObjectList<Source>::iterator dim_names_end);
}
}

#endif // TL_OMP_NANOX_HPP
