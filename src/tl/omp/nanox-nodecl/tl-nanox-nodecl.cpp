/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "tl-nanox-nodecl.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-lowering-visitor.hpp"
#include "tl-compilerpipeline.hpp"
#include "codegen-phase.hpp"
#include "cxx-profile.h"
#include "cxx-driver-utils.h"
#include <stdio.h>
#include <errno.h>

namespace TL { namespace Nanox {

    Lowering::Lowering()
    {
        set_phase_name("Nanos++ lowering");
        set_phase_description("This phase lowers from Mercurium parallel IR into real code involving Nanos++ runtime interface");

        register_parameter("omp_dry_run",
                "Disables OpenMP transformation",
                _openmp_dry_run,
                "0");
    }

    void Lowering::run(DTO& dto)
    {
        if (_openmp_dry_run != "0")
        {
            std::cerr << "Not running Nanos++ phase (by request)" << std::endl;
            return;
        }


        std::cerr << "Nanos++ phase" << std::endl;

        Nodecl::NodeclBase n = dto["nodecl"];

        LoweringVisitor lowering_visitor;
        lowering_visitor.walk(n);

        finalize_phase(n);
    }

    void Lowering::finalize_phase(Nodecl::NodeclBase global_node)
    {
        set_openmp_programming_model(global_node);
    }

    void Lowering::set_openmp_programming_model(Nodecl::NodeclBase global_node)
    {
        Source src;
        // if (!_static_weak_symbols)
        // {
        src 
            << "__attribute__((weak, section(\"nanos_init\"))) nanos_init_desc_t __section__nanos_init = { nanos_omp_set_interface, (void*)0 };"
            ;
        // }
        // else
        // {
        //     // Some compilers (like ICC) may require this
        //     src 
        //         << "static __attribute__((section(\"nanos_init\"))) nanos_init_desc_t __section__nanos_init = { nanos_omp_set_interface, (void*)0 };"
        //         ;
        // }

        FORTRAN_LANGUAGE()
        {
            // Parse in C
            Source::source_language = SourceLanguage::C;
        }
        Nodecl::NodeclBase n = src.parse_global(global_node);
        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::Current;
        }

        if (!IS_FORTRAN_LANGUAGE)
        {
            Nodecl::Utils::append_to_top_level_nodecl(n);
        }
        else
        {
            CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_C;

            std::string file_name = "aux_omp_file_" + TL::CompilationProcess::get_current_file().get_filename(/* fullpath */ false) + ".c";
            FILE* new_file = fopen(file_name.c_str(), "w");
            if (new_file == NULL)
            {
                running_error("%s: error: cannot open file '%s'. %s\n", 
                        TL::CompilationProcess::get_current_file().get_filename().c_str(),
                        file_name.c_str(),
                        strerror(errno));
            }

            compilation_configuration_t* configuration = ::get_compilation_configuration("auxcc");
            ERROR_CONDITION (configuration == NULL, "auxcc profile is mandatory when using Fortran", 0);

            // Make sure phases are loaded (this is needed for codegen)
            load_compiler_phases(configuration);

            TL::CompilationProcess::add_file(file_name, "auxcc");

            Codegen::CodegenPhase* phase = reinterpret_cast<Codegen::CodegenPhase*>(configuration->codegen_phase);
            phase->codegen_top_level(n, new_file);

            fclose(new_file);

            ::mark_file_for_cleanup(file_name.c_str());

            CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_FORTRAN;
        }

    }

} }


EXPORT_PHASE(TL::Nanox::Lowering);
