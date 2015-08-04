/*--------------------------------------------------------------------
  (C) Copyright 2015-2015 Barcelona Supercomputing Center
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


#include "tl-nanos6.hpp"
#include "tl-nanos6-lower.hpp"
#include "tl-compilerpipeline.hpp"
#include "codegen-phase.hpp"
#include "cxx-profile.h"
#include "cxx-driver-utils.h"

#include <errno.h>

namespace TL { namespace Nanos6 {

    LoweringPhase::LoweringPhase()
    {
        set_phase_name("Nanos 6 lowering");
        set_phase_description("This phase lowers from Mercurium parallel IR "
                "into real code involving the Nanos 6 runtime interface");

        // std::cerr << "Initializing Nanos 6 lowering phase" << std::endl;
    }

    void LoweringPhase::run(DTO& dto)
    {
        std::cerr << "Nanos 6 phase" << std::endl;

        FORTRAN_LANGUAGE()
        {
            this->fortran_load_api(dto);
        }

        Nodecl::NodeclBase translation_unit =
            *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);

        Lower lower(this);
        lower.walk(translation_unit);
    }

    void LoweringPhase::pre_run(DTO& dto)
    {
        std::cerr << "Nanos 6 prerun" << std::endl;
    }

    void LoweringPhase::phase_cleanup(DTO& dto)
    {
        if (_extra_c_code.is_null())
            return;

        std::string original_filename = TL::CompilationProcess::get_current_file().get_filename();
        std::string new_filename = "nanos6_extra_code_" + original_filename  + ".c";

        FILE* ancillary_file = fopen(new_filename.c_str(), "w");
        if (ancillary_file == NULL)
        {
            running_error("%s: error: cannot open file '%s'. %s\n",
                    original_filename.c_str(),
                    new_filename.c_str(),
                    strerror(errno));
        }

        source_language_t prev_lang = CURRENT_CONFIGURATION->source_language;
        CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_C;

        compilation_configuration_t* configuration = ::get_compilation_configuration("auxcc");
        ERROR_CONDITION (configuration == NULL, "auxcc profile is mandatory when there is extra C code", 0);

        // Make sure phases are loaded (this is needed for codegen)
        load_compiler_phases(configuration);

        TL::CompilationProcess::add_file(new_filename, "auxcc");

        ::mark_file_for_cleanup(new_filename.c_str());

        Codegen::CodegenPhase* phase = reinterpret_cast<Codegen::CodegenPhase*>(configuration->codegen_phase);
        phase->codegen_top_level(_extra_c_code, ancillary_file, new_filename);

        CURRENT_CONFIGURATION->source_language = prev_lang;

        fclose(ancillary_file);

        // Do not forget to clear the node for next files
        _extra_c_code = Nodecl::List();
    }

} }

EXPORT_PHASE(TL::Nanos6::LoweringPhase);
