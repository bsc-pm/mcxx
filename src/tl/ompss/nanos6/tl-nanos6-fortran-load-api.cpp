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
#include "tl-source.hpp"

#include "cxx-driver-utils.h"
#include "cxx-utils.h"

#include <string>
#include <fstream>
#include <iomanip>

namespace TL { namespace Nanos6 {

namespace
{
const char *entry_points[] = {
    "nanos_create_task",
    "nanos_submit_task",
    "nanos_taskwait",
    "nanos_register_read_depinfo",
    "nanos_register_write_depinfo",
    "nanos_register_readwrite_depinfo",
    "nanos_register_weak_read_depinfo",
    "nanos_register_weak_write_depinfo",
    "nanos_register_weak_readwrite_depinfo",
    "nanos_user_lock",
    "nanos_user_unlock",
};

// This is kludgy: devise a way to do this in the FE
void fixup_entry_points()
{
    TL::Scope global_scope = TL::Scope::get_global_scope();
    for (const char **it = entry_points;
         it < (const char **)(&entry_points + 1);
         it++)
    {
        const char *str = *it;
        TL::Symbol sym = global_scope.get_symbol_from_name(str);

        ERROR_CONDITION(
            !sym.is_valid(), "Nanos 6 entry point '%s' not found", str);

        Source bind_name_src;
        bind_name_src << "\"" << str << "\"";
        Nodecl::NodeclBase bind_name
            = bind_name_src.parse_expression(global_scope);

        symbol_entity_specs_set_bind_info(
            sym.get_internal_symbol(),
            nodecl_make_fortran_bind_c(bind_name.get_internal_nodecl(),
                                       sym.get_locus()));
    }
}
}

    void LoweringPhase::fortran_load_api(DTO& dto)
    {
        ERROR_CONDITION(!IS_FORTRAN_LANGUAGE, "This is only for Fortran", 0);

        Nodecl::NodeclBase top_level = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);

        const char** old_preprocessor_options = CURRENT_CONFIGURATION->preprocessor_options;

        int num_orig_args = count_null_ended_array((void**)old_preprocessor_options);
        int num_args = num_orig_args;

        // -x c
        num_args += 2;

        // NULL ended
        num_args += 1;

        const char** preprocessor_options = new const char*[num_args];

        for (int i = 0;  i < num_orig_args; i++)
        {
            preprocessor_options[i] = old_preprocessor_options[i];
        }

        // We add -x c since we want /dev/null be preprocessed as an empty C file
        // FIXME - This is very gcc specific
        preprocessor_options[num_args - 3] = "-x";
        preprocessor_options[num_args - 2] = "c";
        preprocessor_options[num_args - 1] = NULL;

        CURRENT_CONFIGURATION->preprocessor_options = preprocessor_options;

        const char* output_filename = preprocess_file("/dev/null");

        delete[] preprocessor_options;

        // Restore old flags
        CURRENT_CONFIGURATION->preprocessor_options = old_preprocessor_options;

        TL::Source src;

        std::ifstream preproc_file(output_filename);

        if (preproc_file.is_open())
        {
            std::string str;

            while (preproc_file.good())
            {
                std::getline(preproc_file, str);
                src << str << "\n";
            }
            preproc_file.close();
        }
        else
        {
            fatal_error("Could not open Nanos++ include");
        }

        Source::source_language = SourceLanguage::C;

        Nodecl::NodeclBase new_tree = src.parse_global(top_level);
        // This is actually a top level tree!
        new_tree = Nodecl::TopLevel::make(new_tree);
        // FIXME - keep this?

        Source::source_language = SourceLanguage::Current;

        fixup_entry_points();
    }


} }
