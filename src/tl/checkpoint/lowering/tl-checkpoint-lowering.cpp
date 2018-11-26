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

#include "tl-checkpoint-lowering.hpp"
#include "tl-checkpoint-visitor.hpp"

#include "tl-omp-lowering-utils.hpp"

#include "cxx-cexpr.h"
namespace TL { namespace Checkpoint {

    namespace
    {
        void fortran_fixup_api(int max_dims)
        {
            const char *entry_points[] = {
                "tcl_begin_load",
                "tcl_begin_store",
                "tcl_end_load",
                "tcl_end_store",
                "tcl_init",
                "tcl_shutdown",
                NULL
            };

            const char *multidimensional_entry_points[] = {
                "tcl_register_cpinfo",
                NULL
            };

            TL::OpenMP::Lowering::Utils::Fortran::fixup_entry_points(
                    entry_points, multidimensional_entry_points, max_dims);
        }
    }


    LoweringPhase::LoweringPhase()
    {
        set_phase_name("Checkpoint lowering");
        set_phase_description("This phase lowers from Mercurium'c checkpoint IR into real code involving calls to TCL");
    }

    void LoweringPhase::run(DTO& dto)
    {
        Nodecl::NodeclBase translation_unit =
            *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);

        FORTRAN_LANGUAGE()
        {
            TL::OpenMP::Lowering::Utils::Fortran::preprocess_api(translation_unit);
        }

        // Computing api_max_dimensions: this information is obtained from an enumerator defined in global scope
        {
            TL::Symbol max_dimensions_sym =
                TL::Scope::get_global_scope().get_symbol_from_name("__tcl_max_dimensions");
            ERROR_CONDITION(max_dimensions_sym.is_invalid(), "'__tcl_max_dimensions' symbol not found", 0);

            Nodecl::NodeclBase value = max_dimensions_sym.get_value();
            ERROR_CONDITION(value.is_null(), "'__tcl_max_dimensions' does not have a value", 0);
            ERROR_CONDITION(!value.is_constant(), "'__tcl_max_dimensions' should have a costant value", 0);

            _tcl_max_dims = const_value_cast_to_unsigned_int(value.get_constant());
        }

        FORTRAN_LANGUAGE()
        {
           fortran_fixup_api(_tcl_max_dims);
        }

        CheckpointVisitor visitor(this);
        visitor.walk(translation_unit);
    }

    int LoweringPhase::get_tcl_max_dims() const
    {
       return _tcl_max_dims;
    }
}}

EXPORT_PHASE(TL::Checkpoint::LoweringPhase);
