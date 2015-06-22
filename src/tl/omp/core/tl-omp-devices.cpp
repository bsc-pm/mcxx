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




#include "tl-omp-core.hpp"
#include "cxx-diagnostic.h"

namespace TL { namespace OpenMP {

    void Core::omp_target_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::omp_target_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::target_data_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::target_data_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::target_update_handler_pre(TL::PragmaCustomDirective ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::target_update_handler_post(TL::PragmaCustomDirective ctr) { }

    void Core::teams_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::teams_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::distribute_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::distribute_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::distribute_parallel_for_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::distribute_parallel_for_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::distribute_parallel_do_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::distribute_parallel_do_handler_post(TL::PragmaCustomStatement ctr) { }

    // Combined
    void Core::target_teams_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::target_teams_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::teams_distribute_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::teams_distribute_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::teams_distribute_parallel_for_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::teams_distribute_parallel_for_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::teams_distribute_parallel_do_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::teams_distribute_parallel_do_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::target_teams_distribute_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::target_teams_distribute_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::target_teams_distribute_parallel_for_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::target_teams_distribute_parallel_for_handler_post(TL::PragmaCustomStatement ctr) { }

    void Core::target_teams_distribute_parallel_do_handler_pre(TL::PragmaCustomStatement ctr) {
        error_printf("%s: error: OpenMP 4.0 construct not implemented yet\n", ctr.get_locus_str().c_str());
    }
    void Core::target_teams_distribute_parallel_do_handler_post(TL::PragmaCustomStatement ctr) { }

} }
