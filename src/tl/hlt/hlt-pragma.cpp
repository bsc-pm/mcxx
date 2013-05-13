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

#include "tl-pragmasupport.hpp"

#include "hlt-pragma.hpp"

namespace TL { namespace HLT {

HLTPragmaPhase::HLTPragmaPhase()
    : PragmaCustomCompilerPhase("hlt")
{
    set_phase_name("High Level Transformations");
    set_phase_description("This phase implements several high level "
            "transformations available through the usage of #pragma hlt");

    // register_construct("unroll");
    // on_directive_post["unroll"].connect(functor(&HLTPragmaPhase::unroll_loop, *this));

    // register_construct("block");
    // on_directive_post["block"].connect(functor(&HLTPragmaPhase::block_loop, *this));

    // register_construct("blocking");
    // on_directive_post["blocking"].connect(functor(&HLTPragmaPhase::block_loop, *this));

    // register_construct("stripmine");
    // on_directive_post["stripmine"].connect(functor(&HLTPragmaPhase::stripmine_loop, *this));

    // register_construct("distribute");
    // on_directive_post["distribute"].connect(functor(&HLTPragmaPhase::distribute_loop, *this));

    // register_construct("fusion");
    // on_directive_pre["fusion"].connect(functor(&HLTPragmaPhase::pre_fuse_loops, *this));
    // on_directive_post["fusion"].connect(functor(&HLTPragmaPhase::fuse_loops, *this));

    // register_construct("interchange");
    // on_directive_post["interchange"].connect(functor(&HLTPragmaPhase::interchange_loops, *this));

    // register_construct("collapse");
    // on_directive_post["collapse"].connect(functor(&HLTPragmaPhase::collapse_loop, *this));

    // register_construct("outline");
    // on_directive_post["outline"].connect(functor(&HLTPragmaPhase::outline_code, *this));

    // register_construct("extend");
    // on_directive_post["extend"].connect(functor(&HLTPragmaPhase::extend_function, *this));

    // register_construct("peel");
    // on_directive_post["peel"].connect(functor(&HLTPragmaPhase::peel_loop, *this));

    // register_construct("task_aggregate");
    // on_directive_post["task_aggregate"].connect(functor(&HLTPragmaPhase::task_aggregate, *this));

    // register_construct("simd");
    // on_directive_post["simd"].connect(functor(&HLTPragmaPhase::simdize, *this));

    // _allow_identity_str = "1";

    // register_parameter("allow_identity", 
    //         "Use this to disable identity, this is for testing only",
    //         _allow_identity_str,
    //         "true").connect(functor( update_identity_flag ));

    // register_parameter("instrument",
    //         "Enables mintaka instrumentation if set to '1'",
    //         _enable_hlt_instr_str,
    //         "0").connect(functor( &HLTPragmaPhase::set_instrument_hlt, *this ));

    // register_parameter("acml",
    //         "Enables ACML library in SIMD regions if set to '1'",
    //         _enable_hlt_acml_str,
    //         "0").connect(functor( &HLTPragmaPhase::set_acml_hlt, *this ));

    // register_parameter("interm-simd",
    //         "Enables Intermediate SIMD code prettyprint if set to '1'",
    //         _enable_hlt_intermediate_simd_prettyprint,
    //         "0").connect(functor( &HLTPragmaPhase::set_intermediate_simd_prettyprint, *this ));

}

} }

EXPORT_PHASE(TL::HLT::HLTPragmaPhase)
