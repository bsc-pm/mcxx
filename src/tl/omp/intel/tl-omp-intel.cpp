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

#include "tl-omp-intel.hpp"
#include "tl-cache-rtl-calls.hpp"
#include "tl-lowering-visitor.hpp"
#include "tl-lowering-utils.hpp"

namespace TL { namespace Intel {

    Lowering::Lowering()
        : _simd_reductions_knc(false)
    {
        set_phase_name("Intel OpenMP RTL lowering");
        set_phase_description("This phase lowers from Mercurium parallel IR into real code involving Intel OpenMP RTL");

        register_parameter("omp_dry_run",
                "Disables OpenMP transformation",
                _openmp_dry_run,
                "0");

        register_parameter("simd-reduction-knc",
                "Emits reductions using SIMD instructions from KNC",
                _simd_reductions_knc_str,
                "0").connect(std::bind(&Lowering::set_simd_reduction_knc, this, std::placeholders::_1));
    }

    void Lowering::pre_run(DTO& dto)
    {
    }

    void Lowering::run(DTO& dto)
    {
        if (_openmp_dry_run != "0")
        {
            std::cerr << "Not running Intel OpenMP RTL phase (by request)" << std::endl;
            return;
        }

        std::cerr << "Intel OpenMP RTL phase" << std::endl;

        Nodecl::NodeclBase n = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);
        LoweringVisitor lowering_visitor(this);
        lowering_visitor.walk(n);

        CacheRTLCalls cache_calls_visitor(this);
        cache_calls_visitor.walk(n);
    }

    void Lowering::set_simd_reduction_knc(const std::string &str)
    {
        parse_boolean_option("simd-reduction-knc", str, _simd_reductions_knc, "Assuming false.");
    }

    void Lowering::set_instrumentation(const std::string& str)
    {
        parse_boolean_option("instrument", str, _instrumentation_enabled, "Assuming false.");
    }


    bool Lowering::instrumentation_enabled() const
    {
        return _instrumentation_enabled;
    }

    bool Lowering::simd_reductions_knc() const
    {
        return _simd_reductions_knc;
    }

    void Lowering::phase_cleanup(DTO& data_flow)
    {
        Intel::cleanup_lock_map();
    }
} }


EXPORT_PHASE(TL::Intel::Lowering);
