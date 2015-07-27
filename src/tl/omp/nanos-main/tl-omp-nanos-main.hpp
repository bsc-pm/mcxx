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

#ifndef TL_OMP_SIMD_HPP
#define TL_OMP_SIMD_HPP

#include "tl-compilerphase.hpp"
#include "tl-devices.hpp"


using namespace TL::Nanox;

namespace TL
{
    namespace Nanox
    {
        //! This class transforms 
        class NanosMain : public TL::PragmaCustomCompilerPhase
        {
            public:
                NanosMain();

                virtual void run(TL::DTO& dto);
                virtual void pre_run(TL::DTO& dto);

                virtual ~NanosMain() { }

            private:
                Nodecl::NodeclBase _root;

                std::string _nanos_main_enabled_str;
                bool _nanos_main_enabled;
                void set_nanos_main(const std::string& str);

                std::string _instrumentation_enabled_str;
                bool _instrumentation_enabled;
                void set_instrumentation(const std::string& str);

                void phase_cleanup(DTO& data_flow);

                TL::Symbol get_main_function_symbol();
        };

    }
}

#endif // TL_OMP_SIMD_HPP
