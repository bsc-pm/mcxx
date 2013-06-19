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




#include "tl-example-basic.hpp"
#include <iostream>

namespace TL
{
    BasicTestPhase::BasicTestPhase()
    {
        set_phase_name("Basic Test");
        set_phase_description("This is a basic phase test which does nothing but printing some messages");

        std::cerr << "Basic test phase created" << std::endl;
    }

    BasicTestPhase::~BasicTestPhase()
    {
        std::cerr << "Basic test phase destroyed" << std::endl;
    }

    void BasicTestPhase::pre_run(TL::DTO& dto)
    {
        std::cerr << "Basic test phase pre_run" << std::endl;
    }

    void BasicTestPhase::run(TL::DTO& dto)
    {
        std::cerr << "Basic test phase run" << std::endl;
    }
}

EXPORT_PHASE(TL::BasicTestPhase);
