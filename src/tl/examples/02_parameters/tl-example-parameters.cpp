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




#include "tl-example-parameters.hpp"
#include <iostream>

namespace TL
{
    ExampleParameters::ExampleParameters()
    {
        // Parameters are registered in the constructor of the phase
        set_phase_name("Example phase with parameters");
        set_phase_description("This phase does nothing but registering some parameters and printing them");

        register_parameter("param1",
                "This is the parameter number one",
                _parameter_1_value,
                "val1");

        register_parameter("param2",
                "This is the parameter number two",
                _parameter_2_value,
                "val2").connect(std::bind(&ExampleParameters::check_param2, this, std::placeholders::_1));
    }

    void ExampleParameters::check_param2(const std::string& str)
    {
        if (str != "val2" && str != "val3")
        {
            std::cerr << "Invalid value for 'param2', it will be assumed to be 'val2'" << std::endl;
            _parameter_2_value = "val2";
        }
        else
        {
            _parameter_2_value = "val3";
        }
    }

    ExampleParameters::~ExampleParameters()
    {
    }

    void ExampleParameters::pre_run(TL::DTO& dto)
    {
    }

    void ExampleParameters::run(TL::DTO& dto)
    {
        std::cerr << "Parameter 1 -> " << _parameter_1_value << std::endl;
        std::cerr << "Parameter 2 -> " << _parameter_2_value << std::endl;
    }
}

EXPORT_PHASE(TL::ExampleParameters);
