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




#include "tl-compilerphase.hpp"

namespace TL
{
    void CompilerPhase::set_phase_name(const std::string& phase_name)
    {
        _phase_name = phase_name;
    }

    const std::string& CompilerPhase::get_phase_name() const
    {
        return _phase_name;
    }

    void CompilerPhase::set_phase_description(const std::string& phase_description)
    {
        _phase_description = phase_description;
    }

    const std::string& CompilerPhase::get_phase_description() const
    {
        return _phase_description;
    }

    // Returns a reference to the setter signal
    Signal1<std::string>& CompilerPhase::register_parameter(const std::string& parameter_name,
            const std::string& parameter_description, 
            std::string &parameter_reference,
            const std::string& default_value)
    {
        CompilerPhaseParameter *compiler_phase_parameter = new CompilerPhaseParameter(parameter_name,
                parameter_description, 
                parameter_reference, 
                default_value);

        _parameters.push_back(compiler_phase_parameter);

        return compiler_phase_parameter->on_change;
    }

    std::vector<CompilerPhaseParameter*> CompilerPhase::get_parameters() const
    {
        // Returns a copy
        return _parameters;
    }

    CompilerPhase::CompilerPhase()
        : _phase_name(""), 
        _phase_description(""),
        _phase_status(PHASE_STATUS_OK)
    {
    }

    CompilerPhase::~CompilerPhase()
    {
        for(std::vector<CompilerPhaseParameter*>::iterator it = _parameters.begin();
                it != _parameters.end();
                it++)
        {
            delete (*it);
        }
    }

    void CompilerPhase::set_phase_status(PhaseStatus status)
    {
        _phase_status = status;
    }

    CompilerPhase::PhaseStatus CompilerPhase::get_phase_status() const
    {
        return _phase_status;
    }
    
    void parse_boolean_option(const std::string& option_name, 
            const std::string &str_value, 
            bool &bool_value, 
            const std::string &error_message)
    {
        if (str_value == "1"
                || str_value == "yes"
                || str_value == "true")
        {
            bool_value = 1;
        }
        else if (str_value == "0"
                || str_value == "no"
                || str_value == "false")
        {
            bool_value = 0;
        }
        else
        {
            std::cerr 
                << "Invalid boolean value '" << str_value << "' for option '" << option_name << "'. " 
                << error_message << std::endl;
        }
    }
}
