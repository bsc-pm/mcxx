/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
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

    CompilerPhase::~CompilerPhase()
    {
        for(std::vector<CompilerPhaseParameter*>::iterator it = _parameters.begin();
                it != _parameters.end();
                it++)
        {
            delete (*it);
        }
    }
}
