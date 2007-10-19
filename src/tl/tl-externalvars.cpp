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
#include <string>
#include "cxx-driver.h"
#define FROM_TL_EXTERNALVARS_CPP
#include "tl-externalvars.hpp"
#undef FROM_TL_EXTERNALVARS_CPP

namespace TL
{
    std::string ExternalVars::get(const std::string& name, const std::string& default_val)
    {
        for (int i = 0; i < CURRENT_CONFIGURATION(num_external_vars); i++)
        {
            std::string variable = CURRENT_CONFIGURATION(external_vars)[i]->name;
            
            if (variable == name)
            {
                return CURRENT_CONFIGURATION(external_vars)[i]->value;
            }
        }

        return default_val;
    }
}
