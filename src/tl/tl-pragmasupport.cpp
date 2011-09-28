/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#include <typeinfo>
#include <cctype>
#include "cxx-utils.h"
#include "tl-pragmasupport.hpp"

namespace TL
{
// Initialize here the warnings to the dispatcher
    PragmaCustomCompilerPhase::PragmaCustomCompilerPhase(const std::string& pragma_handled)
        : _pragma_handled(pragma_handled)
    {
    }

    void PragmaCustomCompilerPhase::pre_run(DTO& data_flow)
    {
        // Do nothing
    }

    void PragmaCustomCompilerPhase::run(DTO& data_flow)
    {
        // Do nothing
    }

    void PragmaCustomCompilerPhase::register_directive(const std::string& str)
    {
        register_new_directive(_pragma_handled.c_str(), str.c_str(), 0, 0);
    }

    void PragmaCustomCompilerPhase::register_construct(const std::string& str, bool bound_to_statement)
    {
        if (IS_FORTRAN_LANGUAGE)
        {
            register_new_directive(_pragma_handled.c_str(), str.c_str(), 1, bound_to_statement);
        }
        else
        {
            register_new_directive(_pragma_handled.c_str(), str.c_str(), 1, 0);
        }
    }

    void PragmaCustomCompilerPhase::warning_pragma_unused_clauses(bool warning)
    {
        // _pragma_dispatcher.set_warning_clauses(warning);
    }
}
