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
#include "cxx-utils.h"
#include "tl-instrumentation.hpp"
#include "tl-instrumentcalls.hpp"
#include "tl-compilerphase.hpp"
#include "tl-predicateutils.hpp"
#include "tl-langconstruct.hpp"
#include "tl-traverse.hpp"
#include "tl-scopelink.hpp"
#include "tl-externalvars.hpp"

#include <iostream>
#include <fstream>
#include <set>

namespace TL
{
    class Instrumentation : public CompilerPhase
    {
        private:
        public:
            void virtual run(DTO& dto)
            {
                CompilerPhase* instrument_phase = NULL;

                std::string instrument_mode = ExternalVars::get("instrument_mode", "calls");
                if (instrument_mode == "calls")
                {
                    instrument_phase = new InstrumentCalls();
                }
                else if (instrument_mode == "functions")
                {
                    std::cerr << "Instrumentation of functions disabled. Only calls can be instrumented at the moment" << std::endl;
                }
                else
                {
                    std::cerr << "Invalid instrument_mode. It can be 'calls' or 'functions'" << std::endl;
                }

                if (instrument_phase == NULL)
                {
                    std::cerr << "Skipping instrumentation. No valid mode defined" << std::endl;
                    return;
                }

                if (ExternalVars::get("instrument", "0") == "1")
                {
                    instrument_phase->run(dto);
                }
                else
                {
                    std::cerr << "Instrumentation disabled. Enable it with '--variable=instrument:1'" << std::endl;
                }
            }
    };
}

EXPORT_PHASE(TL::Instrumentation);
