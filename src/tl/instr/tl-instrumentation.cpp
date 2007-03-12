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

                if (ExternalVars::get("instrumentation", "0") == "1")
                {
                    instrument_phase->run(dto);
                }
                else
                {
                    std::cerr << "Instrumentation disabled. Enable it with '--variable=instrumentation:1'" << std::endl;
                }
            }
    };
}

EXPORT_PHASE(TL::Instrumentation);
