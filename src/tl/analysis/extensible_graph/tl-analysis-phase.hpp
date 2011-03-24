
#ifndef TL_ANALYSIS_COMPILER_PHASE_HPP
#define TL_ANALYSIS_COMPILER_PHASE_HPP

#include "tl-objectlist.hpp"
#include "extensible_graph.hpp"
#include "tl-compilerphase.hpp"
#include "tl-fortran.hpp"

namespace TL
{
    //! Phase that allows several analysis of code
    class LIBTL_CLASS AnalysisPhase : public CompilerPhase
    {
        
        private:
            ObjectList<FunctionDefinition> _func_defs;
            ObjectList<Fortran::ProgramUnit> _prog_units;
            // List of Main Program Units
            ScopeLink _sl;
        
        public:
            //! Constructor of this phase
            AnalysisPhase();
            
            //! Entry point of the phase
            /*!
            * This function gets the different FunctionDefinitions / ProgramUnits of the DTO,
            * depending on the language of the code
            */
            virtual void run(TL::DTO& dto);
    };
}

#endif  // TL_ANALYSIS_COMPILER_PHASE_HPP