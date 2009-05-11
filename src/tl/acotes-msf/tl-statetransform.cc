/*
    Acotes Translation Phase
    Copyright (C) 2007 - David Rodenas Pico <david.rodenas@bsc.es>
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
    
    $Id: tl-acotestransform.cpp 1611 2007-07-10 09:28:44Z drodenas $
*/
#include "tl-statetransform.h"

#include <assert.h>
#include <sstream>
#include "ac-state.h"
#include "ac-task.h"
#include "ac-variable.h"
#include "tl-transform.h"
#include "tl-variabletransform.h"

namespace TL { namespace Acotes {
    
    
    /* ****************************************************************
     * * Constructor
     * ****************************************************************/
    
    /**
     * No constructor.
     */
    StateTransform::StateTransform(const std::string& d) : driver(d) {
    }

    
    /* ****************************************************************
     * * Generator.
     * ****************************************************************/

    Source StateTransform::generateParamsTask_assign(State* state) {
        assert(state);
        assert(state->isCopyIn());
        assert(state->getTask());
        assert(state->getVariable());

        Source ss;
        Variable* variable= state->getVariable();
        //Task* task = state->getTask();
        ss << Transform::I(driver)->variable()->generateVariableName(variable)
           << " = prams_p->"
           << Transform::I(driver)->variable()->generateVariableName(variable) 
           << ";";
        ss << "printf (\"" 
           << Transform::I(driver)->variable()->generateVariableName(variable)
           << " = %d\\n\", "
           << Transform::I(driver)->variable()->generateVariableName(variable)
           << ");";
        return ss;
    }

    Source StateTransform::generateParams_assign(State* state) {
        assert(state);
        assert(state->isCopyIn());
        assert(state->getTask());
        assert(state->getVariable());

        Source ss;
        Variable* variable= state->getVariable();
        Task* task = state->getTask();
        ss << task->getName() << "_str."
           << Transform::I(driver)->variable()->generateVariableName(variable) 
           << " = "
           << Transform::I(driver)->variable()->generateVariableName(variable)
           << ";";
        return ss;
    }


    Source StateTransform::generateParams_struct(State* state) {
        assert(state);
        assert(state->isCopyIn());
        assert(state->getTask());
        assert(state->getVariable());

        Source ss;
        Variable* variable= state->getVariable();
        ss << Transform::I(driver)->variable()->generateVarAsParam(variable);
        return ss;
    }
    
    /**
     * Generates the copyin specification.
     */
    Source StateTransform::generateCopy(State* state) {
        assert(state);
        
        Source ss;
        
        if (state->isCopyIn()) {
            ss << generateCopyIn(state);
        }
        if (state->isCopyOut()) {
            ss << generateCopyOut(state);
        }
        
        return ss;
    }

    Source StateTransform::generateCopyStruct(State * state) {
       assert (state);
       Source ss;
       if (state->isCopyIn()) {
          //TOFINISHss << generateCopyinStruct(state);
       }
       if (state->isCopyOut()) {
          //sTOFINISHs << generateCopyoutStruct(state);
       }
       return ss;
    }

            
    /**
     * Get the source code to perform an acquire operation.
     */
    Source StateTransform::generateCopyInAcquire(State* state) {
        assert(state);
        assert(state->isCopyIn());
        assert(state->getTask());
        assert(state->getVariable());
        
        Source ss;
        
        Variable* variable= state->getVariable();
//        ss << "memcpy"
//                << "( " << Transform::I(driver)->variable()->generateReference(variable)
//                << ", copyin_acquire(" << state->getNumber() << ")"
//                << ", " << Transform::I(driver)->variable()->generateSizeof(variable)
//                << "   * " << Transform::I(driver)->variable()->generateElementCount(variable)
//                << ");";
        ss << "" ;
        return ss;
    }
            
    /**
     * Get the source code to perform an acquire operation.
     */
    Source StateTransform::generateCopyOutAcquire(State* state) {
        assert(state);
        assert(state->isCopyOut());
        assert(state->getTask());
        assert(state->getVariable());
        
        Source ss;
        
        Variable* variable= state->getVariable();
        ss << "memcpy"
                << "( copyout_acquire(" << state->getNumber() << ")"
                << ", " << Transform::I(driver)->variable()->generateReference(variable)
                << ", " << Transform::I(driver)->variable()->generateSizeof(variable)
                << "   * " << Transform::I(driver)->variable()->generateElementCount(variable)
                << ");";
        
        return ss;
    }


    Source StateTransform::generateCopyinStruct (State * state) {
       assert(state);
       assert(state->isCopyIn());
       assert(state->getTask());
       assert(state->getVariable());
       Source ss;
       Task* task= state->getTask();
       Variable* variable= state->getVariable();
       //ss << task->getName() + "_" + state->getNumber();
// TOFINISH
       assert(0);
    }
    /**
     * Generates the copyin specification.
     */
    Source StateTransform::generateCopyIn(State* state) {
        assert(state);
        assert(state->isCopyIn());
        assert(state->getTask());
        assert(state->getVariable());
        
        Source ss;
        
        Task* task= state->getTask();
        Variable* variable= state->getVariable();
//        ss << "task_copyin"
//                << "( " << task->getName()
//                << ", " << state->getNumber()
//                << ", " << Transform::I(driver)->variable()->generateReference(variable)
//                << ", " << Transform::I(driver)->variable()->generateSizeof(variable)
//                << "   * " << Transform::I(driver)->variable()->generateElementCount(variable)
//                << ");";
        ss << "";
        return ss;
    }
    
    /**
     * Generates the copyout taskgroup specification.
     */
    Source StateTransform::generateCopyOut(State* state) {
        assert(state);
        assert(state->isCopyOut());
        assert(state->getTask());
        assert(state->getVariable());
        
        Source ss;
        
        Task* task= state->getTask();
        Variable* variable= state->getVariable();
        ss << "task_copyout"
                << "( " << task->getName()
                << ", " << state->getNumber()
                << ", " << Transform::I(driver)->variable()->generateReference(variable)
                << ", " << Transform::I(driver)->variable()->generateSizeof(variable)
                << "   * " << Transform::I(driver)->variable()->generateElementCount(variable)
                << ");";
        
        return ss;
    }
            

    
} /* end namespace Acotes */ } /* end namespace TL */
