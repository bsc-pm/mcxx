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
#include "tl-variabletransform.h"

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * Generator.
     * ****************************************************************/
    
    /**
     * Generates the copyin specification.
     */
    std::string StateTransform::generateCopy(State* state) {
        assert(state);
        
        std::stringstream ss;
        
        if (state->isCopyIn()) {
            ss << generateCopyIn(state);
        }
        if (state->isCopyOut()) {
            ss << generateCopyOut(state);
        }
        
        return ss.str();
    }
            
    /**
     * Get the source code to perform an acquire operation.
     */
    std::string StateTransform::generateCopyInAcquire(State* state) {
        assert(state);
        assert(state->isCopyIn());
        assert(state->getTask());
        assert(state->getVariable());
        
        std::stringstream ss;
        
        Task* task= state->getTask();
        Variable* variable= state->getVariable();
        ss << "memcpy"
                << "( " << VariableTransform::generateReference(variable)
                << ", copyin_acquire(" << state->getNumber() << ")"
                << ", " << VariableTransform::generateSizeof(variable)
                << ");";
        
        return ss.str();
    }
            
    /**
     * Get the source code to perform an acquire operation.
     */
    std::string StateTransform::generateCopyOutAcquire(State* state) {
        assert(state);
        assert(state->isCopyOut());
        assert(state->getTask());
        assert(state->getVariable());
        
        std::stringstream ss;
        
        Task* task= state->getTask();
        Variable* variable= state->getVariable();
        ss << "memcpy"
                << "( copyout_acquire(" << state->getNumber() << ")"
                << ", " << VariableTransform::generateReference(variable)
                << ", " << VariableTransform::generateSizeof(variable)
                << ");";
        
        return ss.str();
    }

    /**
     * Generates the copyin specification.
     */
    std::string StateTransform::generateCopyIn(State* state) {
        assert(state);
        assert(state->isCopyIn());
        assert(state->getTask());
        assert(state->getVariable());
        
        std::stringstream ss;
        
        Task* task= state->getTask();
        Variable* variable= state->getVariable();
        ss << "task_copyin"
                << "( " << task->getName()
                << ", " << state->getNumber()
                << ", " << VariableTransform::generateReference(variable)
                << ", " << VariableTransform::generateSizeof(variable)
                << ");";
        
        return ss.str();
    }
    
    /**
     * Generates the copyout taskgroup specification.
     */
    std::string StateTransform::generateCopyOut(State* state) {
        assert(state);
        assert(state->isCopyOut());
        assert(state->getTask());
        assert(state->getVariable());
        
        std::stringstream ss;
        
        Task* task= state->getTask();
        Variable* variable= state->getVariable();
        ss << "task_copyout"
                << "( " << task->getName()
                << ", " << state->getNumber()
                << ", " << VariableTransform::generateReference(variable)
                << ", " << VariableTransform::generateSizeof(variable)
                << ");";
        
        return ss.str();
    }
            

    
    /* ****************************************************************
     * * No Constructor
     * ****************************************************************/
    
    /**
     * No constructor.
     */
    StateTransform::StateTransform() {
        assert(0);
    }
    
} /* end namespace Acotes */ } /* end namespace TL */
