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
#include "tl-tasktransform.h"

#include <assert.h>
#include <sstream>
#include "tl-source.hpp"
#include "ac-port.h"
#include "ac-state.h"
#include "ac-task.h"
#include "tl-acoteslogger.h"
#include "tl-initializertransform.h"
#include "tl-finalizertransform.h"
#include "tl-porttransform.h"
#include "tl-statetransform.h"
#include "tl-variabletransform.h"

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * Transform.
     * ****************************************************************/
    
    /** 
     * Transform the task and its children.
     */
    void TaskTransform::transform(Task* task) {
        assert(task);
        
        // First transform all the children, recursive call
        transformChildren(task);
        
        if (!task->isImplicitTask()) {
            transformAddOutline(task);
            transformReplaceConstruct(task);
        }
    }
    
    /**
     * Transform all the task children. 
     * <p>
     * Must be called only by taskgrouptransform or tasktransform.
     */
    void TaskTransform::transformChildren(Task* task) {
        const std::vector<Task*> &children= task->getChildVector();
        for (unsigned i= 0; i < children.size(); i++) {
            Task* child= children.at(i);
            transform(child);
        }
    }
    
    /**
     * Adds the outline.
     */
    void TaskTransform::transformAddOutline(Task* task) {
        assert(task);
        
        TL::LangConstruct* taskConstruct= task->getConstruct();
        AST_t taskAST= taskConstruct->get_ast();
        ScopeLink taskScopeLink= taskConstruct->get_scope_link();
    
        // Add outline task
        Source outlineSource= generateOutline(task);
        AST_t outlineTree= outlineSource.parse_global(taskAST, taskScopeLink);
        taskAST.prepend_sibling_function(outlineTree);
    }

    /**
     * Adds the outline.
     */
    void TaskTransform::transformReplaceConstruct(Task* task) {
        assert(task);
        
        TL::LangConstruct* taskConstruct= task->getConstruct();
        AST_t taskAST= taskConstruct->get_ast();
        ScopeLink taskScopeLink= taskConstruct->get_scope_link();
    
        // Replace taskgroup construct
        Source replaceSource= generateReplacement(task);
        AST_t replaceTree= replaceSource.parse_statement(taskAST, taskScopeLink);
        taskAST.replace(replaceTree);
    }

    
    
    /* ****************************************************************
     * * Outline generation
     * ****************************************************************/
    
    std::string TaskTransform::generateOutline(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        ss << "void " << task->getName() << "_outline(task_t __task)"
                << "{"
                <<   generateVariable(task)
                <<   generateInitializer(task)
                <<   generateCopyInAcquire(task)
                <<   generateControlAcquire(task)
                <<   "while (task_allopen())"
                <<   "{"
                <<      generateControlInputPeek(task)
                <<      generateBody(task)
                <<      generateControlOutputPeek(task)
                <<      generateControlPush(task)
                <<      generateControlPop(task)
                <<      generateControlAcquire(task)
                <<   "}"
                <<   generateCopyOutAcquire(task)
                <<   generateFinalizer(task)
                <<   "task_close();"
                << "}"
                ;
        
        return ss.str();
    }
    
    /**
     * Generates the variables declaration of the task.
     */
    std::string TaskTransform::generateVariable(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Variable*> &variables= task->getVariableVector();
        for (unsigned i= 0; i < variables.size(); i++) {
            Variable* variable= variables.at(i);
            ss << VariableTransform::generateVariable(variable);
        }
        
        return ss.str();
    }
    
    std::string TaskTransform::generateInitializer(Task* task) {
        return InitializerTransform::generate(task);
    }
    
    std::string TaskTransform::generateFinalizer(Task* task) {
        return FinalizerTransform::generate(task);
    }
    
    /**
     * Generates the copyin acquires of the variables.
     */
    std::string TaskTransform::generateCopyInAcquire(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<State*> &states= task->getStateVector();
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            if (state->isCopyIn()) {
                ss << StateTransform::generateCopyInAcquire(state);
            }
        }
        
        return ss.str();
    }
    
    /**
     * Generates the copyout acquires of the variables.
     */
    std::string TaskTransform::generateCopyOutAcquire(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<State*> &states= task->getStateVector();
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            if (state->isCopyOut()) {
                ss << StateTransform::generateCopyOutAcquire(state);
            }
        }
        
        return ss.str();
    }
    
    /**
     * Generates the taskbody.
     */
    std::string TaskTransform::generateBody(Task* task) 
    {
       assert(task);
        
       std::stringstream ss;
       
       ss << task->getBody()->prettyprint();
       
       return ss.str();
    }

    std::string TaskTransform::generateControlAcquire(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl()) {
                ss << PortTransform::generateAcquire(port);
            }
        }
        
        return ss.str();
    }

    std::string TaskTransform::generateControlInputPeek(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isInput()) {
                ss << PortTransform::generateInputPeek(port);
            }
        }
        
        return ss.str();
    }

    std::string TaskTransform::generateControlOutputPeek(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isOutput()) {
                ss << PortTransform::generateOutputPeek(port);
            }
        }
        
        return ss.str();
    }

    std::string TaskTransform::generateControlPop(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isInput()) {
                ss << PortTransform::generatePop(port);
            }
        }
        
        return ss.str();
    }

    std::string TaskTransform::generateControlPush(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isOutput()) {
                ss << PortTransform::generatePush(port);
            }
        }
        
        return ss.str();
    }
    
            
    /* ****************************************************************
     * * Replacement generation
     * ****************************************************************/
    
    std::string TaskTransform::generateReplacement(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        ss      << "{"
                << generateArtificialPush(task)
                << generateArtificialPop(task)
                << "}"
                ;
        
        return ss.str();
    }
    
    std::string TaskTransform::generateArtificialPush(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            Port* artificialCounterpart= port->getArtificialCounterpart();
            if (artificialCounterpart && artificialCounterpart->isOutput()) {
                ss << PortTransform::generateAcquire(artificialCounterpart);
                ss << PortTransform::generateOutputPeek(artificialCounterpart);
                ss << PortTransform::generatePush(artificialCounterpart);
            }
        }
        
        return ss.str();
    }

    std::string TaskTransform::generateArtificialPop(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            Port* artificialCounterpart= port->getArtificialCounterpart();
            if (artificialCounterpart && artificialCounterpart->isInput()) {
                ss << PortTransform::generateAcquire(artificialCounterpart);
                ss << PortTransform::generateInputPeek(artificialCounterpart);
                ss << PortTransform::generatePop(artificialCounterpart);
            }
        }
        
        return ss.str();
    }

    
    
    /* ****************************************************************
     * * Taskgroup replacement generation support
     * ****************************************************************/
    
    /**
     * Generates the start code for the taskgroup.
     */
    std::string TaskTransform::generateInit(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        ss      << "task_t " << task->getName() << ";"
                << "task_init("
                <<     "&" << task->getName();
        if (task->isImplicitTask()) {
            ss  <<   ", (void*)0";
        } else {
            ss  <<   ", " << task->getName() << "_outline";
        }
        ss      <<   ", " << task->getTeam()
                <<   ");"
                ;
        
        return ss.str();
    }
    
    /**
     * Generates the start code for the taskgroup.
     */
    std::string TaskTransform::generatePorts(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            ss << PortTransform::generatePort(port);
        }
        
        return ss.str();
    }
    
    /**
     * Generates the start code for the taskgroup.
     */
    std::string TaskTransform::generateStart(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        ss << "task_start(" << task->getName() << ");";
        
        return ss.str();
    }
    
    /**
     * Generates the start code for the taskgroup.
     */
    std::string TaskTransform::generateWait(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        ss << "task_wait(" << task->getName() << ");";
        
        return ss.str();
    }
    
    
            
    /* ****************************************************************
     * * No Constructor
     * ****************************************************************/
     
    TaskTransform::TaskTransform() {
        assert(0);
    }
     
    
} /* end namespace Acotes */ } /* end namespace TL */
