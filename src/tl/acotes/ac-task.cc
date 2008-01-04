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
#include "ac-task.h"

#include <assert.h>
#include "ac-port.h"
#include "ac-portconnection.h"
#include "ac-state.h"
#include "ac-taskgroup.h"
#include "ac-variable.h"

namespace TL { namespace Acotes {


    /* ****************************************************************
     * * Tribal behaviour
     * ****************************************************************/

    std::vector<Task*> Task::instanceVector;
     
    Task* Task::create(Taskgroup* taskgroup, Task* parent, TL::LangConstruct* construct, TL::LangConstruct* body) {
        
        std::stringstream ssname;
        ssname << "task" << instanceVector.size();

        // register the instance
        Task* task= new Task(ssname.str());
        instanceVector.push_back(task);
        
        // initializate fields
        task->setBody(body);
        task->setConstruct(construct);
        
        // create parent-child relationship
        task->setParent(parent);

        // create taskgroup task relationship
        task->setTaskgroup(taskgroup);
        

        return task;
    }

    Task::Task(const std::string& nam) 
    : name(nam)
    , body(NULL), construct(NULL)
    , taskgroup(NULL)
    , parent(NULL) 
    {    
    }
    
    
    
    /* ****************************************************************
     * * Name
     * ****************************************************************/
    
    
    
    /* ****************************************************************
     * * LangConstruct support
     * ****************************************************************/
    
    /**
     * Sets the body lang construct for this task.
     * <p> 
     * The body does not includes the pragma statement. Body
     * must be a valed statement.
     */
    void Task::setBody(TL::LangConstruct* body) {
        assert(body);
        assert(!this->body /* only call once */);
        
        this->body= body;
    }

    /**
     * Sets the pragma lang construct for this task.
     * <p> 
     * The construct must include the pragma statement.
     */
    void Task::setConstruct(TL::LangConstruct* construct) {
        assert(construct);
        assert(!this->construct /* only call once */);
        
        this->construct= construct;
    }
    
    
    
    /* ****************************************************************
     * * Taskgroup relationship
     * ****************************************************************/
    
    /**
     * Links the task with the taskgroup.
     * <p>
     * It also mantains the tasks relationship between taskgroup and
     * task calling to Taskgroup::addTask(Task*) method.
     */
    void Task::setTaskgroup(Taskgroup* taskgroup) {
        assert(taskgroup);
        assert(!this->taskgroup /* only call once */);
        
        this->taskgroup= taskgroup;
        taskgroup->addTask(this);
    }
    
    
    
    /* ****************************************************************
     * * Parent/Child relationship
     * ****************************************************************/
    
    /**
     * Ask if this task is an implicit task of a taskgroup.
     */
    bool Task::isImplicitTask() const
    {
        bool result= getTaskgroup()->getImplicitTask() == this;
        return result;
    }
    
    /**
     * Links this task as child of parent.
     * <p>
     * 
     */
    void Task::setParent(Task* parent) {
        assert(!this->parent /* only call once */);
        
        this->parent= parent;
        if (parent) {
            parent->childVector.push_back(this);
        }
    }
    

    
    /* ****************************************************************
     * * Port relationship
     * ****************************************************************/
    
    /**
     * Adds a new port to the Task.
     * <p>
     * Return the number of the port inside of the task.
     * This method must be called by port and noone else.
     */
    int Task::addPort(Port* port) {
        assert(port);
        
        int portNumber= portVector.size();
        portVector.push_back(port);
        
        return portNumber;
    }
   
    /**
     * Ask if this task has any input control port.
     */
    bool Task::hasInputControlPort() const
    {
        bool result= false;
        
        for (unsigned i= 0; i < portVector.size() && !result; i++) {
            Port* port= portVector.at(i);
            result= port->isInput() && port->isControl();
        }
        
        return result;
    }
       
    /**
     * Ask if this task has any input control port.
     */
    Port* Task::getInputControlPort(TL::Symbol symbol) const
    {
        Port* result= NULL;
        
        for (unsigned i= 0; i < portVector.size() && !result; i++) {
            Port* port= portVector.at(i);
            if (port->isInput() && port->hasVariable() && port->getVariable()->hasSymbol(symbol)) {
                result= port;
            }
        }
        
        return result;
    }
       
    /**
     * Ask if this task has any input control port.
     */
    Port* Task::getOutputControlPort(TL::Symbol symbol) const
    {
        Port* result= NULL;
        
        for (unsigned i= 0; i < portVector.size() && !result; i++) {
            Port* port= portVector.at(i);
            if (port->isOutput() && port->getVariable()->hasSymbol(symbol)) {
                result= port;
            }
        }
        
        return result;
    }


    
    /* ****************************************************************
     * * State relationship
     * ****************************************************************/
    
    /**
     * Adds a new state to the Task.
     * <p>
     * Return the number of the state inside of the task.
     * This method must be called by state and noone else.
     */
    int Task::addState(State* state) {
        assert(state);
        
        int stateNumber= stateVector.size();
        stateVector.push_back(state);
        
        return stateNumber;
    }

    /**
     * 
     */
    State* Task::getState(TL::Symbol symbol) const
    {
       State* result= NULL;
       
       for (unsigned i= 0; i < stateVector.size() && !result; i++) {
           State* state= stateVector.at(i);
           if (state->getVariable()->hasSymbol(symbol)) {
               result= state;
           }
       }
       
       return result;
    }
    
    
    
    /* ****************************************************************
     * * Port connection creation
     * ****************************************************************/
    
    /**
     * Creates the local required port connections for this task.
     */
    void Task::createPortConnections() {
        createChildPortConnections();
        
        if (!hasInputControlPort() && hasParent()) {
            createVirtualPortandConnection();
        } else {
            createBypassConnection();
            createArtificalPortandConnection();
            // compute_graph_outputs();
        }
    }

    /**
     * Creates its childs port connections.
     */
    void Task::createChildPortConnections() {
        for (unsigned i= 0; i < childVector.size(); i++) {
            Task* child= childVector.at(i);
            child->createPortConnections();
        }
    }

    void Task::createVirtualPortandConnection() {
        assert(!hasInputControlPort());
        assert(hasParent());
        
        Task* parent= getParent();
        
        Port* virtualInput= Port::createVirtualInputPort(this);
        Port* virtualOutput= Port::createVirtualOutputPort(parent);
        virtualInput->setControl(true);
        virtualOutput->setArtificial(true);
        PortConnection::create(virtualOutput, virtualInput);
    }
  
    void Task::createArtificalPortandConnection() 
    {
        const std::vector<Port*> ports= getPortVector();
        
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && !port->hasPortConnection() && !port->isNamed()) {
                createArtificalPortandConnection(port);
            }
        }
    }
    
    void Task::createArtificalPortandConnection(Port* port)
    {
        assert(port);
        assert(port->getVariable());
        assert(port->isControl());
        assert(!port->hasPortConnection());
        
        Variable* parentVariable= getParentVariable(port->getVariable());
        if (!parentVariable) {
            if (hasParent() && getParent()->isImplicitTask()) {
                parentVariable= Variable::create(getParent(), port->getVariable()->getSymbol());
            } else {
                assert(0 /* TODO: user error */); 
            }
        }
        if (!isBypass(port->getVariable()) && !getParent()->isBypass(parentVariable)) {
            if (port->isInput()) {
                Port* counterpart= Port::createArtificialOutputPort(parentVariable);
                PortConnection::create(counterpart, port);
            } else if (port->isOutput()) {
                Port* counterpart= Port::createArtificialInputPort(parentVariable);
                PortConnection::create(port, counterpart);
            } else {
                assert(0);
            }
        }
    }
    
    void Task::createBypassConnection()
    {
        const std::vector<TL::Symbol> &bypasses= getBypassVector();
        
        for (unsigned i= 0; i < bypasses.size(); i++) {
            TL::Symbol bypass= bypasses.at(i);
            createBypassConnection(bypass);
        }
    }
    
    void Task::createBypassConnection(TL::Symbol symbol)
    {
        Task* output= NULL;
        
        output= createBypassConnection(symbol, output);
    }
     
    Task* Task::createBypassConnection(TL::Symbol symbol, Task* output)
    {
        // if here is also shortcutted
        if (isBypass(symbol))
        {
            // for each initial input before output 
            const std::vector<Task*> &children= getChildVector();
            for (unsigned i= 0; i < children.size(); i++) {
                Task* child= children.at(i);
                
                // if symbol is input calls to it as symbol/output
                // if symbol is output overwrites it, 
                // if both, do both
                output= child->createBypassConnection(symbol, output);
            }
	
	// if not shortcutted output
	} else /* if (!isBypass(symbol)) */ {
            // if it is input shortcut
            if (hasInputControlPort(symbol) && !getInputControlPort(symbol)->isNamed() && output) {                  
                    // connect!!
                    Port* inport= getInputControlPort(symbol);
                    Port* outport= output->getOutputControlPort(symbol);
                    
                    PortConnection::create(outport, inport);
            } 
            // if it is output is the next output
            if (hasOutputControlPort(symbol) && !getOutputControlPort(symbol)->isNamed()) {
                    output= this;
            }
	}
	
	return output;
    }

    
    
    /* ****************************************************************
     * * Variable relationship
     * ****************************************************************/
    
    /** 
     * Get the variable with the requested symbol.
     */
    Variable* Task::getVariable(TL::Symbol symbol) {
        
        Variable* result= NULL;
        
        for (unsigned i= 0; i < variableVector.size() && !result; i++) {
            Variable* variable= variableVector.at(i);
            if (variable && variable->hasSymbol(symbol)) {
                result= variable;
            } 
        }
        
        return result;
    }
    
    /** 
     * Method called by Variable.
     */
    void Task::addVariable(Variable* variable)
    {
        assert(variable);
        assert(variable->getTask() == this);
        
        variableVector.push_back(variable);
    }
   
    Variable* Task::getParentVariable(Variable* variable) const 
    {
        assert(variable);
        assert(variable->hasSymbol());
        assert(hasParent());
        
        Variable* result= parent->getVariable(variable->getSymbol());
        return result;
    }
    
    
    
    /* ****************************************************************
     * * Bypass symbols
     * ****************************************************************/
    
    bool Task::isBypass(TL::Symbol symbol) const 
    {
        bool result= false;
        
        for (unsigned i= 0; i < bypassVector.size() && !result; i++) {
            TL::Symbol other= bypassVector.at(i);
            result= other == symbol;
        }
        
        return result;
    }
    
    bool Task::isBypass(Variable* variable) const 
    {
        assert(variable);
        assert(variable->hasSymbol());
        
        TL::Symbol other= variable->getSymbol();
        bool result= false;
        
        for (unsigned i= 0; i < bypassVector.size() && !result; i++) {
            TL::Symbol symbol= bypassVector.at(i);
            result= other == symbol;
        }
        
        return result;
    }
    
} /* end namespace Acotes */ } /* end namespace TL */

