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

#include "ac-taskgroup.h"

#include <assert.h>
#include <sstream>

#include "ac-port.h"
#include "ac-portconnection.h"
#include "ac-state.h"
#include "ac-sharedconnection.h"
#include "ac-task.h"
#include "ac-variable.h"
#include "tl-acoteslogger.h"

namespace TL { namespace Acotes {

    
    
    /* ****************************************************************
     * * Tribal behaviour
     * ****************************************************************/

    std::vector<Taskgroup*> Taskgroup::instanceVector;
     
    /**
     * Creates a complete instance of the taskgrpoup and register it.
     */
    Taskgroup* Taskgroup::create(TL::LangConstruct* construct, TL::LangConstruct* body, DTO& dto) {
        
        // generate uniq taksgroup name
        std::stringstream ssname;
        int ssnum = instanceVector.size();
        ssname << "taskgroup" << ssnum;
        
        // create taskgroup instance and register
        Taskgroup* taskgroup= new Taskgroup(ssname.str(), ssnum);
        instanceVector.push_back(taskgroup);
        
        // create implicitTask relationship
        taskgroup->createImplicitTask(construct, body, dto);
        
        return taskgroup;
    }

    /**
     * Default constructor.
     */
    Taskgroup::Taskgroup(const std::string& nam, int nm) 
    : name(nam), num(nm)
    , implicitTask(NULL) {
    
    }

    
    
    /* ****************************************************************
     * * Task relationship.
     * ****************************************************************/

    /**
     * Adds a task to this taskgroup.
     * <p>
     * This method is called by Task because it is a derivated relationship.
     */
    void Taskgroup::addTask(Task* task) {
        taskVector.push_back(task);
    }
    
    /**
     * Create the taskgroup implicit task.
     */
    void Taskgroup::createImplicitTask(TL::LangConstruct* construct, TL::LangConstruct* body, DTO& dto)
    {
        assert(body);
        assert(construct);
        assert(!implicitTask);
        
        Task* implicitTask= Task::create(this, NULL, construct, body, dto);
        this->implicitTask= implicitTask;
    }
    

    
    /* ****************************************************************
     * * LangConstruct support
     * ****************************************************************/

    TL::LangConstruct* Taskgroup::getBody() const
    {
        return getImplicitTask()->getBody();
    }
    
    TL::LangConstruct* Taskgroup::getConstruct() const 
    {
        return getImplicitTask()->getConstruct();
    }
 
    
    
    /* ****************************************************************
     * * Port Connections support
     * ****************************************************************/
    
    void Taskgroup::createPortConnections() {
        assert(getImplicitTask());
        

        createNamedPortConnections();

        Task* implicitTask= getImplicitTask();
        implicitTask->createPortConnections();

        verifyPortConnections();
    }
    
    /**
     * Adds a port connection to the taskgroup.
     * <p>
     * Method called by PortConnection.
     */
    void Taskgroup::addPortConnection(PortConnection* portConnection) {
        assert(portConnection);
        
        portConnectionVector.push_back(portConnection);
    }

    /**
     * Verifies that all ports has a connection.
     */
    void Taskgroup::verifyPortConnections() {
        assert(getImplicitTask());
        
        Task* implicitTask= getImplicitTask();
        implicitTask->verifyPortConnections();
    }
    
    
    /* ****************************************************************
     * * CopyInOut state relationship
     * ****************************************************************/
   
    /**
     * Adds a state as copyin or copyout state.
     * <p>
     * Method called by state.
     */
    void Taskgroup::addCopyState(State* state) 
    {
        assert(state);
        assert(state->hasTask());
        assert(state->getTask()->getTaskgroup() == this);
        assert(state->isCopyIn() || state->isCopyOut());
        
        if (!state->isCopyOut() 
        || checkCopyOutSymbol(state->getVariable()->getSymbol(), state->getTask())) {
            copyStateVector.push_back(state);        
        }
    }
    
    bool Taskgroup::checkCopyOutSymbol(TL::Symbol symbol, Task* task) const
    {
        bool result= true;
        
        for (unsigned i= 0; i < copyStateVector.size() && result; i++) {
            State* state= copyStateVector.at(i);
            if (state->isCopyOut() && state->getVariable()->getSymbol() == symbol
                    && state->getTask() != task) {
                AcotesLogger::error(NULL) 
                        << "symbol " << symbol.get_point_of_declaration()
                        .get_locus() << " defined twice as copyout." << std::endl;
                result= false;
            }
        }
        
        return result;
    }
    
    

    /* ****************************************************************
     * * Shared state relationship
     * ****************************************************************/
   
    /**
     * Adds a state as copyin or copyout state.
     * <p>
     * Method called by state.
     */
    void Taskgroup::addSharedState(State* state) 
    {
        assert(state);
        assert(state->isUpdateShared() || state->isAsyncShared() || state->isSyncShared());
        assert(state->hasTask());
        assert(state->getTask()->getTaskgroup() == this);
        
        sharedStateVector.push_back(state);        
    }
    
    

    /* ****************************************************************
     * * Shared Connections support
     * ****************************************************************/
    
    void Taskgroup::createSharedConnections() {
        assert(getImplicitTask());

        const std::vector<State*> &states= getSharedStateVector();
        
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            if (state->isUpdateShared()) {
                createSharedConnections(state);
            }
        }
        
    }
    
    /**
     * Adds a port connection to the taskgroup.
     * <p>
     * Method called by PortConnection.
     */
    void Taskgroup::addSharedConnection(SharedConnection* sharedConnection) {
        assert(sharedConnection);
        
        sharedConnectionVector.push_back(sharedConnection);
    }
    
    void Taskgroup::createSharedConnections(State* source)
    {
        const std::vector<State*> &states= getSharedStateVector();
        
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            if (state->isShared() 
                    && state->getTask() != source->getTask()
                    && state->getVariable()->getSymbol() == source->getVariable()->getSymbol()) {
                SharedConnection::create(source, state);
            }
        }
    }
    
 

    /* ****************************************************************
     * * NamedPorts support
     * ****************************************************************/
    
    void Taskgroup::addNamedPort(Port* port)
    {
        assert(port);
        assert(port->isNamed());
        assert(port->hasTask());
        assert(port->getTask()->getTaskgroup() == this);
        
        namedPortVector.push_back(port);
    }
    
    void Taskgroup::createNamedPortConnections()
    {
        for (unsigned i= 0; i < namedPortVector.size(); i++) {
            Port* port= namedPortVector.at(i);
            if (port->isOutput()) {
                createNamedPortConnections(port);
            }
        }
    }
        
    void Taskgroup::createNamedPortConnections(Port* output)
    {
        for (unsigned i= 0; i < namedPortVector.size(); i++) {
            Port* port= namedPortVector.at(i);
            if (port->getName() == output->getName()) {
                if (port->isOutput() && port != output) {
                    assert(0); /* TODO: two outputs, user error... */
                } else
                if (port->isInput() && !port->hasPortConnection()) {
                    PortConnection::create(output, port);
                } else if (port != output) {
                    assert(0); /* internal error. */
                }
            }
        }
    }
        
     
} /* end namespace Acotes */ } /* end namespace TL */

