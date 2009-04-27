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
#include <tl-userportconstruct.h>

#include <assert.h>
#include "ac-port.h"
#include "ac-userport.h"
#include "ac-variable.h"
#include "tl-acoteslogger.h"
#include "tl-acotesstack.h"
#include "tl-variableclause.h"

namespace TL { namespace Acotes {
    
    class Task;
    
    /* ****************************************************************
     * * LangConstruct support
     * ****************************************************************/
    
    UserPortConstruct::UserPortConstruct(TL::LangConstruct langConstruct)
    : TL::PragmaCustomConstruct(langConstruct.get_ast(), langConstruct.get_scope_link())
    {
    }

    TL::LangConstruct UserPortConstruct::getConstruct() {
        return *this;
    }

    
    /* ****************************************************************
     * * CompilerPhase events
     * ****************************************************************/
    
    void UserPortConstruct::onPre() {
        // retrieve information
        TL::LangConstruct* construct= new TL::LangConstruct(getConstruct());
        
        Task* task= AcotesStack::taskTop();
        UserPort* userPort= UserPort::create(construct, task);

        onPreInputPort(userPort);
        onPreInputReplicatePort(userPort);
        onPreOutputPort(userPort);
    }

    void UserPortConstruct::onPost() {
    }

    void UserPortConstruct::onPreInputPort(UserPort* userPort) {
        VariableClause stateClause(get_clause("input"), userPort->getTask());
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            Port* port= Port::createInputPort(variable);
            if (stateClause.hasLabel(i)) {
                port->setName(stateClause.getLabel(i));
                userPort->addInputPort(port);
            } else {
                AcotesLogger::error(this)
                        << "input variable '" << variable->getName() << "'"
                        << "has no label"
                        << std::endl;
            }
        }
    }
    
    void UserPortConstruct::onPreInputReplicatePort(UserPort* userPort) {
        VariableClause stateClause(get_clause("inputreplicate"), userPort->getTask());
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            Port* port= Port::createInputPort(variable);
            if (stateClause.hasLabel(i)) {
                port->setName(stateClause.getLabel(i));
                port->setReplicate(true);
                userPort->addInputPort(port);
            } else {
                AcotesLogger::error(this)
                        << "input variable '" << variable->getName() << "'"
                        << "has no label"
                        << std::endl;
            }
        }
    }
    
    void UserPortConstruct::onPreOutputPort(UserPort* userPort) {
        VariableClause stateClause(get_clause("output"), userPort->getTask());
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            Port* port= Port::createOutputPort(variable);
            if (stateClause.hasLabel(i)) {
                port->setName(stateClause.getLabel(i));
                userPort->addOutputPort(port);
            } else {
                AcotesLogger::error(this)
                        << "output variable '" << variable->getName() << "'"
                        << "has no label"
                        << std::endl;
            }
        }
    }



} /* end namespace Acotes */ } /* end namespace TL */
