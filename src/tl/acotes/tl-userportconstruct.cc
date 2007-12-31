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

    TL::LangConstruct UserPortConstruct::getBody() {
        PragmaCustomConstruct construct(getConstruct().get_ast(), getConstruct().get_scope_link());
        return construct.get_statement();
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
        TL::LangConstruct* body= new TL::LangConstruct(getBody());
        
        Task* task= AcotesStack::taskTop();
        UserPort* userPort= UserPort::create(construct, body, task);

        onPreInputPort(userPort);
        onPreOutputPort(userPort);
    }

    void UserPortConstruct::onPost() {
    }

    void UserPortConstruct::onPreInputPort(UserPort* userPort) {
        VariableClause stateClause(get_clause("input"), userPort->getTask());
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            Port* port= Port::createInputPort(variable);
            assert(stateClause.hasLabel(i));
            port->setName(stateClause.getLabel(i));
            userPort->addInputPort(port);
        }
    }
    
    void UserPortConstruct::onPreOutputPort(UserPort* userPort) {
        VariableClause stateClause(get_clause("output"), userPort->getTask());
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            Port* port= Port::createOutputPort(variable);
            assert(stateClause.hasLabel(i));
            port->setName(stateClause.getLabel(i));
            userPort->addOutputPort(port);
        }
    }



} /* end namespace Acotes */ } /* end namespace TL */
