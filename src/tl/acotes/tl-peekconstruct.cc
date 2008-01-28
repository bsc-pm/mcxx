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
#include <tl-peekconstruct.h>

#include "tl-acoteslogger.h"
#include "tl-acotesstack.h"
#include "ac-peek.h"
#include "ac-port.h"
#include "ac-state.h"
#include "ac-task.h"
#include "ac-variable.h"

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * LangConstruct support
     * ****************************************************************/
    
    PeekConstruct::PeekConstruct(TL::LangConstruct langConstruct)
    : TL::PragmaCustomConstruct(langConstruct.get_ast(), langConstruct.get_scope_link())
    {
    }

    TL::LangConstruct PeekConstruct::getBody() {
        PragmaCustomConstruct construct(getConstruct().get_ast(), getConstruct().get_scope_link());
        return construct.get_statement();
    }
    
    TL::LangConstruct PeekConstruct::getConstruct() {
        return *this;
    }

    
    /* ****************************************************************
     * * CompilerPhase events
     * ****************************************************************/
    
    void PeekConstruct::onPre() {
        // retrieve information
        TL::LangConstruct* construct= new TL::LangConstruct(getConstruct());
        TL::LangConstruct* body= new TL::LangConstruct(getBody());
        Task* task= AcotesStack::taskTop();
        
        ObjectList<IdExpression> ideInput= get_clause("input").id_expressions();
        ObjectList<IdExpression> ideHistory= get_clause("history").id_expressions();
        ObjectList<IdExpression> ideIndex= get_clause("index").id_expressions();
        
        Port* input= NULL;
        if (ideInput.size() < 1) {
            AcotesLogger::error(this) 
                    << "no input port for the peek specified" << std::endl;
        } else if (ideInput.size() > 1) {
            AcotesLogger::error(this) 
                    << "too many input ports for the peek" << std::endl;
        } else {
            IdExpression ide= ideInput.at(0);
            Port* port= task->getInputControlPort(ide.get_symbol());
            if (!port) {
                AcotesLogger::error(this) 
                        << ide.prettyprint() << ": "
                        << "is not an input control port" << std::endl;
            } else if (port->hasPeek()) {
                AcotesLogger::error(this) 
                        << ide.prettyprint() << ": "
                        << "has already a peek" << std::endl;
            } else {
                if (task->isTeam() && !port->isReplicate()) {
                AcotesLogger::error(this) 
                        << ide.prettyprint() << ": "
                        << "cannot peek on non replicate inputs at teams" << std::endl;
                } else {
                    input= port;
                }
            }
        }
        
        State* history= NULL;
        if (ideHistory.size() < 1) {
            AcotesLogger::error(this) 
                    << "no history copy in variable for the peek specified" << std::endl;
        } else {
            if (ideHistory.size() > 1) {
                AcotesLogger::error(this) 
                    << "too many history variables" << std::endl;
            }    
            IdExpression ide= ideHistory.at(0);
            State* state= task->getState(ide.get_symbol());
            if (!state || !state->isCopyIn()) {
                AcotesLogger::error(this) 
                        << ide.prettyprint() << ": "
                        << "is not a copy in state" << std::endl;
            } else {
                if (!state->getVariable()->isArray()) {
                    AcotesLogger::error(this) 
                        << ide.prettyprint() << ": "
                        << "history must be an array" << std::endl;
                } else {
                    history= state;
                }
            }
        }
        
        State* index= NULL;
        if (ideIndex.size() > 1) {
            AcotesLogger::error(this) 
                    << "too many index variables" << std::endl;
        } if (ideIndex.size() == 1) {
            IdExpression ide= ideIndex.at(0);
            State* state= task->getState(ide.get_symbol());
            if (!state || !state->isCopyIn()) {
                AcotesLogger::error(this) 
                        << ide.prettyprint() << ": "
                        << "is not a copy in state" << std::endl;
            } else {
                if (state->getVariable()->isArray()) {
                    AcotesLogger::error(this) 
                            << ide.prettyprint() << ": "
                            << "index cannot be an array" << std::endl;
                } else {
                    index= state;
                }
            }
        }
        
        if (input && history) {
            Peek::create(construct, body, input, history, index);
        }
    }
    
    void PeekConstruct::onPost() {
        // pop current task
    }
    

} /* end namespace Acotes */ } /* end namespace TL */
