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
#include "tl-sharedconstruct.h"

#include "ac-sharedcheck.h"
#include "ac-sharedupdate.h"
#include "tl-acotesstack.h"
#include "tl-variableclause.h"

namespace TL { namespace Acotes {

    /* ****************************************************************
     * * LangConstruct support
     * ****************************************************************/
    
    SharedConstruct::SharedConstruct(TL::LangConstruct langConstruct)
    : TL::PragmaCustomConstruct(langConstruct.get_ast(), langConstruct.get_scope_link())
    {
    }

    TL::LangConstruct SharedConstruct::getConstruct() {
        return *this;
    }

    
    /* ****************************************************************
     * * CompilerPhase events
     * ****************************************************************/
    
    void SharedConstruct::onPre() {
        onPreCheck();
        onPreUpdate();
    }
    
    void SharedConstruct::onPost() {
        // pop current task
    }

    void SharedConstruct::onPreCheck() {
        Task* task= AcotesStack::taskTop();
        VariableClause stateClause(get_clause("check"), task);
        
        if (stateClause.getVariableCount() > 0) {
            // retrieve information
            TL::LangConstruct* construct= new TL::LangConstruct(getConstruct());
            
            SharedCheck* sharedCheck= SharedCheck::create(construct, task);

            for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
                Variable* variable= stateClause.getVariable(i);
                sharedCheck->addVariable(variable);
            }
        }
    }
    
    void SharedConstruct::onPreUpdate() {
        Task* task= AcotesStack::taskTop();
        VariableClause stateClause(get_clause("update"), task);
        
        if (stateClause.getVariableCount() > 0) {
            // retrieve information
            TL::LangConstruct* construct= new TL::LangConstruct(getConstruct());
            
            SharedUpdate* sharedUpdate= SharedUpdate::create(construct, task);

            for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
                Variable* variable= stateClause.getVariable(i);
                sharedUpdate->addVariable(variable);
            }
        }
    }
    

} /* end namespace Acotes */ } /* end namespace TL */
