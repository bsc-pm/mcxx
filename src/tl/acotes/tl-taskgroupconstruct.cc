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
// 
// File:   tl-taskgroupconstruct.cc
// Author: drodenas
//
// Created on 19 / desembre / 2007, 13:15
//

#include "tl-taskgroupconstruct.h"

#include <tl-pragmasupport.hpp>

#include "ac-task.h"
#include "ac-taskgroup.h"
#include "tl-acotesstack.h"

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * LangConstruct support
     * ****************************************************************/
    
    TaskgroupConstruct::TaskgroupConstruct(TL::LangConstruct langConstruct)
    : TL::PragmaCustomConstruct(langConstruct.get_ast(), langConstruct.get_scope_link())
    {
    }

    TL::LangConstruct TaskgroupConstruct::getBody() {
        PragmaCustomConstruct construct(get_ast(), get_scope_link());
        return construct.get_statement();
    }
    
    /* ****************************************************************
     * * CompilerPhase events
     * ****************************************************************/
    
    void TaskgroupConstruct::onPre() {

        TL::LangConstruct* construct= new TL::LangConstruct(getConstruct());
        TL::LangConstruct* body= new TL::LangConstruct(getBody());

        // Create the taskgroup and push
        Taskgroup* taskgroup= Taskgroup::create(construct, body);
        AcotesStack::taskgroupPush(taskgroup);
        
        // Push the implicit task
        Task* task= taskgroup->getImplicitTask();
        AcotesStack::taskPush(task);
        
        onPreBypass(taskgroup);
    }
    
    void TaskgroupConstruct::onPost() {
        Taskgroup* taskgroup= AcotesStack::taskgroupTop();
        taskgroup->createPortConnections();
        
        
        // pop implicit task
        AcotesStack::taskPop();
        
        // pop taskgroup
        AcotesStack::taskgroupPop();
    }
    
    void TaskgroupConstruct::onPreBypass(Taskgroup* taskgroup) {      
        Task* task= taskgroup->getImplicitTask();
        PragmaCustomClause symbolClause= get_clause("bypass");
        ObjectList<IdExpression> idExpressions= symbolClause.id_expressions();
        for (unsigned i= 0; i < idExpressions.size(); i++) {
            IdExpression idExpression= idExpressions.at(i);
            TL::Symbol symbol= idExpression.get_symbol();
            task->addBypass(symbol);
        }
    }
    
    
} /* end namespace Acotes */ } /* end namespace TL */

