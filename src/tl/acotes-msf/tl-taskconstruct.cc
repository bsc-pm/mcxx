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
// File:   tl-taskconstruct.cc
// Author: drodenas
//
// Created on 19 / desembre / 2007, 13:15
//

#include "tl-taskconstruct.h"

#include <tl-pragmasupport.hpp>

#include "ac-forreplicate.h"
#include "ac-port.h"
#include "ac-task.h"
#include "ac-state.h"
#include "ac-variable.h"
#include "tl-acoteslogger.h"
#include "tl-acotesstack.h"
#include "tl-variableclause.h"

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * LangConstruct support
     * ****************************************************************/
    
    TaskConstruct::TaskConstruct(TL::LangConstruct langConstruct, DTO& dto)
    : TL::PragmaCustomConstruct(langConstruct.get_ast(), langConstruct.get_scope_link()),
    _dto(dto)
    {
    }

    TL::LangConstruct TaskConstruct::getBody() {
        PragmaCustomConstruct construct(getConstruct().get_ast(), getConstruct().get_scope_link());
        return construct.get_statement();
    }
    
    TL::LangConstruct TaskConstruct::getConstruct() {
        return *this;
    }

    
    /* ****************************************************************
     * * CompilerPhase events
     * ****************************************************************/
    
    void TaskConstruct::onPre() {
        // retrieve information
        TL::LangConstruct* construct= new TL::LangConstruct(getConstruct());
        TL::LangConstruct* body= new TL::LangConstruct(getBody());
        Taskgroup* taskgroup= AcotesStack::taskgroupTop();
        Task* parentTask= AcotesStack::taskTop();
        
        // create and register current task
        Task* task= Task::create(taskgroup, parentTask, construct, body, _dto);
        AcotesStack::taskPush(task);
        
        onPreTeam(task);
        onPreState(task);
        onPreCopyInState(task);
        onPreCopyOutState(task);
        onPreInitializeState(task);
        onPreFinalizeState(task);
        onPreInputPort(task);
        onPreInputReplicatePort(task);
        onPreOutputPort(task);
        onPreBypass(task);
        onPreAsync(task);
        onPreSync(task);
        onForReplicate(task);
        onPreDevice(task);
    }
    
    void TaskConstruct::onPost() {
        // pop current task
        AcotesStack::taskPop();
    }
    
    void TaskConstruct::onPreTeam(Task* task) {
        const ObjectList<std::string> arguments= get_clause("team").get_arguments(ExpressionTokenizer());
        
        if (arguments.size() == 0) {
        } else if (arguments.size() == 1) {
            std::stringstream ss;
            int team;
            ss << arguments.at(0);
            ss >> team;
            if (team != 0 && task->hasForReplicate()) {
                AcotesLogger::warning(this)
                            << "cannot create teams with forreplicate, team ignored"
                            << std::endl;
            } else {
                task->setTeam(team);
            }
        } else {
            AcotesLogger::error(this) << "team clause has more than one argument." << std::endl;
        }
    }
    
    void TaskConstruct::onPreState(Task* task) {
        VariableClause stateClause(get_clause("state"), task);
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            State::create(variable);
        }
    }
    
    void TaskConstruct::onPreCopyInState(Task* task) {
        VariableClause stateClause(get_clause("copyinstate"), task);
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            State::createCopyIn(variable);
        }
    }
    
    void TaskConstruct::onPreCopyOutState(Task* task) {
        VariableClause stateClause(get_clause("copyoutstate"), task);
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            State::createCopyOut(variable);
        }
    }
    
    void TaskConstruct::onPreInitializeState(Task* task) {
        VariableClause stateClause(get_clause("initializestate"), task);
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            State::create(variable);
            task->addInitializer(variable->getSymbol());
        }
    }
    
    void TaskConstruct::onPreFinalizeState(Task* task) {
        VariableClause stateClause(get_clause("finalizestate"), task);
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            State::create(variable);
            task->addFinalizer(variable->getSymbol());
        }
    }
    
    void TaskConstruct::onPreInputPort(Task* task) {
        VariableClause stateClause(get_clause("input"), task);
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            Port* port= Port::createControlInputPort(variable);
            if (stateClause.hasLabel(i)) {
                port->setName(stateClause.getLabel(i));
            }
        }
    }

    void TaskConstruct::onPreDevice(Task * task) {
        const ObjectList<std::string> arguments= get_clause("device").get_arguments(ExpressionTokenizer());
        if (arguments.size() == 0) {
           printf ("No Device clause\n");
        } else if (arguments.size() == 1) {
            printf ("String arg %s\n", arguments.at(0).c_str());
            if (strcmp (arguments.at(0).c_str(),  "spu") == 0) {
               task->setTaskDevice (TASKDEVICE_SPU);
               printf ("Device spu!!!\n");
            }
            else if (strcmp (arguments.at(0).c_str(),  "ppu") == 0) {
               task->setTaskDevice (TASKDEVICE_PPU);
               printf ("Device ppu!!!\n");
            }
            else {
               task->setTaskDevice (TASKDEVICE_PPU);
               printf ("device not recognized!!!\n");
            }
        } else {
            AcotesLogger::error(this) << "device clause has more than one argument." << std::endl;
        }
    }
    
    void TaskConstruct::onPreInputReplicatePort(Task* task) {
        VariableClause stateClause(get_clause("inputreplicate"), task);
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            Port* port= Port::createControlInputPort(variable);
            if (stateClause.hasLabel(i)) {
                port->setName(stateClause.getLabel(i));
            }
            port->setReplicate(true);
        }
    }
    
    void TaskConstruct::onPreOutputPort(Task* task) {
        VariableClause stateClause(get_clause("output"), task);
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            Port* port= Port::createControlOutputPort(variable);
            if (stateClause.hasLabel(i)) {
                port->setName(stateClause.getLabel(i));
            }
        }
    }
    
    void TaskConstruct::onPreBypass(Task* task) {      
        PragmaCustomClause symbolClause= get_clause("bypass");
        ObjectList<IdExpression> idExpressions= symbolClause.id_expressions();
        for (unsigned i= 0; i < idExpressions.size(); i++) {
            IdExpression idExpression= idExpressions.at(i);
            TL::Symbol symbol= idExpression.get_symbol();
            task->addBypass(symbol);
        }
    }
        
    void TaskConstruct::onPreAsync(Task* task) {
        VariableClause stateClause(get_clause("async"), task);
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            State::createAsyncShared(variable);
        }
    }
    
    void TaskConstruct::onPreSync(Task* task) {
        VariableClause stateClause(get_clause("sync"), task);
        
        for (unsigned i= 0; i < stateClause.getVariableCount(); i++) {
            Variable* variable= stateClause.getVariable(i);
            State::createSyncShared(variable);
        }
    }    
    
    void TaskConstruct::onForReplicate(Task* task) {     
        // FIXME: checkorder and others...
        ObjectList<IdExpression> idExpressions= get_clause("forreplicate").id_expressions();
        for (unsigned i= 0; i < idExpressions.size(); i++) {
            IdExpression idExpression= idExpressions.at(i);
            TL::Symbol symbol= idExpression.get_symbol();
            TL::ForStatement* forStatement= new TL::ForStatement(AcotesStack::getForStatement(symbol));
            ForReplicate::create(forStatement, task);
        }
    }

    
} /* end namespace Acotes */ } /* end namespace TL */

