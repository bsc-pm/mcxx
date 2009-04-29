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
#include "tl-sharedtransform.h"

#include <assert.h>
#include <sstream>
#include "ac-sharedcheck.h"
#include "ac-sharedupdate.h"
#include "ac-sharedconnection.h"
#include "ac-state.h"
#include "ac-task.h"
#include "tl-variabletransform.h"
#include "tl-transform.h"

namespace TL { namespace Acotes {
    
    
    /* ****************************************************************
     * * Constructor
     * ****************************************************************/
        
    SharedTransform::SharedTransform(const std::string &d) : driver(d)
    {
    }
    


    /* ****************************************************************
     * * Transform
     * ****************************************************************/
    
    void SharedTransform::transform(SharedCheck* sharedCheck)
    {
        assert(sharedCheck);
        
        transformReplacement(sharedCheck);
    }
        
    void SharedTransform::transform(SharedUpdate* sharedUpdate)
    {
        assert(sharedUpdate);
        
        transformReplacement(sharedUpdate);
    }
    
    void SharedTransform::transformReplacement(SharedCheck* shared)
    {
        assert(shared);
        
        TL::LangConstruct* sharedConstruct= shared->getConstruct();
        AST_t sharedAST= sharedConstruct->get_ast();
        ScopeLink sharedScopeLink= sharedConstruct->get_scope_link();
    
        // Replace taskgroup construct
        Source replaceSource= generateReplacement(shared);
        AST_t replaceTree= replaceSource.parse_statement(sharedAST, sharedScopeLink);
        sharedAST.replace(replaceTree);

    }
     
    Source SharedTransform::generateReplacement(SharedCheck* shared)
    {
        Source ss;
        
        ss      << "{"
                <<   generateCheck(shared)
                << "}"
                ;
        
        return ss;
    }
    
    Source SharedTransform::generateCheck(SharedCheck* shared)
    {
        Source ss;
        
        const std::vector<State*> &states= shared->getStateVector();
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            ss << SharedTransform::generateCheck(state);
        }
        
        return ss;
    }
    
    void SharedTransform::transformReplacement(SharedUpdate* shared)
    {
        assert(shared);
        
        TL::LangConstruct* sharedConstruct= shared->getConstruct();
        AST_t sharedAST= sharedConstruct->get_ast();
        ScopeLink sharedScopeLink= sharedConstruct->get_scope_link();
    
        // Replace taskgroup construct
        Source replaceSource= generateReplacement(shared);
        AST_t replaceTree= replaceSource.parse_statement(sharedAST, sharedScopeLink);
        sharedAST.replace(replaceTree);

    }
     
    Source SharedTransform::generateReplacement(SharedUpdate* shared)
    {
        Source ss;
        
        ss      << "{"
                <<   generateUpdate(shared)
                << "}"
                ;
        
        return ss;
    }
    
    Source SharedTransform::generateUpdate(SharedUpdate* shared)
    {
        Source ss;
        
        const std::vector<State*> &states= shared->getStateVector();
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            ss << SharedTransform::generateUpdate(state);
        }
        
        return ss;
    }
    
        
        
    /* ****************************************************************
     * * Generator
     * ****************************************************************/
    
    Source SharedTransform::generateShared(State* state)
    {
        assert(state);
        assert(state->isShared() || state->isUpdateShared());
        assert(state->hasTask());
        
        Source ss;
        
        Task* task= state->getTask();
        Variable* variable= state->getVariable();
        ss << "task_shared"
                << "( " << task->getName()
                << ", " << state->getNumber()
                << ", " << Transform::I(driver)->variable()->generateReference(variable)
                << ", " << Transform::I(driver)->variable()->generateSizeof(variable)
                << ");";
        
        return ss;

    }
    
    Source SharedTransform::generateSharedConnection(SharedConnection* sharedConnection)
    {
        assert(sharedConnection);
        
        State* source= sharedConnection->getSource();
        Task* sourceTask= source->getTask();
        State* target= sharedConnection->getTarget();
        Task* targetTask= target->getTask();
        
        Source ss;
        
        if (target->isAsyncShared()) {
            ss << "shared_async";
        } else if (target->isSyncShared()) {
            ss << "shared_sync";
        } else {
            assert(0);
        }
        ss      << "( " << sourceTask->getName()
                << ", " << source->getNumber()
                << ", " << targetTask->getName()
                << ", " << target->getNumber()
                << ");";
        
        
        return ss;
    }
    
    Source SharedTransform::generateAcquire(State* state)
    {
        assert(state);
        assert(state->isShared() || state->isUpdateShared());
        assert(state->hasTask());
        
        Source ss;
        
        Variable* variable= state->getVariable();
        ss << "memcpy"
                << "( " << Transform::I(driver)->variable()->generateReference(variable)
                << ", shared_acquire(" << state->getNumber() << ")"
                << ", " << Transform::I(driver)->variable()->generateSizeof(variable)
                << ");";
        
        return ss;

    }
    
    Source SharedTransform::generateCheck(State* state)
    {
        assert(state);
        assert(state->isShared() || state->isUpdateShared());
        assert(state->hasTask());
         
        Source ss;
        
        ss << "shared_check(" << state->getNumber() << ");"
           << generateAcquire(state)
           ;
        
        return ss;

    }
    
    Source SharedTransform::generateUpdate(State* state)
    {
        assert(state);
        assert(state->isShared() || state->isUpdateShared());
        assert(state->hasTask());
        
        Source ss;
        
        Variable* variable= state->getVariable();
        ss << "memcpy"
                << "( shared_acquire(" << state->getNumber() << ")"
                << ", " << Transform::I(driver)->variable()->generateReference(variable)
                << ", " << Transform::I(driver)->variable()->generateSizeof(variable)
                << ");"
           << "shared_update(" << state->getNumber() << ");"
           ;
        
        return ss;

    }
    
    
} /* end namespace Acotes */ } /* end namespace TL */
