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
#include "tl-taskgrouptransform.h"

#include <assert.h>
#include <sstream>
#include "ac-taskgroup.h"
#include "tl-acoteslogger.h"
#include "tl-portconnectiontransform.h"
#include "tl-sharedtransform.h"
#include "tl-statetransform.h"
#include "tl-tasktransform.h"

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * Transform.
     * ****************************************************************/
    
    /**
     * Transforms the taskgroup-
     * <p>
     * Transforms its tasks childrens.
     * Replaces the taskgroup code.
     */
    void TaskgroupTransform::transform(Taskgroup* taskgroup) {
        assert(taskgroup);
        
        TaskTransform::transform(taskgroup->getImplicitTask());
        transformReplaceConstruct(taskgroup);
    }
    
    void TaskgroupTransform::transformReplaceConstruct(Taskgroup* taskgroup) {
                
        TL::LangConstruct* taskgroupConstruct= taskgroup->getConstruct();
        AST_t taskgroupAST= taskgroupConstruct->get_ast();
        ScopeLink taskgroupScopeLink= taskgroupConstruct->get_scope_link();
    
        // Replace taskgroup construct
        Source replaceSource= generateReplacement(taskgroup);
        AST_t replaceTree= replaceSource.parse_statement(taskgroupAST, taskgroupScopeLink);
        taskgroupAST.replace(replaceTree);
    }
    
    
    
    /* ****************************************************************
     * * Replacement generation
     * ****************************************************************/

    /**
     * Generates the source for the replacement of the taskgroup.
     * <p>
     */
    std::string TaskgroupTransform::generateReplacement(Taskgroup* taskgroup) {
        std::stringstream ss;
        
        ss      << "{"
                << generateTasksInit(taskgroup)
                << generateTasksPorts(taskgroup)
                << generateTasksShareds(taskgroup)
                << generatePortConnections(taskgroup)
                << generateCopyState(taskgroup)
                << generateSharedConnections(taskgroup)
                << generateTasksStart(taskgroup)
                << generateBody(taskgroup)
                << "task_close();"
                << generateTasksWait(taskgroup)
                << "}"
                ;
        
        return ss.str();
    }
    
    /**
     * Generates the call to task init.
     */
    std::string TaskgroupTransform::generateTasksInit(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        std::stringstream ss;
        const std::vector<Task*> &tasks= taskgroup->getTaskVector();
        
        for (unsigned i= 0; i < tasks.size(); i++)
        {
            Task* task= tasks.at(i);
            ss << TaskTransform::generateInit(task);
        }
                        
        return ss.str();
    }

    /**
     * Generates the ports of the tasks.
     */
    std::string TaskgroupTransform::generateTasksPorts(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        std::stringstream ss;
        const std::vector<Task*> &tasks= taskgroup->getTaskVector();
        
        for (unsigned i= 0; i < tasks.size(); i++)
        {
            Task* task= tasks.at(i);
            ss << TaskTransform::generatePorts(task);
        }
                        
        return ss.str();
    }
    
    /**
     * Generates the ports of the tasks.
     */
    std::string TaskgroupTransform::generateTasksShareds(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        std::stringstream ss;
        const std::vector<Task*> &tasks= taskgroup->getTaskVector();
        
        for (unsigned i= 0; i < tasks.size(); i++)
        {
            Task* task= tasks.at(i);
            ss << TaskTransform::generateShareds(task);
        }
                        
        return ss.str();
    }
    
    /**
     * Generates the ports of the tasks.
     */
    std::string TaskgroupTransform::generatePortConnections(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        std::stringstream ss;
        const std::vector<PortConnection*> &connections= taskgroup->getPortConnectionVector();
        
        for (unsigned i= 0; i < connections.size(); i++)
        {
            PortConnection* portConnection= connections.at(i);
            ss << PortConnectionTransform::generatePortConnection(portConnection);
        }
                        
        return ss.str();
    }
    
    /**
     * Generates the ports of the tasks.
     */
    std::string TaskgroupTransform::generateCopyState(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        std::stringstream ss;
        const std::vector<State*> &states= taskgroup->getCopyStateVector();
        
        for (unsigned i= 0; i < states.size(); i++)
        {
            State* state= states.at(i);
            ss << StateTransform::generateCopy(state);
        }
                        
        return ss.str();
    }
    
    /**
     * Generates the ports of the tasks.
     */
    std::string TaskgroupTransform::generateSharedConnections(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        std::stringstream ss;
        const std::vector<SharedConnection*> &connections= taskgroup->getSharedConnectionVector();
        
        for (unsigned i= 0; i < connections.size(); i++)
        {
            SharedConnection* sharedConnection= connections.at(i);
            ss << SharedTransform::generateSharedConnection(sharedConnection);
        }
                        
        return ss.str();
    }
    
    /**
     * Generates the start of the tasks.
     */
    std::string TaskgroupTransform::generateTasksStart(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        std::stringstream ss;
        const std::vector<Task*> &tasks= taskgroup->getTaskVector();
        
        for (unsigned i= 0; i < tasks.size(); i++)
        {
            Task* task= tasks.at(i);
            ss << TaskTransform::generateStart(task);
        }
                        
        return ss.str();
    }
    
    /**
     * Generates the wait for the tasks.
     */
    std::string TaskgroupTransform::generateTasksWait(Taskgroup* taskgroup) 
    {
        assert(taskgroup);
        
        std::stringstream ss;
        const std::vector<Task*> &tasks= taskgroup->getTaskVector();
        
        for (int i= tasks.size() - 1; i >= 0; i--)
        {
            Task* task= tasks.at(i);
            ss << TaskTransform::generateWait(task);
        }
                        
        return ss.str();
    }
        
    /**
     * Generates the taskgroupbody.
     */
    std::string TaskgroupTransform::generateBody(Taskgroup* taskgroup) 
    {
       assert(taskgroup);
        
       std::stringstream ss;
       
       ss << "trace_iteration_begin();" << std::endl;
       ss << taskgroup->getBody()->prettyprint();
       ss << "trace_iteration_end();" << std::endl;
       
       return ss.str();
    }

    
    
    /* ****************************************************************
     * * No Constructor
     * ****************************************************************/
     
    TaskgroupTransform::TaskgroupTransform() {
        assert(0);
    }
     
    
} /* end namespace Acotes */ } /* end namespace TL */

