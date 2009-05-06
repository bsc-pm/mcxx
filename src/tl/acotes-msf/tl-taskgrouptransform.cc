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
#include "tl-transform.h"

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * Constructor
     * ****************************************************************/
     
    TaskgroupTransform::TaskgroupTransform(const std::string& d) : driver(d) {
    }

    
    
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
        
        Transform::I(driver)->task()->transform(taskgroup->getImplicitTask());
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
    Source TaskgroupTransform::generateReplacement(Taskgroup* taskgroup) {
        Source ss;
        
        ss      << "{"
                << generateBufferSizeInit(taskgroup)
                << generateBufferInfo(taskgroup)
                << generateTasksInit(taskgroup)
                << generateTasksPorts(taskgroup)
                << generateTasksShareds(taskgroup)
                << generatePortConnections(taskgroup)
                << generateCopyState(taskgroup)
                << generateSharedConnections(taskgroup)
                << generateTasksStart(taskgroup)
                << generateBody(taskgroup)
                //<< "task_close();"
                << generateTasksWait(taskgroup)
                << "fflush (0L);"
                << "printf (\"Waiting to end\\n\");"
                //<< "sleep (10);"
                << "printf (\"End of generated code\\n\");"
                << "}"
                ;
        
        return ss;
    }

    Source TaskgroupTransform::generateBufferSizeInit(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        Source ss;
        ss << "extern int acotes__bs[16][16][16][2];";
        ss << "int acotes__tg = " << taskgroup->getNum() << ";";
        ss << "int __endofoutput = 0;";
        return ss;
    }

    Source TaskgroupTransform::generateBufferInfo(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        Source ss;

        ss << "static FILE * f;"
           << "static int acotes__tg" << taskgroup->getNum() << "_gen = 0;"
           << "if (acotes__tg" << taskgroup->getNum() << "_gen == 0) {"
           << "   acotes__tg" << taskgroup->getNum() << "_gen = 1;"
           << "   f = fopen (\"tg" << taskgroup->getNum() << "\", \"w\");"
           << "   if (f!=0L) {"
           << "      fprintf (f, \"digraph taskgroup_" 
                           << taskgroup->getNum() << " {\\n\");"
           <<        generateConnectionInfo(taskgroup)
           << "      fprintf (f, \"}\\n\");"
           << "      fclose (f);"
           << "   }"
           << "}";

        return ss;
    }
    
    /**
     * Generates the call to task init.
     */
    Source TaskgroupTransform::generateTasksInit(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        Source ss;
        const std::vector<Task*> &tasks= taskgroup->getTaskVector();
        
        for (unsigned i= 0; i < tasks.size(); i++)
        {
            Task* task= tasks.at(i);
            ss << Transform::I(driver)->task()->generateInit(task);
        }
                        
        return ss;
    }

    /**
     * Generates the ports of the tasks.
     */
    Source TaskgroupTransform::generateTasksPorts(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        Source ss;
        const std::vector<Task*> &tasks= taskgroup->getTaskVector();
        
        for (unsigned i= 0; i < tasks.size(); i++)
        {
            Task* task= tasks.at(i);
            ss << Transform::I(driver)->task()->generatePorts(task);
        }
                        
        return ss;
    }
    
    /**
     * Generates the ports of the tasks.
     */
    Source TaskgroupTransform::generateTasksShareds(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        Source ss;
        const std::vector<Task*> &tasks= taskgroup->getTaskVector();
        
        for (unsigned i= 0; i < tasks.size(); i++)
        {
            Task* task= tasks.at(i);
            ss << Transform::I(driver)->task()->generateShareds(task);
        }
                        
        return ss;
    }
    
    /**
     * Generates the ports of the tasks.
     */
    Source TaskgroupTransform::generatePortConnections(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        Source ss;
        const std::vector<PortConnection*> &connections= taskgroup->getPortConnectionVector();
        
        for (unsigned i= 0; i < connections.size(); i++)
        {
            PortConnection* portConnection= connections.at(i);
            ss << Transform::I(driver)->portConnection()->generatePortConnection(portConnection);
        }
                        
        return ss;
    }
    
    Source TaskgroupTransform::generateConnectionInfo(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        Source ss;
        const std::vector<PortConnection*> &connections= taskgroup->getPortConnectionVector();
        
        for (unsigned i= 0; i < connections.size(); i++)
        {
            PortConnection* portConnection= connections.at(i);
            ss << Transform::I(driver)->portConnection()->generateConnection(portConnection);
        }
                        
        return ss;
    }
    
    /**
     * Generates the ports of the tasks.
     */
    Source TaskgroupTransform::generateCopyState(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        Source ss;
        const std::vector<State*> &states= taskgroup->getCopyStateVector();
        
        for (unsigned i= 0; i < states.size(); i++)
        {
            State* state= states.at(i);
            ss << Transform::I(driver)->state()->generateCopy(state);
        }
                        
        return ss;
    }
    
    /**
     * Generates the ports of the tasks.
     */
    Source TaskgroupTransform::generateSharedConnections(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        Source ss;
        const std::vector<SharedConnection*> &connections= taskgroup->getSharedConnectionVector();
        
        for (unsigned i= 0; i < connections.size(); i++)
        {
            SharedConnection* sharedConnection= connections.at(i);
            ss << Transform::I(driver)->shared()->generateSharedConnection(sharedConnection);
        }
                        
        return ss;
    }
    
    /**
     * Generates the start of the tasks.
     */
    Source TaskgroupTransform::generateTasksStart(Taskgroup* taskgroup)
    {
        assert(taskgroup);
        
        Source ss;
        const std::vector<Task*> &tasks= taskgroup->getTaskVector();
        
        for (unsigned i= 0; i < tasks.size(); i++)
        {
            Task* task= tasks.at(i);
            ss << Transform::I(driver)->task()->generateStart(task);
        }
                        
        return ss;
    }
    
    /**
     * Generates the wait for the tasks.
     */
    Source TaskgroupTransform::generateTasksWait(Taskgroup* taskgroup) 
    {
        assert(taskgroup);
        
        Source ss;
        const std::vector<Task*> &tasks= taskgroup->getTaskVector();
        
        for (int i= tasks.size() - 1; i >= 0; i--)
        {
            Task* task= tasks.at(i);
            ss << Transform::I(driver)->task()->generateWait(task);
        }
                        
        return ss;
    }
        
    /**
     * Generates the taskgroupbody.
     */
    Source TaskgroupTransform::generateBody(Taskgroup* taskgroup) 
    {
       assert(taskgroup);
        
       Source ss;
       
       //ss << "trace_iteration_begin();" << std::endl;
       ss << taskgroup->getBody()->prettyprint();
       //ss << "trace_iteration_end();" << std::endl;
       
       return ss;
    }
     
    
} /* end namespace Acotes */ } /* end namespace TL */

