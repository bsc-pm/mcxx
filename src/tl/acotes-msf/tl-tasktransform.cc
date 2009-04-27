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
#include "tl-tasktransform.h"

#include <assert.h>
#include <sstream>
#include <tl-source.hpp>
#include "ac-port.h"
#include "ac-state.h"
#include "ac-task.h"
#include "tl-acoteslogger.h"
#include "tl-initializertransform.h"
#include "tl-forreplicatetransform.h"
#include "tl-finalizertransform.h"
#include "tl-peektransform.h"
#include "tl-porttransform.h"
#include "tl-sharedtransform.h"
#include "tl-statetransform.h"
#include "tl-teamreplicatetransform.h"
#include "tl-transform.h"
#include "tl-userporttransform.h"
#include "tl-variabletransform.h"

#include "acotes-outputtasks.hpp"

namespace TL { namespace Acotes {
    
    
    
            
    /* ****************************************************************
     * * No Constructor
     * ****************************************************************/
     
    TaskTransform::TaskTransform(const std::string& d) : driver(d) {
    }

    
    
    /* ****************************************************************
     * * Transform.
     * ****************************************************************/
    
    /** 
     * Transform the task and its children.
     */
    void TaskTransform::transform(Task* task) {
        assert(task);
        
        // First transform all the children, recursive call
        transformChildren(task);
        transformReplacePeek(task);
        transformReplaceVariable(task);
        transformReplaceUserPort(task);
        transformReplaceSharedCheck(task);
        transformReplaceSharedUpdate(task);
        transformReplaceTeamReplicate(task);
                
        if (!task->isImplicitTask()) {
            transformAddOutline(task);
            transformReplaceConstruct(task);
        }
    }
    
    /**
     * Transform all the task children. 
     * <p>
     * Must be called only by taskgrouptransform or tasktransform.
     */
    void TaskTransform::transformChildren(Task* task) {
        const std::vector<Task*> &children= task->getChildVector();
        for (unsigned i= 0; i < children.size(); i++) {
            Task* child= children.at(i);
            transform(child);
        }
    }
    
    /**
     * Adds the outline.
     */
    void TaskTransform::transformAddOutline(Task* task) {
        assert(task);

        //static int _num_task = 0;
        
        TL::LangConstruct* taskConstruct= task->getConstruct();
        AST_t taskAST= taskConstruct->get_ast();
        ScopeLink taskScopeLink= taskConstruct->get_scope_link();
    
        // Add outline task
        Source outlineSource= generateOutline(task);

        // FIXME - There is 'driver' mechanism, how to deal with it ?
        AST_t outlineTree= outlineSource.parse_global(taskAST, taskScopeLink);
        // taskAST.prepend_sibling_function(outlineTree);

        /* Set up a new outline for further compilation */
        DTO& dto = task->getDTO();
        RefPtr<OutputTasks> output_tasks = RefPtr<OutputTasks>::cast_dynamic(dto["outline_info"]);

        OutputTask output_task;

        output_task.code = outlineTree;
        output_task.scope_link = taskScopeLink;

        std::stringstream ss;
        //_num_task++;
        //ss << _num_task;
        ss << task->getNum();

        CompiledFile current_compiled_file = CompilationProcess::get_current_file();
        std::string current_filename = current_compiled_file.get_filename();
        output_task.task_device = task->getTaskDevice();
        if (output_task.task_device == TASKDEVICE_SPU)
           output_task.filename = "spu_default_" + ss.str() + "_" + current_filename;
        else
           output_task.filename = "ppu_" + ss.str() + "_" + current_filename;
        //printf ("Getting device\n");
        //fflush(NULL);
        //printf ("Getting device %d\n", task->getTaskDevice());
        

        output_tasks->add_task(output_task);
    }
    
    void TaskTransform::transformReplacePeek(Task* task) {
        assert(task);
        
        const std::vector<Peek*> &peeks= task->getPeekVector();
        for (unsigned i= 0; i < peeks.size(); i++) {
            Peek* peek= peeks.at(i);
            Transform::I(driver)->peek()->transform(peek);
        }
    }
    
    void TaskTransform::transformReplaceVariable(Task* task) {
        assert(task);
        
        const std::vector<Variable*> &variables= task->getVariableVector();
        for (unsigned i= 0; i < variables.size(); i++) {
            Variable* variable= variables.at(i);
            Transform::I(driver)->variable()->transformReplacement(variable);
        }
    }
    
    void TaskTransform::transformReplaceUserPort(Task* task) {
        assert(task);
        
        const std::vector<UserPort*> &uports= task->getUserPortVector();
        for (unsigned i= 0; i < uports.size(); i++) {
            UserPort* userPort= uports.at(i);
            Transform::I(driver)->userPort()->transform(userPort);
        }
    }
    
    void TaskTransform::transformReplaceSharedCheck(Task* task) {
        assert(task);
        
        const std::vector<SharedCheck*> &shareds= task->getSharedCheckVector();
        for (unsigned i= 0; i < shareds.size(); i++) {
            SharedCheck* sharedCheck= shareds.at(i);
            Transform::I(driver)->shared()->transform(sharedCheck);
        }
    }
    
    void TaskTransform::transformReplaceSharedUpdate(Task* task) {
        assert(task);
        
        const std::vector<SharedUpdate*> &shareds= task->getSharedUpdateVector();
        for (unsigned i= 0; i < shareds.size(); i++) {
            SharedUpdate* sharedUpdate= shareds.at(i);
            Transform::I(driver)->shared()->transform(sharedUpdate);
        }
    }

    void TaskTransform::transformReplaceTeamReplicate(Task* task) {
        assert(task);
        
        const std::vector<TeamReplicate*> &replicates= task->getTeamReplicateVector();
        for (unsigned i= 0; i < replicates.size(); i++) {
            TeamReplicate* replicate= replicates.at(i);
            Transform::I(driver)->teamReplicate()->transform(replicate);
        }
    }

    /**
     * Adds the outline.
     */
    void TaskTransform::transformReplaceConstruct(Task* task) {
        assert(task);
        
        TL::LangConstruct* taskConstruct= task->getConstruct();
        AST_t taskAST= taskConstruct->get_ast();
        ScopeLink taskScopeLink= taskConstruct->get_scope_link();
    
        // Replace taskgroup construct
        Source replaceSource= generateReplacement(task);
        AST_t replaceTree= replaceSource.parse_statement(taskAST, taskScopeLink);
        taskAST.replace(replaceTree);
    }

    
    
    /* ****************************************************************
     * * Outline generation
     * ****************************************************************/
    
    std::string TaskTransform::generateOutline(Task* task) {
        assert(task);
        
        std::stringstream ss;

#ifdef PARAMS_STRUCT
        ss << "typedef struct " << task->getName() << "_tag {"
           << "   int ntask;"
           << "}" << task->getName() << "_param_t;";
        
        ss << "void " << task->getName() << "_outline("
                      << task->getName() << "_param_t * params_p)"
#else
        ss << "void " << task->getName() << "_outline("
                      << "void * params_p)"
#endif
                << "{"
                //<<   "trace_instance_begin();"
                <<   generateVariable(task)
                <<   "printf (\"IN "
                <<                 task->getName() << "\\n\");"
		<<   "if (0 /* MSF_TASK_INIT */ == msf_get_task_state()) {"
                <<       "printf (\"INIT " 
                <<                 task->getName() << "\\n\");"
		//<<       "msf_set_task_parms_size(sizeof(*params_p));"
                <<       "msf_set_task_parms_size(0);"
		// msf_add_buffer_ports...
		<<       generateBufferPorts(task)
		<<       "msf_update_framework();"
                <<       "printf (\"FINIT "
                <<                 task->getName() << "\\n\");"
		<<       "return;"
                <<   "}"
                <<   "printf (\"START "
                <<             task->getName() << "\\n\");"
                ;

	ss
                <<   generateCopyInAcquire(task)
                <<   generateSharedAcquire(task)
                <<   generateInitializer(task)
                <<   generateControlAcquire(task)
                <<   generateNelemsBufferPorts(task) 
                ;
//                <<   "while (task_allopen())"
        if (hasInput(task)) {
           ss      <<   generate_task_allopen_condition(task);
        }
        ss      <<   generateForReplicate(task)
                <<   "{"
                <<      generateControlSharedCheck(task)
                ;
        if (task->hasLeader()) {
            ss  <<      "if (task_leader())"
                <<      "{"
                ;
        }
// AQUI4 was
	//ss	<<	   generateControlOutputBufferAccess(task)
	//	<<	   generateControlInputBufferAccess(task)
	//	<<	   generateBody(task)
	ss	<< generateControlOutputBufferAccess(task);
        ss      << generateControlInputBufferAccess(task);
        ss      << generateBody(task);




//		<<	   generateBody(task)
//                <<	   generateControlOutputBufferAccess(task)
//		<<	   generateControlInputBufferAccess(task)
         
//                ;
#if 0
        ss      <<         generateControlInputPeek(task)
                <<         generateBody(task)
                <<         generateControlOutputPeek(task)
                <<         generateControlPop(task)
                <<         generateControlPush(task)
                <<         generateControlAcquire(task)
                ;
#endif
        if (task->hasLeader()) {
            ss  <<      "} else { "
                <<         generateReplicatePeek(task)
                <<         generateReplicateBody(task)
                <<         generateReplicatePop(task)
                <<         generateReplicateAcquire(task)
                <<      "}"
                ;
        }
        ss      <<   "}"
                <<   generateFinalizer(task)
                <<   generateCopyOutAcquire(task)
                     // write data on output ports
                <<   generateCommitPorts(task)
                //<<   "msf_commit_written_data();"
                //<<   "trace_instance_end();"
                << "}"
                ;
/* xavim */

//        ss      << "void _SPUEAR_"
//                <<         task->getName() << "_outline("
//                       << task->getName() << "_param_t ** __params)"
//		<< "{"
//                <<     task->getName() << "_outline(* __params);"
//                << "}"
//                << "void (* __worker_"
//                << task->getName() << "_outline)("
//                       << task->getName() << "_param_t *) "
//                <<    "__attribute__((section(\".spu.worker_functions.p.c\"),"
//                        "aligned(4)))  = _SPUEAR_"
//                <<       task->getName() 
//                << "_outline;"
//                ;
/* mivax */

        return ss.str();
    }
    
    std::string TaskTransform::generateForReplicate(Task* task) {
        assert(task);
        
        std::stringstream ss;

        const std::vector<ForReplicate*> &replicates= task->getForReplicateVector();
        for (unsigned i= 0; i < replicates.size(); i++) {
            ForReplicate* forReplicate= replicates.at(i);
            ss << Transform::I(driver)->forReplicate()->generateFor(forReplicate);
        }
        
        return ss.str();
    }
    
    /**
     * Generates the variables declaration of the task.
     */
    std::string TaskTransform::generateVariable(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Variable*> &variables= task->getVariableVector();
        for (unsigned i= 0; i < variables.size(); i++) {
            Variable* variable= variables.at(i);
            ss << Transform::I(driver)->variable()->generateVariable(variable);
        }
        
        return ss.str();
    }
    
    std::string TaskTransform::generateInitializer(Task* task) {
        return Transform::I(driver)->initializer()->generate(task);
    }
    
    std::string TaskTransform::generateFinalizer(Task* task) {
        return Transform::I(driver)->finalizer()->generate(task);
    }
    
    /**
     * Generates the copyin acquires of the variables.
     */
    std::string TaskTransform::generateCopyInAcquire(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<State*> &states= task->getStateVector();
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            if (state->isCopyIn()) {
                ss << Transform::I(driver)->state()->generateCopyInAcquire(state);
            }
        }
        
        return ss.str();
    }
    
    /**
     * Generates the copyout acquires of the variables.
     */
    std::string TaskTransform::generateCopyOutAcquire(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<State*> &states= task->getStateVector();
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            if (state->isCopyOut()) {
                ss << Transform::I(driver)->state()->generateCopyOutAcquire(state);
            }
        }
        
        return ss.str();
    }
    
    /**
     * 
     */
    std::string TaskTransform::generateSharedAcquire(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<State*> &states= task->getStateVector();
        printf ("TaskTransform::generateSharedAcquire task %s\n", task->getName().c_str());
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            if (state->isShared() || state->isUpdateShared()) {
                ss << Transform::I(driver)->shared()->generateAcquire(state);
            }
        }
        
        return ss.str();
    }
    
    /**
     * Generates the taskbody.
     */
    std::string TaskTransform::generateBody(Task* task) 
    {
       assert(task);
        
       std::stringstream ss;
       printf ("Generate task body + tracing\n");
       ss << "trace_iteration_begin();";
       ss << task->getBody()->prettyprint();
       ss << "trace_iteration_end();";
       
       return ss.str();
    }

    std::string TaskTransform::generateControlAcquire(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        printf ("commented TaskTransform::generateControlAcquire task %s\n", task->getName().c_str());
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl()) {
                //ss << Transform::I(driver)->port()->generateAcquire_task(port);
            }
        }
        
        return ss.str();
    }

    std::string TaskTransform::generateControlSharedCheck(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<State*> &states= task->getStateVector();
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            if (state->isShared() || state->isUpdateShared()) {
                ss << Transform::I(driver)->shared()->generateCheck(state);
            }
        }
        
        return ss.str();
    }

    std::string TaskTransform::generateControlInputPeek(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isInput()) {
                ss << Transform::I(driver)->port()->generateInputPeek(port);
            }
        }
        
        return ss.str();
    }

    std::string TaskTransform::generateControlInputBufferAccess(Task* task) {
        assert(task);

        std::stringstream ss;

        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isInput()) {
                ss << Transform::I(driver)->port()->generateInputBufferAccess(port);
            }
        }

        return ss.str();
    }

    bool TaskTransform::hasInput (Task * task)
    {
        assert(task);
       
        const std::vector<Port*> &ports= task->getPortVector();
        printf ("Task %d\n", task->getNum());
        for (unsigned i= 0; i < ports.size(); i++) {
           Port* port= ports.at(i);
           printf ("  Port %d input %d output %d variable %d is control %d\n",
                   i, port->isInput() ? 1 : 0, port->isOutput() ? 1 : 0,
                      port->hasVariable() ? 1 : 0,
                      port->isControl() ? 1 : 0);
           if (port->isInput() && port->hasVariable()) return true;
        }
        return false;
    }

    std::string TaskTransform::generateControlOutputBufferAccess(Task* task) {
        assert(task);

        std::stringstream ss;

        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            //if (port->isControl() && port->isOutput()) {
            if (port->isOutput()) {
                ss << Transform::I(driver)->port()->generateOutputBufferAccess(port);
            }
        }

        return ss.str();
    }


    std::string TaskTransform::generateControlOutputPeek(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isOutput()) {
                ss << Transform::I(driver)->port()->generateOutputPeek(port);
            }
        }
        
        return ss.str();
    }

    std::string TaskTransform::generateControlPop(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isInput() && !port->hasPeek()) {
                ss << Transform::I(driver)->port()->generatePop(port);
            }
        }
        
        return ss.str();
    }

    std::string TaskTransform::generateControlPush(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isOutput()) {
                ss << Transform::I(driver)->port()->generatePush(port);
            }
        }
        
        return ss.str();
    }
    
    std::string TaskTransform::generateReplicatePeek(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isInput() && port->isReplicate()) {
                ss << Transform::I(driver)->port()->generateInputPeek(port);
            }
        }
        
        return ss.str();
    }
        
    std::string TaskTransform::generateReplicateBody(Task* task) {
        assert(task);
        
        std::stringstream ss;

        ss << "trace_teamreplicate_begin();";
        const std::vector<TeamReplicate*> &replicates= task->getTeamReplicateVector();
        for (unsigned i= 0; i < replicates.size(); i++) {
            TeamReplicate* teamReplicate= replicates.at(i);
            ss << Transform::I(driver)->teamReplicate()->generateReplicate(teamReplicate);
        }
        ss << "trace_teamreplicate_end();";
        
        return ss.str();
    }

    std::string TaskTransform::generateReplicatePop(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isInput() && port->isReplicate()) {
                ss << Transform::I(driver)->port()->generatePop(port);
            }
        }
        
        return ss.str();
    }

    std::string TaskTransform::generateReplicateAcquire(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        printf ("TaskTransform::generateReplicateAcquire task %s\n", task->getName().c_str());
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isReplicate()) {
                assert(port->isInput());
                ss << Transform::I(driver)->port()->generateAcquire(port);
            }
        }
        
        return ss.str();
    }


            
    /* ****************************************************************
     * * Replacement generation
     * ****************************************************************/
    
    std::string TaskTransform::generateReplacement(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        ss      << "{"
                << generateArtificialPush(task)
                << generateArtificialPop(task)
                << "}"
                ;
        
        return ss.str();
    }
    
    std::string TaskTransform::generateArtificialPush(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        printf ("TaskTransform::generateArtificialPush task %s\n", task->getName().c_str());
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            Port* artificialCounterpart= port->getArtificialCounterpart();
            if (artificialCounterpart && artificialCounterpart->isOutput()) {
                ss << Transform::I(driver)->port()->generateAcquire(artificialCounterpart);
                ss << Transform::I(driver)->port()->generateOutputPeek(artificialCounterpart);
                ss << Transform::I(driver)->port()->generatePush(artificialCounterpart);
                ss << "kkk();";
            }
        }
        
        return ss.str();
    }

    std::string TaskTransform::generateArtificialPop(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        printf ("TaskTransform::generateArtificialPop task %s\n", task->getName().c_str());
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            Port* artificialCounterpart= port->getArtificialCounterpart();
            if (artificialCounterpart && artificialCounterpart->isInput()) {
                ss << Transform::I(driver)->port()->generateAcquire(artificialCounterpart);
                ss << Transform::I(driver)->port()->generateInputPeek(artificialCounterpart);
                ss << Transform::I(driver)->port()->generatePop(artificialCounterpart);
            }
        }
        
        return ss.str();
    }

    
    
    /* ****************************************************************
     * * Taskgroup replacement generation support
     * ****************************************************************/
    
    /**
     * Generates the start code for the taskgroup.
     */
    std::string TaskTransform::generateInit(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        if (task->isImplicitTask()) {
            // nothing to do...
            ss << "";
            //ss  <<   "(void*)0";
	    //ss  <<   ", (void*)0";
        } else {
            ss      << "msf_task_handle_p " << task->getName() << ";"
                    << task->getName() << " = msf_task_load(";
            if (task->getTaskDevice() == TASKDEVICE_SPU) {
              ss  <<   "\"" << task->getName() << "_lib\"";
              ss  <<   ", \"spe_prog_" << task->getName() << "_outline\", 0L, 0";
            }
            else {
              ss  <<   "\"libmsf_ppu.so\"";
              ss  <<   ", \"" << task->getName() << "_outline\", 0L, 0";
            }
            ss      <<   ");"
            ;
        }
	// team (xavim)
        //ss      <<   ", " << task->getTeam()
        
        return ss.str();
    }
    
    /**
     * Generates the start code for the taskgroup.
     */
    std::string TaskTransform::generatePorts(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            ss << Transform::I(driver)->port()->generatePort(port);
        }
        
        return ss.str();
    }

    /**
     * Generates the init code for the task.
     */
    std::string TaskTransform::generateBufferPorts(Task* task) {
        assert(task);

        std::stringstream ss;

        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            ss << Transform::I(driver)->port()->generateBufferPort(port);
        }

        return ss.str();
    }

    /**
     * Generates the end of task condition 
     */
    std::string TaskTransform::generate_task_allopen_condition(Task* task) {
        assert(task);

        std::stringstream ss;

        ss << "while (";

        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isInput()) {
//printf ("next port\n"); fflush (NULL);
               ss << Transform::I(driver)->port()->generate_endofstream_condition(port)
                  << " && "
               ;
            }
        }

        ss << "1)" ;
        return ss.str();
    }

    /**
     * Generates the code to get nelems for each input port in the task.
     */
    std::string TaskTransform::generateNelemsBufferPorts(Task* task) {
        assert(task);

        std::stringstream ss;

        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            ss << Transform::I(driver)->port()->generateNelemsBufferPort(port);
        }

        return ss.str();
    }

    /**
     * Generates commits
     */

    std::string TaskTransform::generateCommitPorts(Task* task) {
        assert(task);

        std::stringstream ss;

        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            ss << Transform::I(driver)->port()->generateCommitBufferPort(port);
        }
        return ss.str();
    }

    /**
     * Generates the start code for the taskgroup.
     */
    std::string TaskTransform::generateShareds(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<State*> &states= task->getStateVector();
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            if (state->isShared() || state->isUpdateShared()) {
                ss << Transform::I(driver)->shared()->generateShared(state);
            }            
        }
        
        return ss.str();
    }
    
    /**
     * Generates the start code for the taskgroup.
     */
    std::string TaskTransform::generateStart(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        //ss << "task_start(" << task->getName() << ");";
        //msf_set_task_state(reader_task, MSF_TASK_READY);
        if (task->isImplicitTask())
           ss << "";
        else {
           ss << "msf_set_task_runtime(" << task->getName() << ", 10);";
           ss << "msf_app_set_task_state(" << task->getName() 
              << ", MSF_TASK_READY);";
        }
        
        return ss.str();
    }
    
    /**
     * Generates the start code for the taskgroup.
     */
    std::string TaskTransform::generateWait(Task* task) {
        assert(task);
        
        std::stringstream ss;
        
        //ss << "task_wait(" << task->getName() << ");";
      if (task->isImplicitTask())
        ss << "";
      else {
        ss << "printf (\"Waiting for tasks to finish\\n\");";
        ss << "while (MSF_TASK_REST != msf_app_get_task_state("
           << task->getName() << ")) {"
           << "   printf (\"task state %d\\n\", msf_app_get_task_state("
           << task->getName() << "));"
           << "sleep (1);"
           << "break;"
           << "}";
          ss << "";
      }
        return ss.str();
    }
     
    
} /* end namespace Acotes */ } /* end namespace TL */
