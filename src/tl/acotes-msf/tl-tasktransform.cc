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
        //transformReplaceUserPort2(task);
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
        printf ("Getting device\n");
        fflush(NULL);
        printf ("Getting device %d\n", task->getTaskDevice());
        printf ("filename %s\n", output_task.filename.c_str());
        fflush(NULL);
        

        output_tasks->add_task(output_task);
        printf ("Task added\n");
        fflush(NULL);

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
            int last = (i==uports.size()-1);
            printf ("Userport last %d\n", last);
            Transform::I(driver)->userPort()->transform(userPort, last);
            //Transform::I(driver)->userPort()->transform(userPort);
        }
    }
    
    void TaskTransform::transformReplaceUserPort2(Task* task) {
        assert(task);
        
        const std::vector<UserPort*> &uports= task->getUserPortVector();
        for (unsigned i= 0; i < uports.size(); i++) {
            UserPort* userPort= uports.at(i);
            Transform::I(driver)->userPort()->transform2(userPort);
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
    
    Source TaskTransform::generateOutline(Task* task) {
        assert(task);
        
        Source ss;

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
                <<   comment ("MSF_BUFFER_PORT_ACTIVE")
                <<   "int __transfer_type = 0; /* MSF_BUFFER_PORT_ACTIVE */"
                <<   "int __endofoutput = 0;"
                ;

	ss
                <<   generateCopyInAcquire(task)
                <<   generateSharedAcquire(task)
                <<   generateInitializer(task)
                <<   generateControlAcquire(task)
                <<   generateNelemsBufferPorts(task) 
                ;
//                <<   "while (task_allopen())"
        //if (hasInput(task)) {
           ss      <<   generate_task_allopen_condition(task);
        //}
        //else {
           //ss      <<   "while (1)";
        //}
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
        //ss      << "hello();"  ;
        ss      << "__endofoutput = 0;";
        ss      << generateBody(task);
	ss      << generatePreviousBufferWrite(task);
        ss      << generateEOF(task);




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
                <<   comment ("MSF_BUFFER_PORT_LAST")
                <<   "if (1 == __transfer_type) {"
                <<   comment ("MSF_TASK_REST")
                <<   "msf_set_task_state(2);"
                <<   "}"
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

        return ss;
    }
    
    Source TaskTransform::generateForReplicate(Task* task) {
        assert(task);
        
        Source ss;

        const std::vector<ForReplicate*> &replicates= task->getForReplicateVector();
        for (unsigned i= 0; i < replicates.size(); i++) {
            ForReplicate* forReplicate= replicates.at(i);
            ss << Transform::I(driver)->forReplicate()->generateFor(forReplicate);
        }
        
        return ss;
    }
    
    /**
     * Generates the variables declaration of the task.
     */
    Source TaskTransform::generateVariable(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<Variable*> &variables= task->getVariableVector();
        for (unsigned i= 0; i < variables.size(); i++) {
            Variable* variable= variables.at(i);
            ss << Transform::I(driver)->variable()->generateVariable(variable);
        }
        
        return ss;
    }
    
    Source TaskTransform::generateInitializer(Task* task) {
        return Transform::I(driver)->initializer()->generate(task);
    }
    
    Source TaskTransform::generateFinalizer(Task* task) {
        return Transform::I(driver)->finalizer()->generate(task);
    }
    
    /**
     * Generates the copyin acquires of the variables.
     */
    Source TaskTransform::generateCopyInAcquire(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<State*> &states= task->getStateVector();
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            if (state->isCopyIn()) {
                ss << Transform::I(driver)->state()->generateCopyInAcquire(state);
            }
        }
        
        return ss;
    }
    
    /**
     * Generates the copyout acquires of the variables.
     */
    Source TaskTransform::generateCopyOutAcquire(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<State*> &states= task->getStateVector();
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            if (state->isCopyOut()) {
                ss << Transform::I(driver)->state()->generateCopyOutAcquire(state);
            }
        }
        
        return ss;
    }
    
    /**
     * 
     */
    Source TaskTransform::generateSharedAcquire(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<State*> &states= task->getStateVector();
        printf ("TaskTransform::generateSharedAcquire task %s\n", task->getName().c_str());
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            if (state->isShared() || state->isUpdateShared()) {
                ss << Transform::I(driver)->shared()->generateAcquire(state);
            }
        }
        
        return ss;
    }
    
    /**
     * Generates the taskbody.
     */
    Source TaskTransform::generateBody(Task* task) 
    {
       assert(task);
        
       Source ss;
       printf ("Generate task body + tracing\n");
       if (hasInput(task)) {
          ss << "while (1) {";
          ss << generate_input_connections(task);
       }
       //ss << "trace_iteration_begin();";
       ss << task->getBody()->prettyprint();
       //ss << "trace_iteration_end();";
       if (hasInput(task)) {
          ss << generate_output_connections(task);
          ss << "if (__endofoutput == 1) break;";
          ss << "}";
       }
       
       return ss;
    }

    Source TaskTransform::generate_input_connections(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        printf ("commented TaskTransform::generate_input_connections task %s\n", task->getName().c_str());
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isInput()) {
                ss << Transform::I(driver)->port()->generateAcquire_task(port);
            }
        }
        
        return ss;
    }

    Source TaskTransform::generate_output_connections(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        printf ("commented TaskTransform::generate_input_connections task %s\n", task->getName().c_str());
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isOutput()) {
                ss << Transform::I(driver)->port()->generateAcquire_task(port);
            }
        }
        
        return ss;
    }

    Source TaskTransform::generateControlAcquire(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        printf ("commented TaskTransform::generateControlAcquire task %s\n", task->getName().c_str());
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl()) {
                //ss << Transform::I(driver)->port()->generateAcquire_task(port);
            }
        }
        
        return ss;
    }

    Source TaskTransform::generateControlSharedCheck(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<State*> &states= task->getStateVector();
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            if (state->isShared() || state->isUpdateShared()) {
                ss << Transform::I(driver)->shared()->generateCheck(state);
            }
        }
        
        return ss;
    }

    Source TaskTransform::generateControlInputPeek(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isInput()) {
                ss << Transform::I(driver)->port()->generateInputPeek(port);
            }
        }
        
        return ss;
    }

    Source TaskTransform::generateControlInputBufferAccess(Task* task) {
        assert(task);

        Source ss;

        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isInput()) {
                ss << Transform::I(driver)->port()->generateInputBufferAccess(port);
            }
        }

        return ss;
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

    Source TaskTransform::generateControlOutputBufferAccess(Task* task) {
        assert(task);

        Source ss;

        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            //if (port->isControl() && port->isOutput()) {
            if (port->isOutput()) {
                ss << Transform::I(driver)->port()->generateOutputBufferAccess(port);
            }
        }

        return ss;
    }


    Source TaskTransform::generateControlOutputPeek(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isOutput()) {
                ss << Transform::I(driver)->port()->generateOutputPeek(port);
            }
        }
        
        return ss;
    }

    Source TaskTransform::generateControlPop(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isInput() && !port->hasPeek()) {
                ss << Transform::I(driver)->port()->generatePop(port);
            }
        }
        
        return ss;
    }

    Source TaskTransform::generateControlPush(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isOutput()) {
                ss << Transform::I(driver)->port()->generatePush(port);
            }
        }
        
        return ss;
    }
    
    Source TaskTransform::generateReplicatePeek(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isInput() && port->isReplicate()) {
                ss << Transform::I(driver)->port()->generateInputPeek(port);
            }
        }
        
        return ss;
    }
        
    Source TaskTransform::generateReplicateBody(Task* task) {
        assert(task);
        
        Source ss;

        ss << "trace_teamreplicate_begin();";
        const std::vector<TeamReplicate*> &replicates= task->getTeamReplicateVector();
        for (unsigned i= 0; i < replicates.size(); i++) {
            TeamReplicate* teamReplicate= replicates.at(i);
            ss << Transform::I(driver)->teamReplicate()->generateReplicate(teamReplicate);
        }
        ss << "trace_teamreplicate_end();";
        
        return ss;
    }

    Source TaskTransform::generateReplicatePop(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isInput() && port->isReplicate()) {
                ss << Transform::I(driver)->port()->generatePop(port);
            }
        }
        
        return ss;
    }

    Source TaskTransform::generateReplicateAcquire(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        printf ("TaskTransform::generateReplicateAcquire task %s\n", task->getName().c_str());
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            if (port->isControl() && port->isReplicate()) {
                assert(port->isInput());
                ss << Transform::I(driver)->port()->generateAcquire(port);
            }
        }
        
        return ss;
    }


            
    /* ****************************************************************
     * * Replacement generation
     * ****************************************************************/
    
    Source TaskTransform::generateReplacement(Task* task) {
        assert(task);
        
        Source ss;
        
        ss      << "{";
        //if (!task->isImplicitTask())
        //      ss  << " if (__endofoutput == 1) break;"; // should be done once!
        ss      << generateArtificialPush(task)
                << generateArtificialPop(task)
                << "}"
                ;
        
        return ss;
    }
    
    Source TaskTransform::generateArtificialPush(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        printf ("TaskTransform::generateArtificialPush task %s\n", task->getName().c_str());
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            Port* artificialCounterpart= port->getArtificialCounterpart();
            if (artificialCounterpart && artificialCounterpart->isOutput()) {
                ss << Transform::I(driver)->port()->generateAcquire(artificialCounterpart);
                ss << Transform::I(driver)->port()->generateOutputPeek(artificialCounterpart);
                ss << Transform::I(driver)->port()->generatePush(artificialCounterpart);
                //ss << "kkk();";
            }
        }
        
        return ss;
    }

    Source TaskTransform::generateArtificialPop(Task* task) {
        assert(task);
        
        Source ss;
        
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
        
        return ss;
    }

    
    
    /* ****************************************************************
     * * Taskgroup replacement generation support
     * ****************************************************************/
    
    /**
     * Generates the start code for the taskgroup.
     */
    Source TaskTransform::generateInit(Task* task) {
        assert(task);
        
        Source ss;
        
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
        
        return ss;
    }
    
    /**
     * Generates the start code for the taskgroup.
     */
    Source TaskTransform::generatePorts(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            ss << Transform::I(driver)->port()->generatePort(port);
        }
        
        return ss;
    }

    /**
     * Generates the init code for the task.
     */
    Source TaskTransform::generateBufferPorts(Task* task) {
        assert(task);

        Source ss;

        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            ss << Transform::I(driver)->port()->generateBufferPort(port);
        }

        return ss;
    }

    /**
     * Generates the end of task condition 
     */
    Source TaskTransform::generate_task_allopen_condition(Task* task) {
        assert(task);

        Source ss;

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
            else if (port->isOutput()) {
               ss << Transform::I(driver)->port()->generate_continue_condition(port)
                  << " && "
               ;
            }
        }

        ss << "1)" ;
        return ss;
    }

    /**
     * Generates the code to get nelems for each input port in the task.
     */
    Source TaskTransform::generateNelemsBufferPorts(Task* task) {
        assert(task);

        Source ss;

        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            ss << Transform::I(driver)->port()->generateNelemsBufferPort(port);
        }

        return ss;
    }

#if 1
    /**
     * Generates the code to write each port buffer.
     */
    Source TaskTransform::generateEOF(Task* task) {
        assert(task);

        Source ss;

        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            ss << Transform::I(driver)->port()->generateEndOfFile(port);
        }

        return ss;
    }
#endif

    /**
     * Generates the code to write each port buffer.
     */
    Source TaskTransform::generatePreviousBufferWrite(Task* task) {
        assert(task);

        Source ss;

        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            ss << Transform::I(driver)->port()->generatePrevBufferWrite(port);
        }

        return ss;
    }

    /**
     * Generates commits
     */

    Source TaskTransform::generateCommitPorts(Task* task) {
        assert(task);

        Source ss;

        const std::vector<Port*> &ports= task->getPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            ss << Transform::I(driver)->port()->generateCommitBufferPort(port);
        }
        return ss;
    }

    /**
     * Generates the start code for the taskgroup.
     */
    Source TaskTransform::generateShareds(Task* task) {
        assert(task);
        
        Source ss;
        
        const std::vector<State*> &states= task->getStateVector();
        for (unsigned i= 0; i < states.size(); i++) {
            State* state= states.at(i);
            if (state->isShared() || state->isUpdateShared()) {
                ss << Transform::I(driver)->shared()->generateShared(state);
            }            
        }
        
        return ss;
    }
    
    /**
     * Generates the start code for the taskgroup.
     */
    Source TaskTransform::generateStart(Task* task) {
        assert(task);
        
        Source ss;
        
        //ss << "task_start(" << task->getName() << ");";
        //msf_set_task_state(reader_task, MSF_TASK_READY);
        if (task->isImplicitTask())
           ss << "";
        else {
           ss << "msf_set_task_runtime(" << task->getName() << ", 10);";
           ss << "msf_app_set_task_state(" << task->getName() 
              << ", MSF_TASK_READY);";
        }
        
        return ss;
    }
    
    /**
     * Generates the start code for the taskgroup.
     */
    Source TaskTransform::generateWait(Task* task) {
        assert(task);
        
        Source ss;
        
        //ss << "task_wait(" << task->getName() << ");";
      if (task->isImplicitTask())
        ss << "";
      else {
        ss << "printf (\"Waiting for tasks to finish\\n\");";
        ss << "while (MSF_TASK_REST != msf_app_get_task_state("
           << task->getName() << ")) {"
           //<< "   printf (\"task " << task->getName() << " state %d\\n\", msf_app_get_task_state("
           //<< task->getName() << "));"
           << "usleep (1000);"
           ////////////////////NOOOOOOOOOOO<< "break;"
           << "}";
           
          ss << "printf (\"Wake up from task '" << task->getName()
             << "'\\n\");";
      }
        return ss;
    }
     
    
} /* end namespace Acotes */ } /* end namespace TL */
