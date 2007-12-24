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
#include "ac-task.h"

#include <assert.h>
#include "ac-port.h"
#include "ac-portconnection.h"
#include "ac-state.h"
#include "ac-taskgroup.h"
#include "ac-variable.h"

namespace TL { namespace Acotes {


    /* ****************************************************************
     * * Tribal behaviour
     * ****************************************************************/

    std::vector<Task*> Task::instanceVector;
     
    Task* Task::create(Taskgroup* taskgroup, Task* parent, TL::LangConstruct* construct, TL::LangConstruct* body) {
        
        std::stringstream ssname;
        ssname << "task" << instanceVector.size();

        // register the instance
        Task* task= new Task(ssname.str());
        instanceVector.push_back(task);
        
        // initializate fields
        task->setBody(body);
        task->setConstruct(construct);
        
        // create parent-child relationship
        task->setParent(parent);

        // create taskgroup task relationship
        task->setTaskgroup(taskgroup);
        

        return task;
    }

    Task::Task(const std::string& nam) 
    : name(nam)
    , body(NULL), construct(NULL)
    , taskgroup(NULL)
    , parent(NULL) 
    {    
    }
    
    
    
    /* ****************************************************************
     * * Name
     * ****************************************************************/
    
    
    
    /* ****************************************************************
     * * LangConstruct support
     * ****************************************************************/
    
    /**
     * Sets the body lang construct for this task.
     * <p> 
     * The body does not includes the pragma statement. Body
     * must be a valed statement.
     */
    void Task::setBody(TL::LangConstruct* body) {
        assert(body);
        assert(!this->body /* only call once */);
        
        this->body= body;
    }

    /**
     * Sets the pragma lang construct for this task.
     * <p> 
     * The construct must include the pragma statement.
     */
    void Task::setConstruct(TL::LangConstruct* construct) {
        assert(construct);
        assert(!this->construct /* only call once */);
        
        this->construct= construct;
    }
    
    
    
    /* ****************************************************************
     * * Taskgroup relationship
     * ****************************************************************/
    
    /**
     * Links the task with the taskgroup.
     * <p>
     * It also mantains the tasks relationship between taskgroup and
     * task calling to Taskgroup::addTask(Task*) method.
     */
    void Task::setTaskgroup(Taskgroup* taskgroup) {
        assert(taskgroup);
        assert(!this->taskgroup /* only call once */);
        
        this->taskgroup= taskgroup;
        taskgroup->addTask(this);
    }
    
    
    
    /* ****************************************************************
     * * Parent/Child relationship
     * ****************************************************************/
    
    /**
     * Ask if this task is an implicit task of a taskgroup.
     */
    bool Task::isImplicitTask() const
    {
        bool result= getTaskgroup()->getImplicitTask() == this;
        return result;
    }
    
    /**
     * Links this task as child of parent.
     * <p>
     * 
     */
    void Task::setParent(Task* parent) {
        assert(!this->parent /* only call once */);
        
        this->parent= parent;
        if (parent) {
            parent->childVector.push_back(this);
        }
    }
    

    
    /* ****************************************************************
     * * Port relationship
     * ****************************************************************/
    
    /**
     * Adds a new port to the Task.
     * <p>
     * Return the number of the port inside of the task.
     * This method must be called by port and noone else.
     */
    int Task::addPort(Port* port) {
        assert(port);
        
        int portNumber= portVector.size();
        portVector.push_back(port);
        
        return portNumber;
    }
   
    /**
     * Ask if this task has any input control port.
     */
    bool Task::hasInputControlPort() const
    {
        bool result= false;
        
        for (unsigned i= 0; i < portVector.size() && !result; i++) {
            Port* port= portVector.at(i);
            result= port->isInput() && port->isControl();
        }
        
        return result;
    }

    
    
    /* ****************************************************************
     * * State relationship
     * ****************************************************************/
    
    /**
     * Adds a new state to the Task.
     * <p>
     * Return the number of the state inside of the task.
     * This method must be called by state and noone else.
     */
    int Task::addState(State* state) {
        assert(state);
        
        int stateNumber= stateVector.size();
        stateVector.push_back(state);
        
        return stateNumber;
    }

    
    
    /* ****************************************************************
     * * Port connection creation
     * ****************************************************************/
    
    /**
     * Creates the local required port connections for this task.
     */
    void Task::createPortConnections() {
        createChildPortConnections();
        
        if (!hasInputControlPort() && hasParent()) {
            createVirtualPortandConnection();
        }
    }

    /**
     * Creates its childs port connections.
     */
    void Task::createChildPortConnections() {
        for (unsigned i= 0; i < childVector.size(); i++) {
            Task* child= childVector.at(i);
            child->createPortConnections();
        }
    }

    void Task::createVirtualPortandConnection() {
        assert(!hasInputControlPort());
        assert(hasParent());
        
        Task* parent= getParent();
        
        Port* virtualInput= Port::createVirtualInputPort(this);
        Port* virtualOutput= Port::createVirtualOutputPort(parent);
        virtualInput->setControl(true);
        virtualOutput->setArtificial(true);
        PortConnection::create(virtualOutput, virtualInput);
    }
 
    
    
    /* ****************************************************************
     * * Variable relationship
     * ****************************************************************/
    
    /** 
     * Get the variable with the requested symbol.
     */
    Variable* Task::getVariable(TL::Symbol symbol) {
        
        Variable* result= NULL;
        
        for (unsigned i= 0; i < variableVector.size() && !result; i++) {
            Variable* variable= variableVector.at(i);
            if (variable && variable->hasSymbol(symbol)) {
                result= variable;
            } 
        }
        
        return result;
    }
    
    /** 
     * Method called by Variable.
     */
    void Task::addVariable(Variable* variable)
    {
        assert(variable);
        assert(variable->getTask() == this);
        
        variableVector.push_back(variable);
    }
   
} /* end namespace Acotes */ } /* end namespace TL */

#if 0
void                        
TaskInfo::
compute_graph
		( void
		)
{
	for		( std::list<TaskInfo*>::iterator it= _task_info_children.begin()
			; it != _task_info_children.end()
			; it++)
	{
		TaskInfo* child= *it;
		
		child->compute_graph();
	}
	
	compute_graph_shortcuts();
	compute_graph_inputs();
	compute_graph_outputs();
}

// compute_graph_input ---------------------------------------------------------
void 
TaskInfo::
compute_graph_input
		( const Symbol& input
		)
{
	assert(_task_info_parent);
	
	if		(  !is_shortcut(input) 
			&& !_task_info_parent->is_shortcut(input)
			)
	{
		StreamInfo* stream_info= _taskgroup_info
				->new_stream_info(input, _task_info_parent, this);
				
		// is input for this task
		add_loop_pop(stream_info->get_input_stream_info());
		add_replace_push(stream_info->get_output_stream_info());
	}
}

// compute_graph_inputs --------------------------------------------------------
void 
TaskInfo::
compute_graph_inputs
		( void
		)
{
	for		( std::set<Symbol>::iterator it= _inputs.begin()
			; it != _inputs.end()
			; it++
			)
	{
		Symbol input= *it;
		
		compute_graph_input(input);
	}
}

// compute_graph_output --------------------------------------------------------
void 
TaskInfo::
compute_graph_output
		( const Symbol& output
		)
{
	assert(_task_info_parent);
	
	if		(  !is_shortcut(output) 
			&& !_task_info_parent->is_shortcut(output)
			)
	{
		StreamInfo* stream_info= _taskgroup_info
				->new_stream_info(output, this, _task_info_parent);
				
		// is output for this task
		add_loop_push(stream_info->get_output_stream_info());
		add_replace_pop(stream_info->get_input_stream_info());
	} 
}

// compute_graph_outputs -------------------------------------------------------
void 
TaskInfo::
compute_graph_outputs
		( void
		)
{
	for		( std::set<Symbol>::iterator it= _outputs.begin()
			; it != _outputs.end()
			; it++
			)
	{
		Symbol output= *it;
		
		compute_graph_output(output);
	}
}

// compute_graph_shortcut ------------------------------------------------------
void 
TaskInfo::
compute_graph_shortcut
		( const Symbol& symbol
		)
{
	TaskInfo* output= (TaskInfo*)0;

	output= compute_graph_shortcut_output(symbol, output);	
}

// compute_graph_shortcut_output
TaskInfo* 
TaskInfo::
compute_graph_shortcut_output
		( const Symbol& symbol
		, TaskInfo* output
		)
{
	// if here is also shortcutted
	if (is_shortcut(symbol))
	{
		// for each initial input before output 
		for		( std::list<TaskInfo*>::iterator it= _task_info_children.begin()
				; it != _task_info_children.end()
				; it++
				)
		{
			TaskInfo* child_task_info= *it;
			
			// if symbol is input calls to it as symbol/output
			// if symbol is output overwrites it, 
			// if both, do both
			output= child_task_info->
					compute_graph_shortcut_output(symbol, output); 
		}
	
	// if not shortcutted output
	} else /* if (!is_shortcut(symbol)) */ {
		// if it is input shortcut
		if (is_input(symbol) && output) {
			StreamInfo* stream_info= _taskgroup_info
					->new_stream_info(symbol, output, this);
					
			// connect!!
			// is input for this task
			this->add_loop_pop(stream_info->get_input_stream_info());
			// output for the other task
			output->add_loop_push(stream_info->get_output_stream_info());
		} 
		// if it is output is the next output
		if (is_output(symbol)) {
			output= this;
		}
	}
	
	return output;
}

// compute_graph_shortcut ------------------------------------------------------
void 
TaskInfo::
compute_graph_shortcuts
		( void
		)
{
	for		( std::set<Symbol>::iterator it= _shortcuts.begin()
			; it != _shortcuts.end()
			; it++
			)
	{
		Symbol shortcut= *it;
		
		compute_graph_shortcut(shortcut);
	}
}

#endif
