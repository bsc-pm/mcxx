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
// File:   ac-taskgroup.h
// Author: drodenas
//
// Created on 19 / desembre / 2007, 13:33
//

#ifndef _AC_TASKGROUP_H
#define	_AC_TASKGROUP_H

#include <string>
#include <vector>

#include "tl-langconstruct.hpp"

namespace TL { namespace Acotes {
    
    class PortConnection;
    class State;
    class Task;
    
    class Taskgroup {
    // -- Tribal behaviour
    public:
        static Taskgroup* create(TL::LangConstruct* construct, TL::LangConstruct* body);
        static const std::vector<Taskgroup*> &getInstanceVector() { return instanceVector; }
    private:
        Taskgroup(const std::string& name);
        static std::vector<Taskgroup*> instanceVector;

    // -- Name
    private:
        std::string name;
        
    // -- Task relationship
    public:
        Task* getImplicitTask() const { return implicitTask; }
        void addTask(Task* task);
        const std::vector<Task*>& getTaskVector() const { return taskVector; }
    private:
        void createImplicitTask(TL::LangConstruct* construct, TL::LangConstruct* body);
        Task* implicitTask;
        std::vector<Task*> taskVector;

    // -- LangConstruct support
    public:
        TL::LangConstruct* getBody() const;
        TL::LangConstruct* getConstruct() const;
        
    // -- Port Connections support
    public:
        void createPortConnections();
        void addPortConnection(PortConnection* portConnection);
        const std::vector<PortConnection*> &getPortConnectionVector() const { return portConnectionVector; }
    private:
        std::vector<PortConnection*> portConnectionVector;
        
    // -- CopyInOut state relationship
    public:
        void addCopyStateVector(State* state);
        const std::vector<State*> &getCopyStateVector() const { return copyStateVector; }
    private:
        bool checkCopyOutSymbol(TL::Symbol symbol) const;
        std::vector<State*> copyStateVector;
    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _AC_TASKGROUP_H */

