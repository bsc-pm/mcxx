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
// File:   ac-userport.h
// Author: drodenas
//
// Created on 29 / desembre / 2007, 11:33
//

#ifndef _AC_USERPORT_H
#define	_AC_USERPORT_H


#include <vector>


#include <tl-langconstruct.hpp>


namespace TL { namespace Acotes {

    class Task;
    class Port;
    
    class UserPort {
    // -- UserPort creation
    public:
        static UserPort* create(TL::LangConstruct* construct, Task* task);
    private:
        UserPort();
        
    // -- LangConstruct support
    public:
        TL::LangConstruct* getConstruct() const { return construct; }
    private:
        void setConstruct(TL::LangConstruct* construct);
        TL::LangConstruct* construct;
        
    // -- Task relationship
    public:
        Task* getTask() const { return task; }
    private:
        void setTask(Task* task);
        Task* task;
        
    // -- Ports relationships
    public:
        void addInputPort(Port* inport);
        void addOutputPort(Port* outport);
        const std::vector<Port*> &getInputPortVector() const { return inputPortVector; }
        const std::vector<Port*> &getOutputPortVector() const { return outputPortVector; }
    private:
        std::vector<Port*> inputPortVector;
        std::vector<Port*> outputPortVector;
    };
    
} /* end namespace Acotes */ } /* end namespace TL */




#endif	/* _AC_USERPORT_H */

