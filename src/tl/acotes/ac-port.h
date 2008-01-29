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
// File:   ac-port.h
// Author: drodenas
//
// Created on 22 / desembre / 2007, 12:24
//

#ifndef _AC_PORT_H
#define	_AC_PORT_H

#include <vector>
#include <string>

namespace TL { namespace Acotes {

    class Peek;
    class PortConnection;
    class Task;
    class Variable;
    
    class Port {
    // -- Port creation
    public:
        static Port* createVirtualInputPort(Task* task);
        static Port* createVirtualOutputPort(Task* task);
        static Port* createControlInputPort(Variable* variable);
        static Port* createControlOutputPort(Variable* variable);
        static Port* createArtificialInputPort(Variable* variable);
        static Port* createArtificialOutputPort(Variable* variable);
        static Port* createInputPort(Variable* variable);
        static Port* createOutputPort(Variable* variable);
    private:
        static Port* createPort(Task* task);
        static Port* createPort(Variable* variable);
        Port();
        
    // -- Task relationship
    public:
        Task* getTask() const { return task; }
        bool hasTask() const { return task; }
        int getNumber() const { return number; }
    private:
        void setTask(Task* task);
        Task* task;
        int number;
        
    // -- Port direction
    public:
        bool isInput() const { return input; }
        bool isOutput() const { return output; } 
    private:
        void setInput(bool value) { input= value; }
        void setOutput(bool value) { output= value; }
        bool input; // default false
        bool output; // default false
        
    // -- Port connection relationship
    public:
        void addPortConnection(PortConnection* portConnection);
        PortConnection* getPortConnection();
        bool hasPortConnection() const { return portConnectionVector.size() > 0; };
        Port* getArtificialCounterpart();
        void verifyConnection();
    private:
        std::vector<PortConnection*> portConnectionVector;
        
    // -- Variable relationship
    public:
        Variable* getVariable() const { return variable; } 
        bool hasVariable() const { return variable; }
    private:
        void setVariable(Variable* variable);
        Variable* variable;
        
    // -- Control port
    public:
        bool isControl() const { return control; }
        void setControl(bool control) { this->control= control; }
    private:
        bool control; // default false
        
    // -- Artificial port
    public:
        bool isArtificial() const { return artificial; }
        void setArtificial(bool artificial) { this->artificial= artificial; }
    private:
        bool artificial;
        
    // -- Name
    public:
        void setName(const std::string& name);
        bool isNamed() const { return named; }
        const std::string &getName() const;
    private:
        std::string name;
        bool named;
        
    // -- Peek support
    public:
        bool hasPeek() const { return getPeek(); }
        Peek* getPeek() const { return peek; }
        void setPeek(Peek* peek);
        int getPeekWindow() const;
    private:
        Peek* peek;
        
    // -- Replicate property
    public:
        void setReplicate(bool value) { replicate= value; }
        bool isReplicate() const { return replicate; }
    private:
        bool replicate;
            
    // -- Locus
    public:
        const std::string &getLocus() const { return locus; };
        void setLocus(const std::string& locus) { this->locus= locus; };
    private:
        std::string locus;

    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _AC_PORT_H */

