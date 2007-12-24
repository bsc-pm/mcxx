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
// File:   ac-state.h
// Author: drodenas
//
// Created on 24 / desembre / 2007, 13:31
//

#ifndef _AC_STATE_H
#define	_AC_STATE_H

namespace TL { namespace Acotes {
    
    class Task;
    class Variable;
    
    class State {
        
    // -- Creator
    public:
        static State* create(Variable* variable);
        static State* createCopyIn(Variable* variable);
        static State* createCopyOut(Variable* variable);
    private:
        State();
        
        
    // -- Task relationship
    public:
        Task* getTask() const { return task; }
        bool hasTask() const { return task; }
        int getNumber() const { return number; }
    private:
        void setTask(Task* task);
        Task* task;
        int number;
        
        
    // -- Variable relataionship
    public:
        Variable* getVariable() const { return variable; } 
    private:
        void setVariable(Variable* variable);
        Variable* variable;
        
    // -- CopyIn or CopyOut definition    
    public:
        bool isCopyIn() const { return copyIn; }
        bool isCopyOut() const { return copyOut; }
    private:
        void setCopyIn(bool value);
        void setCopyOut(bool value);
        bool copyIn;
        bool copyOut;
    };
    
} /* end namespace Acotes */ } /* end namespace TL */



#endif	/* _AC_STATE_H */

