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
// File:   ac-variable.h
// Author: drodenas
//
// Created on 22 / desembre / 2007, 18:06
//

#ifndef _AC_VARIABLE_H
#define	_AC_VARIABLE_H

#include <string.h>
#include <tl-symbol.hpp>

namespace TL { namespace Acotes {
    
    class Task;
    
    class Variable {
    
    // -- Creation support
    public:
        static Variable* create(Task* task, TL::Symbol symbol);
    private:
        Variable();

    // -- Name
    public:
        std::string getName() const { return name; };
    private:
        void setName(const std::string &name);
        std::string name;
        
    // -- Task relationship
    public:
        Task* getTask() const { return task; }
        bool hasTask() const { return task; }
    private:
        void setTask(Task* task);
        Task* task;
        
    // -- Symbol support
    public:
        bool hasSymbol() const { return symbol; }
        bool hasSymbol(TL::Symbol symbol) const;
        TL::Symbol getSymbol() const;
    private:
        void setSymbol(TL::Symbol* symbol);
        TL::Symbol* symbol;
        
    // -- Array support
    public:
        bool isArray() const { return array; }
        TL::Type getElementType() const { return elementType[0]; }
        bool hasElementType() const { return elementType; }
        int getElementCount() const { return elementCount; }
        void setArray(int n);
    private:
        void setElementType(TL::Type* type) { this->elementType= type; }
        void setElementCount(int elementCount) { this->elementCount= elementCount; }
        bool array;
        TL::Type* elementType;
        int elementCount;
    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _AC_VARIABLE_H */

