/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/

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
        std::string getOrigName() const { return origname; };
    private:
        void setName(const std::string &name);
        void setOrigName(const std::string &name);
        std::string name;
        std::string origname;
        
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

