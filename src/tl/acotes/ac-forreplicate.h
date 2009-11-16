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
// File:   ac-forreplicate.h
// Author: drodenas
//
// Created on 3 / gener / 2008, 15:08
//

#ifndef _AC_FORREPLICATE_H
#define	_AC_FORREPLICATE_H


#include <tl-langconstruct.hpp>


namespace TL { namespace Acotes {

    class Task;
    
    class ForReplicate {
        
    // -- Creation
    public:
        static ForReplicate* create(TL::ForStatement* forStatement, Task* task);
    private:
        ForReplicate();
        
    // -- Task relationship
    public:
        Task* getTask() const { return task; }
    private:
        void setTask(Task* task);
        Task* task;
        
    // -- ForStatement handling
    public:
        TL::ForStatement getForStatement() const { return forStatement[0]; }
    private:
        void setForStatement(TL::ForStatement* forStatement);
        TL::ForStatement* forStatement;
    };
    
    
} /* end namespace TL */ } /* end namespace Acotes */


#endif	/* _AC_FORREPLICATE_H */

