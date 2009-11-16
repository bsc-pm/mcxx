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
// File:   tl-acotesstack.h
// Author: drodenas
//
// Created on 19 / desembre / 2007, 12:21
//

#ifndef _TL_ACOTESSTACK_H
#define	_TL_ACOTESSTACK_H

#include <stack>
#include <vector>

#include <tl-langconstruct.hpp>

namespace TL { namespace Acotes {

    class Task;
    class Taskgroup;

    class AcotesStack {
    // -- Task stack
    public:
        static Task* taskTop();
        static void taskPush(Task* task);
        static void taskPop();
    private:
        static std::stack<Task*> taskStack;
        
                
    // -- Taskgroup stack
    public:
        static Taskgroup* taskgroupTop();
        static void taskgroupPush(Taskgroup* taskgroup);
        static void taskgroupPop();
    private:
        static std::stack<Taskgroup*> taskgroupStack;
        
    // -- ForReplicate list
    public:
        static void forStatementPush(TL::ForStatement forStatement);
        static void forStatementPop();
        static bool hasForStatement(TL::Symbol symbol);
        static TL::ForStatement getForStatement(TL::Symbol symbol);
    private:
        static std::vector<TL::ForStatement> forStatementVector;
        
    // -- No Constructor
        AcotesStack();
    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_ACOTESSTACK_H */

