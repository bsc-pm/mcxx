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

