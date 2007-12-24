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

#include <stack>

#ifndef _TL_ACOTESSTACK_H
#define	_TL_ACOTESSTACK_H

namespace TL { namespace Acotes {

    class Task;
    class Taskgroup;

    class AcotesStack {
    public:
        static Task* taskTop();
        static void taskPush(Task* task);
        static void taskPop();
        
        static Taskgroup* taskgroupTop();
        static void taskgroupPush(Taskgroup* taskgroup);
        static void taskgroupPop();
        
        
    private:
        static std::stack<Task*> taskStack;
        static std::stack<Taskgroup*> taskgroupStack;
        
        AcotesStack();
    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_ACOTESSTACK_H */

