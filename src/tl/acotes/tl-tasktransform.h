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
// File:   tl-tasktransform.h
// Author: drodenas
//
// Created on 20 / desembre / 2007, 11:49
//

#ifndef _TL_TASKTRANSFORM_H
#define	_TL_TASKTRANSFORM_H

#include <string>

namespace TL { namespace Acotes {
    
    class Task;
    
    class TaskTransform {
    // -- Transform
    public:
        static void transform(Task* task);
    private:
        static void transformChildren(Task* task);
        static void transformAddOutline(Task* task);
        static void transformReplaceConstruct(Task* task);
        
    // -- Outline generation
    private:
        static std::string generateOutline(Task* task);
        static std::string generateVariable(Task* task);
        static std::string generateCopyInAcquire(Task* task);
        static std::string generateCopyOutAcquire(Task* task);
        static std::string generateBody(Task* task);
        static std::string generateControlAcquire(Task* task);
        static std::string generateControlPeek(Task* task);
        static std::string generateControlPop(Task* task);
        static std::string generateControlPush(Task* task);
    
    // -- Replacement generation
    private:
        static std::string generateReplacement(Task* task);
        static std::string generateArtificialPush(Task* task);
        static std::string generateArtificialPop(Task* task);
        
    // -- Taskgroup replacement generation support
    public:
        static std::string generateInit(Task* task);
        static std::string generatePorts(Task* task);
        static std::string generateStart(Task* task);
        static std::string generateWait(Task* task);
        
    // -- No Constructor
    private:
        TaskTransform();
    };
    
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_TASKTRANSFORM_H */

