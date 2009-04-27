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
    // -- Constructor
    public:
        TaskTransform(const std::string& driver);
    protected:
        const std::string driver;
        
    // -- Transform
    public:
        virtual void transform(Task* task);
    private:
        virtual void transformChildren(Task* task);
        virtual void transformAddOutline(Task* task);
        virtual void transformReplacePeek(Task* task);
        virtual void transformReplaceVariable(Task* task);
        virtual void transformReplaceUserPort(Task* task);
        virtual void transformReplaceSharedCheck(Task* task);
        virtual void transformReplaceSharedUpdate(Task* task);
        virtual void transformReplaceTeamReplicate(Task* task);
        virtual void transformReplaceConstruct(Task* task);
        
    // -- Outline generation
    private:
        virtual std::string generateOutline(Task* task);
        virtual std::string generateForReplicate(Task* task);
        virtual std::string generateVariable(Task* task);
        virtual std::string generateInitializer(Task* task);
        virtual std::string generateFinalizer(Task* task);
        virtual std::string generateCopyInAcquire(Task* task);
        virtual std::string generateCopyOutAcquire(Task* task);
        virtual std::string generateSharedAcquire(Task* task);
        virtual std::string generateBody(Task* task);
        virtual std::string generateControlAcquire(Task* task);
        virtual std::string generateControlSharedCheck(Task* task);
        virtual std::string generateControlInputPeek(Task* task);
        virtual std::string generateControlOutputPeek(Task* task);
        virtual bool hasInput (Task * task);
        virtual std::string generateControlInputBufferAccess(Task* task);
        virtual std::string generateControlOutputBufferAccess(Task* task);
        virtual std::string generateControlPop(Task* task);
        virtual std::string generateControlPush(Task* task);
        virtual std::string generateReplicatePeek(Task* task);
        virtual std::string generateReplicateBody(Task* task);
        virtual std::string generateReplicatePop(Task* task);
        virtual std::string generateReplicateAcquire(Task* task);
        virtual std::string generateCommitPorts(Task* task);

    // -- Replacement generation
    private:
        virtual std::string generateReplacement(Task* task);
        virtual std::string generateArtificialPush(Task* task);
        virtual std::string generateArtificialPop(Task* task);
        
    // -- Taskgroup replacement generation support
    public:
        virtual std::string generateInit(Task* task);
        virtual std::string generatePorts(Task* task);
        virtual std::string generateBufferPorts(Task* task);
        virtual std::string generateNelemsBufferPorts(Task* task);
        virtual std::string generate_task_allopen_condition(Task* task);
        virtual std::string generateShareds(Task* task);
        virtual std::string generateStart(Task* task);
        virtual std::string generateWait(Task* task);
        
    };
    
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_TASKTRANSFORM_H */

