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
        virtual std::string generateControlPop(Task* task);
        virtual std::string generateControlPush(Task* task);
        virtual std::string generateReplicatePeek(Task* task);
        virtual std::string generateReplicateBody(Task* task);
        virtual std::string generateReplicatePop(Task* task);
        virtual std::string generateReplicateAcquire(Task* task);
    
    // -- Replacement generation
    private:
        virtual std::string generateReplacement(Task* task);
        virtual std::string generateArtificialPush(Task* task);
        virtual std::string generateArtificialPop(Task* task);
        
    // -- Taskgroup replacement generation support
    public:
        virtual std::string generateInit(Task* task);
        virtual std::string generatePorts(Task* task);
        virtual std::string generateShareds(Task* task);
        virtual std::string generateStart(Task* task);
        virtual std::string generateWait(Task* task);
        
    };
    
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_TASKTRANSFORM_H */

