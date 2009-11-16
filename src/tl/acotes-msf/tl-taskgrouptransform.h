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
// File:   tl-taskgrouptransform.h
// Author: drodenas
//
// Created on 19 / desembre / 2007, 19:34
//

#ifndef _TL_TASKGROUPTRANSFORM_H
#define	_TL_TASKGROUPTRANSFORM_H

#include <string>
#include <tl-langconstruct.hpp>

namespace TL { namespace Acotes {
    
    class Taskgroup;
    
    class TaskgroupTransform {
    // -- Constructor
    public:
        TaskgroupTransform(const std::string& driver);
    protected:
        const std::string driver;

        // -- Transform
    public:
        virtual void transform(Taskgroup* taskgroup);
    private:
        virtual void transformReplaceConstruct(Taskgroup* taskgroup);
        
    // -- Replacement generation
    private:
        virtual Source generateReplacement(Taskgroup* taskgroup);
        virtual Source generateBufferSizeInit(Taskgroup* taskgroup);
        virtual Source generateBufferInfo(Taskgroup* taskgroup);
        virtual Source generateTasksInit(Taskgroup* taskgroup);
        virtual Source generateTasksPorts(Taskgroup* taskgroup);
        virtual Source generateTasksShareds(Taskgroup* taskgroup);
        virtual Source generatePortConnections(Taskgroup* taskgroup);
        virtual Source generateConnectionInfo(Taskgroup* taskgroup);
        virtual Source generateCopyState(Taskgroup* taskgroup);
        virtual Source generateSharedConnections(Taskgroup* taskgroup);
        virtual Source generateTasksStart(Taskgroup* taskgroup);
        virtual Source generateTasksWait(Taskgroup* taskgroup);
        virtual Source generateBody(Taskgroup* taskgroup);

    };
    
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_TASKGROUPTRANSFORM_H */

