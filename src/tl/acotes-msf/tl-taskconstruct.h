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
// File:   tl-taskconstruct.h
// Author: drodenas
//
// Created on 19 / desembre / 2007, 16:01
//

#ifndef _TL_TASKCONSTRUCT_H
#define	_TL_TASKCONSTRUCT_H

#include <tl-langconstruct.hpp>
#include <tl-pragmasupport.hpp>

namespace TL { namespace Acotes {
    
    class Task;
    
    class TaskConstruct
    : public TL::PragmaCustomConstruct
    {
    // -- LangConstruct support
    public:
        TaskConstruct(TL::LangConstruct langConstruct, DTO& dto);
    private:
        TL::LangConstruct getBody();
        TL::LangConstruct getConstruct();

        DTO &_dto;

    // -- CompilerPhase events
    public:
        void onPre();
        void onPost();
    private:
        void onPreTeam(Task* task);
        void onPreState(Task* task);
        void onPreCopyInState(Task* task);
        void onPreCopyOutState(Task* task);
        void onPreInitializeState(Task* task);
        void onPreFinalizeState(Task* task);
        void onPreInputPort(Task* task);
        void onPreInputReplicatePort(Task* task);
        void onPreOutputPort(Task* task);
        void onPreBypass(Task* task);
        void onPreAsync(Task* task);
        void onPreSync(Task* task);
        void onForReplicate(Task* task);
        void onPreDevice(Task * task);

    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_TASKCONSTRUCT_H */

