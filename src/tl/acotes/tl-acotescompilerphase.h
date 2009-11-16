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
// File:   tl-acotescompilerphase.h
// Author: drodenas
//
// Created on 18 / desembre / 2007, 19:39
//

#ifndef _TL_ACOTESCOMPILERPHASE_H
#define	_TL_ACOTESCOMPILERPHASE_H

#include <tl-pragmasupport.hpp>
#include "acotes-outputtasks.hpp"

namespace TL { namespace Acotes {
    
    class AcotesCompilerPhase
    : public TL::PragmaCustomCompilerPhase
    , private TL::TraverseFunctor
    {
    // -- CompilerPhase management
    public:
        AcotesCompilerPhase();
        virtual ~AcotesCompilerPhase();
       	virtual void pre_run(DTO& data_flow);
       	virtual void run(DTO& data_flow);
    private:
        PragmaCustomDispatcher pragmaDispatcher;
        DTO *_dto;
        
    // -- CompilerPhase events        
    private:
        void onPreTaskConstruct(PragmaCustomConstruct construct);
        void onPostTaskConstruct(PragmaCustomConstruct construct);
        void onPreTaskgroupConstruct(PragmaCustomConstruct construct);
        void onPostTaskgroupConstruct(PragmaCustomConstruct construct);
        void onPreInitializerConstruct(PragmaCustomConstruct construct);
        void onPostInitializerConstruct(PragmaCustomConstruct construct);
        void onPreFinalizerConstruct(PragmaCustomConstruct construct);
        void onPostFinalizerConstruct(PragmaCustomConstruct construct);
        void onPreUserPortConstruct(PragmaCustomConstruct construct);
        void onPostUserPortConstruct(PragmaCustomConstruct construct);
        void onPreSharedConstruct(PragmaCustomConstruct construct);
        void onPostSharedConstruct(PragmaCustomConstruct construct);
        void onPrePeekConstruct(PragmaCustomConstruct construct);
        void onPostPeekConstruct(PragmaCustomConstruct construct);
        void onPreTeamReplicateConstruct(PragmaCustomConstruct construct);
        void onPostTeamReplicateConstruct(PragmaCustomConstruct construct);

    // -- TraverseFunction management
    private:
        virtual void preorder(Context ctx, AST_t node);
        virtual void postorder(Context ctx, AST_t node);

        RefPtr<OutputTasks> _output_tasks;
    };
    
} /* end namespace Acotes */ } /* end namespace TL */

#endif	/* _TL_ACOTESCOMPILERPHASE_H */

