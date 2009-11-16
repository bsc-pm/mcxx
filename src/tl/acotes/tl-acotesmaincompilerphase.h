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
// File:   tl-acotesmaincompilerphase.h
// Author: drodenas
//
// Created on 16 / gener / 2008, 16:47
//

#ifndef _TL_ACOTESMAINCOMPILERPHASE_H
#define	_TL_ACOTESMAINCOMPILERPHASE_H


#include <tl-pragmasupport.hpp>

namespace TL { namespace Acotes {
    
    class AcotesMainCompilerPhase
    : public TL::CompilerPhase
    , private TL::TraverseFunctor
    {
    // -- CompilerPhase management
    public:
        AcotesMainCompilerPhase();
        virtual ~AcotesMainCompilerPhase();
       	virtual void pre_run(DTO& data_flow);
       	virtual void run(DTO& data_flow);

    // -- TraverseFunction management
    private:
        virtual void preorder(Context ctx, AST_t node);
        virtual void postorder(Context ctx, AST_t node);

    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_ACOTESMAINCOMPILERPHASE_H */

