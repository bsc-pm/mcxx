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
// File:   tl-acotescompilerphase.h
// Author: drodenas
//
// Created on 18 / desembre / 2007, 19:39
//

#ifndef _TL_ACOTESCOMPILERPHASE_H
#define	_TL_ACOTESCOMPILERPHASE_H

#include <tl-pragmasupport.hpp>


namespace TL { namespace Acotes {
    
    class AcotesCompilerPhase
    : public TL::PragmaCustomCompilerPhase
    , private TL::TraverseFunctor
    {
    // -- CompilerPhase management
    public:
        AcotesCompilerPhase();
        virtual ~AcotesCompilerPhase();
       	virtual void run(DTO& data_flow);
    private:
        PragmaCustomDispatcher pragmaDispatcher;
        
    // -- CompilerPhase events        
    private:
        void onPreTaskConstruct(PragmaCustomConstruct construct);
        void onPostTaskConstruct(PragmaCustomConstruct construct);
        void onPreTaskgroupConstruct(PragmaCustomConstruct construct);
        void onPostTaskgroupConstruct(PragmaCustomConstruct construct);

    // -- TraverseFunction management
    private:
        virtual void preorder(Context ctx, AST_t node);
        virtual void postorder(Context ctx, AST_t node);
    };
    
} /* end namespace Acotes */ } /* end namespace TL */

#endif	/* _TL_ACOTESCOMPILERPHASE_H */

