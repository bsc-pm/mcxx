/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - David Rodenas Pico
    Copyright (C) 2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
*/
// 
// File:   tl-sharedconstruct.h
// Author: drodenas
//
// Created on 30 / desembre / 2007, 18:27
//

#ifndef _TL_SHAREDCONSTRUCT_H
#define	_TL_SHAREDCONSTRUCT_H

#include <tl-langconstruct.hpp>
#include <tl-pragmasupport.hpp>

namespace TL { namespace Acotes {

    class SharedConstruct
    : TL::PragmaCustomConstruct
    {
    // -- LangConstruct support
    public:
        SharedConstruct(TL::LangConstruct langConstruct);
    private:
        TL::LangConstruct getConstruct();

    // -- CompilerPhase events
    public:
        void onPre();
        void onPost();
    private:
        void onPreCheck();
        void onPreUpdate();
    };
    
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_SHAREDCONSTRUCT_H */

