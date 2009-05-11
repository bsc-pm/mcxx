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
// File:   tl-statetransform.h
// Author: drodenas
//
// Created on 24 / desembre / 2007, 16:19
//

#ifndef _TL_STATETRANSFORM_H
#define	_TL_STATETRANSFORM_H

#include <string>
#include <tl-langconstruct.hpp>

namespace TL { namespace Acotes {

    class State;
    
    class StateTransform {
        
    // -- Constructor
    public:
        StateTransform(const std::string &driver);
    protected:
       const std::string driver;

    // -- Generation
    public:
        virtual Source generateParams_struct(State* state);
        virtual Source generateParamsTask_assign (State* state);
        virtual Source generateParams_assign(State* state);


        virtual Source generateCopy(State* state);
        virtual Source generateCopyStruct(State * state);

        virtual Source generateCopyInAcquire(State* state);
        virtual Source generateCopyOutAcquire(State* state);
    private:
        virtual Source generateCopyIn(State* state);
        virtual Source generateCopyOut(State* state);
        virtual     Source generateCopyinStruct (State * state) ;

    
    };

} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_STATETRANSFORM_H */

