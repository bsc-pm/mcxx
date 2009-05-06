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
// File:   tl-porttransform.h
// Author: drodenas
//
// Created on 22 / desembre / 2007, 19:11
//

#ifndef _TL_PORTTRANSFORM_H
#define	_TL_PORTTRANSFORM_H

#include <assert.h>
#include <string>
#include <tl-langconstruct.hpp>

namespace TL { namespace Acotes {
    
    class Port;
    
    class PortTransform {
    
    // -- Constructor    
    public:
        PortTransform(const std::string& driver);
    protected:
        const std::string driver;
        
    // -- Auxiliary generators
    public:
        virtual Source generatePort(Port* port);
        virtual Source generateBufferPort(Port* port);
        virtual Source generateNelemsBufferPort(Port* port);
        virtual Source generatePrevBufferWrite(Port* port);
	virtual Source generateEndOfFile(Port* port);
        virtual Source generateCommitBufferPort(Port* port);
        virtual Source generate_endofstream_condition(Port* port);
        virtual Source generate_continue_condition(Port* port);
        virtual Source generateAcquire_task(Port* port);
        virtual Source generateAcquire_task2(Port* port);
        virtual Source generateAcquire(Port* port);
        virtual Source generateInputPeek(Port* port);
        virtual Source generateOutputPeek(Port* port);
        virtual Source generateOutputBufferAccess(Port* port);
        virtual Source generateInputBufferAccess(Port* port);
        virtual Source generatePop(Port* port);
        virtual Source generatePush(Port* port);
    private:
        virtual Source generateNelemsInputBufferPort(Port* port);
        virtual Source generateNelemsOutputBufferPort(Port* port);
        virtual Source generatePrevOutputBufferPort(Port * port);
        virtual Source generateEndOfFilePort(Port * port);
        virtual Source generateCommitOutputBufferPort(Port* port);
        virtual Source generateCommitInputBufferPort(Port* port);
        virtual Source generate_eos_condition(Port* port);
        virtual Source generate_cont_condition(Port* port);
        virtual Source generateInputPort(Port* port);
        virtual Source generateInputBufferPort(Port* port);
        virtual Source generateOutputPort(Port* port);
        virtual Source generateOutputBufferPort(Port* port);
                
    };
    
} /* end namespace Acotes */ } /* end namespace TL */

#endif	/* _TL_PORTTRANSFORM_H */

