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
        virtual Source generateAcquire2_task(Port* port);
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

