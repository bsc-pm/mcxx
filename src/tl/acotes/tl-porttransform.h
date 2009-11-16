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
        virtual std::string generatePort(Port* port);
        virtual std::string generateAcquire(Port* port);
        virtual std::string generateInputPeek(Port* port);
        virtual std::string generateOutputPeek(Port* port);
        virtual std::string generatePop(Port* port);
        virtual std::string generatePush(Port* port);
    private:
        virtual std::string generateInputPort(Port* port);
        virtual std::string generateOutputPort(Port* port);
                
    };
    
} /* end namespace Acotes */ } /* end namespace TL */

#endif	/* _TL_PORTTRANSFORM_H */

