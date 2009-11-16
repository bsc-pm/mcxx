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
// File:   ac-portconnection.h
// Author: drodenas
//
// Created on 22 / desembre / 2007, 14:43
//

#ifndef _AC_PORTCONNECTION_H
#define	_AC_PORTCONNECTION_H


namespace TL { namespace Acotes {
    
    class Port;
    
    class PortConnection {
        
    // -- PortConnection creation
    public:
        static PortConnection* create(Port* output, Port* input);
    private:
        PortConnection();
        
    // -- Input and Output relationship
    public:
        Port* getInput() const { return input; }
        Port* getOutput() const { return output; }
    private:
        void setInput(Port* input) { this->input= input; }
        void setOutput(Port* output) { this->output= output; }
        Port* input;
        Port* output;
    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _AC_PORTCONNECTION_H */

