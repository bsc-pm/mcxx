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

