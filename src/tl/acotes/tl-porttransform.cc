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
#include "tl-porttransform.h"


#include <assert.h>
#include <sstream>

#include "ac-port.h"
#include "ac-task.h"
#include "ac-variable.h"

namespace TL { namespace Acotes {
    

    /* ******************************************************
     * * Auxiliary generators
     * ******************************************************/
    
    std::string PortTransform::generatePort(Port* port)
    {
        assert(port);
        
        std::stringstream ss;

        if (port->isInput()) {
            ss << generateInputPort(port);
        } else if (port->isOutput()) {
            ss << generateOutputPort(port);
        } else {
            assert(0);
        }
        
        return ss.str();
    }
    
    std::string PortTransform::generateAcquire(Port* port)
    {
        assert(port);
        
        std::stringstream ss;
        
        Variable* variable= port->getVariable();
        if (variable || port->isInput()) {
            if (port->isInput()) { ss << 'i'; }
            else if (port->isOutput()) { ss << 'o'; }
            else { assert(0); }
            ss << "port_acquire(" << port->getNumber() << ", 1);";
        }
        
        return ss.str();
    }
    
    std::string PortTransform::generatePeek(Port* port)
    {
        assert(port);
        
        std::stringstream ss;
        
        Variable* variable= port->getVariable();
        if (variable) {
            ss  << variable->getName() << "= ";
            if (port->isInput()) { ss << 'i'; }
            else if (port->isOutput()) { ss << 'o'; }
            else { assert(0); }
            ss  << "port_peek(" << port->getNumber() << ", 0);";
        }
        
        return ss.str();
    }
    
    std::string PortTransform::generatePop(Port* port)
    {
        assert(port);
        assert(port->isInput());
        
        std::stringstream ss;
        
        Variable* variable= port->getVariable();
        if (variable) {
            if (port->isInput()) { ss << 'i'; }
            else if (port->isOutput()) { ss << 'o'; }
            else { assert(0); }
            ss << "port_pop(" << port->getNumber() << ", 1);";
        }
        
        return ss.str();
    }
    
    std::string PortTransform::generatePush(Port* port)
    {
        assert(port);
        assert(port->isOutput());
        
        std::stringstream ss;
        
        Variable* variable= port->getVariable();
        if (variable || port->isOutput()) {
            if (port->isInput()) { ss << 'i'; }
            else if (port->isOutput()) { ss << 'o'; }
            else { assert(0); }
            ss << "port_push(" << port->getNumber() << ", 1);";
        }
        
        return ss.str();
    }

    
    std::string PortTransform::generateInputPort(Port* port) {
        assert(port);
        assert(port->isInput());
        
        std::stringstream ss;
        
        if (!port->hasVariable()) {
            ss << generateVirtualInputPort(port);
        } else {
            assert(0);
        }
        
        return ss.str();
    }
    
    std::string PortTransform::generateVirtualInputPort(Port* port) {
        assert(port);
        assert(port->isInput());
        assert(!port->hasVariable());
        
        std::stringstream ss;
        
        ss << "task_iport"
                << "( " << port->getTask()->getName()
                << ", " << port->getNumber()
                << ", 0, 0, (void*) 0, 0"
                << ");";
        
        return ss.str();
    }
    
    std::string PortTransform::generateOutputPort(Port* port) {
        assert(port);
        assert(port->isOutput());
        
        std::stringstream ss;
        
        if (!port->hasVariable()) {
            ss << generateVirtualOutputPort(port);
        } else {
            assert(0);
        }
        
        return ss.str();
    }
    
    std::string PortTransform::generateVirtualOutputPort(Port* port) {
        assert(port);
        assert(port->isOutput());
        assert(!port->hasVariable());
        
        std::stringstream ss;
        
        ss << "task_oport"
                << "( " << port->getTask()->getName()
                << ", " << port->getNumber()
                << ", 0, 0"
                << ");";
        
        return ss.str();
    }

    
    
    /* ******************************************************
     * * No constructor
     * ******************************************************/
    
    PortTransform::PortTransform() {
        assert(0);
    }
    
} /* end namespace Acotes */ } /* end namespace TL */
    
    
