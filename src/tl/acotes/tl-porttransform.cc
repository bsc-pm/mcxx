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

#include "ac-peek.h"
#include "ac-port.h"
#include "ac-state.h"
#include "ac-task.h"
#include "ac-variable.h"
#include "tl-variabletransform.h"

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
            if (port->isInput()) 
            { 
                ss << "iport_"; 
            }
            else if (port->isOutput()) 
            { 
                ss << "oport_"; 
            }
            else { assert(0); }
            
            ss  << "acquire"
                << "(" << port->getNumber() 
                << ", " << VariableTransform::generateElementCount(variable)
                << ");";
        }
        
        return ss.str();
    }
    
    std::string PortTransform::generateInputPeek(Port* port)
    {
        assert(port);
        assert(port->isInput());

        std::stringstream ss;
        
        Variable* variable= port->getVariable();
        if (variable) {
            ss  << "{"
                << "  int acotescc__for_peek_index;"
                << "  for (acotescc__for_peek_index= 0"
                << "      ; acotescc__for_peek_index < " << VariableTransform::generateElementCount(variable)
                << "      ; acotescc__for_peek_index++"
                << "      ) {"
                // for each element copies the value
                << "      memcpy"
                << "          ( &((" << VariableTransform::generateReference(variable) << ")[acotescc__for_peek_index])"
                << "          , iport_peek"
                << "               (" << port->getNumber() 
                // if we have peek we need to read from the beggining
                << "               , " << port->getPeekWindow() << " + acotescc__for_peek_index"
                << "               )"
                // peeks the size of one element
                << "          , " << VariableTransform::generateSizeof(variable)
                << "      );"
                << "  }"
                << "}";
        }
        
        return ss.str();
    }
    
    std::string PortTransform::generateOutputPeek(Port* port)
    {
        assert(port);
        assert(port->isOutput());

        std::stringstream ss;
        
        Variable* variable= port->getVariable();
        if (variable) {
            ss  << "{"
                << "  int acotescc__for_peek_index;"
                << "  for (acotescc__for_peek_index= 0"
                << "      ; acotescc__for_peek_index < " << VariableTransform::generateElementCount(variable)
                << "      ; acotescc__for_peek_index++"
                << "      ) {"
                // for each element copies the value
                << "      memcpy"
                << "          ( oport_peek"
                << "               (" << port->getNumber() 
                << "               , acotescc__for_peek_index"
                << "               )"
                << "          , &((" << VariableTransform::generateReference(variable) << ")[acotescc__for_peek_index])"
                // peeks the size of one element
                << "          , " << VariableTransform::generateSizeof(variable)
                << "      );"
                << "  }"
                << "}";
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
            if (port->isInput()) 
            { 
                ss << "iport_"; 
            }
            else if (port->isOutput()) 
            { 
                ss << "oport_"; 
            }
            else { assert(0); }
            ss  << "pop"
                << "(" << port->getNumber() 
                << ", " << VariableTransform::generateElementCount(variable)
                << ");";
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
            if (port->isInput()) 
            { 
                ss << "iport_"; 
            }
            else if (port->isOutput()) 
            { 
                ss << "oport_"; 
            }
            else { assert(0); }
            ss  << "push"
                << "(" << port->getNumber() 
                << ", " << VariableTransform::generateElementCount(variable)
                << ");";
        }
        
        return ss.str();
    }

    
    std::string PortTransform::generateInputPort(Port* port) {
        assert(port);
        assert(port->isInput());
        
        std::stringstream ss;
        
        Variable* variable= port->getVariable();
        ss << "task_iport"
                << "( " << port->getTask()->getName()
                << ", " << port->getNumber()
                ;
        if (port->hasVariable()) {
            ss  << ", " << VariableTransform::generateSizeof(variable)
                << ", " << port->getPeekWindow() << "+" << VariableTransform::generateElementCount(variable)
                ;
        } else {
            ss  << ", 0, 0";
        }
        
        if (port->hasPeek()) {
            Peek* peek= port->getPeek();
            ss  << ", " << VariableTransform::generateReference(peek->getHistory()->getVariable())
                << ", " << port->getPeekWindow()
                ;
        } else {
            ss  << ", (void*) 0, 0";
        }
        ss      << ");";
        
        if (port->isReplicate()) {
            ss << "iport_replicate"
                    << "( " << port->getTask()->getName()
                    << ", " << port->getNumber()
                    << ");";
        }
        
        return ss.str();
    }
    
    std::string PortTransform::generateOutputPort(Port* port) {
        assert(port);
        assert(port->isOutput());
        
        std::stringstream ss;
        
        Variable* variable= port->getVariable();
        ss << "task_oport"
                << "( " << port->getTask()->getName()
                << ", " << port->getNumber()
                ;
        if (variable) {
            ss  << ", " << VariableTransform::generateSizeof(variable)
                << ", " << VariableTransform::generateElementCount(variable)
                ;
        } else {
            ss  << ", 0, 0";
        }

        ss      << ");";
        
        return ss.str();
    }
    
    
    
    /* ******************************************************
     * * No constructor
     * ******************************************************/
    
    PortTransform::PortTransform() {
        assert(0);
    }
    
} /* end namespace Acotes */ } /* end namespace TL */
    
    
