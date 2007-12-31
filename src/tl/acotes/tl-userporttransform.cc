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
#include "tl-userporttransform.h"

#include <assert.h>
#include <sstream>
#include "ac-userport.h"
#include "tl-porttransform.h"

namespace TL { namespace Acotes {

    /* ****************************************************************
     * * Transform
     * ****************************************************************/
    
    void UserPortTransform::transform(UserPort* userPort)
    {
        transformReplacement(userPort);
    }
   
    void UserPortTransform::transformReplacement(UserPort* userPort)
    {
        assert(userPort);
        
        TL::LangConstruct* userPortConstruct= userPort->getConstruct();
        AST_t userPortAST= userPortConstruct->get_ast();
        ScopeLink userPortScopeLink= userPortConstruct->get_scope_link();
    
        // Replace taskgroup construct
        Source replaceSource= generateReplacement(userPort);
        AST_t replaceTree= replaceSource.parse_statement(userPortAST, userPortScopeLink);
        userPortAST.replace(replaceTree);
    }
        
    /* ****************************************************************
     * * Transform
     * ****************************************************************/
    
    // -- Generator
    std::string UserPortTransform::generateReplacement(UserPort* userPort)
    {
        assert(userPort);
        
        std::stringstream ss;
        
        ss      << "{"
                <<    generateInputPort(userPort)
                <<    generateBody(userPort)
                <<    generateOutputPort(userPort)
                << "}"
                ;
        
        return ss.str();
    }
    
    std::string UserPortTransform::generateInputPort(UserPort* userPort)
    {
        std::stringstream ss;
        
        const std::vector<Port*> &ports= userPort->getInputPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            ss << PortTransform::generateAcquire(port);
            ss << PortTransform::generateInputPeek(port);
            ss << PortTransform::generatePop(port);
        }
        
        return ss.str();
    }
    
    std::string UserPortTransform::generateBody(UserPort* userPort)
    {
        std::stringstream ss;
        
        ss << userPort->getBody()->prettyprint();
        
        return ss.str();
    }
    
    std::string UserPortTransform::generateOutputPort(UserPort* userPort)
    {
        std::stringstream ss;
        
        const std::vector<Port*> &ports= userPort->getOutputPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
            ss << PortTransform::generateAcquire(port);
            ss << PortTransform::generateOutputPeek(port);
            ss << PortTransform::generatePush(port);
        }
        
        return ss.str();
    }
        
    /* ****************************************************************
     * * No Constructor
     * ****************************************************************/
    
    UserPortTransform::UserPortTransform()
    {
        assert(0);
    }
    
} /* end namespace Acotes */ } /* end namespace TL */
