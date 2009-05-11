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
#include "tl-transform.h"
#include "tl-acoteslogger.h"


namespace TL { namespace Acotes {

    /* ****************************************************************
     * * Constructor
     * ****************************************************************/
    
    UserPortTransform::UserPortTransform(const std::string &d)
            : driver(d)
    {
    }

    /* ****************************************************************
     * * Transform
     * ****************************************************************/
    
    void UserPortTransform::transform(UserPort* userPort)
    {
        transformReplacement(userPort, 0);
    }
    void UserPortTransform::transform(UserPort* userPort, int last)
    {
        transformReplacement(userPort, last);
    }
    void UserPortTransform::transform2(UserPort* userPort)
    {
        transformReplacement2(userPort);
    }
   
    void UserPortTransform::transformReplacement(UserPort* userPort, int last = 0)
    {
        assert(userPort);
        
        TL::LangConstruct* userPortConstruct= userPort->getConstruct();
        AST_t userPortAST= userPortConstruct->get_ast();
        ScopeLink userPortScopeLink= userPortConstruct->get_scope_link();
    
        // Replace taskgroup construct
        fflush(0L);
        printf ("generateReplacement -> Source\n");
        Source replaceSource= generateReplacement(userPort, last);
        printf ("replaceSource.parse_statement -> AST\n");
        AST_t replaceTree= replaceSource.parse_statement(userPortAST, userPortScopeLink, TL::Source::DO_NOT_CHECK_EXPRESSION);
        std::cerr << "replacing " << replaceTree.prettyprint() << std::endl;
        std::cerr << " to " << userPortAST.prettyprint() << std::endl;
        std::cerr << "userPortAST.replace" << std::endl;
        //printf ("userPortAST.replace\n");
        userPortAST.replace(replaceTree);
    }
        
    void UserPortTransform::transformReplacement2(UserPort* userPort)
    {
        assert(userPort);
        
        TL::LangConstruct* userPortConstruct= userPort->getConstruct();
        AST_t userPortAST= userPortConstruct->get_ast();
        ScopeLink userPortScopeLink= userPortConstruct->get_scope_link();
    
        // Replace taskgroup construct
        fflush(0L);
        printf ("generateReplacement -> Source\n");
        Source replaceSource= generateReplacement2(userPort);
        printf ("replaceSource.parse_statement -> AST\n");
        AST_t replaceTree= replaceSource.parse_statement(userPortAST, userPortScopeLink, TL::Source::DO_NOT_CHECK_EXPRESSION);
        printf ("userPortAST.replace\n");
        userPortAST.replace(replaceTree);
    }
        
    /* ****************************************************************
     * * Transform
     * ****************************************************************/
    
    // -- Generator
    Source UserPortTransform::generateReplacement(UserPort* userPort, int last = 0)
    {
        assert(userPort);
        
        Source ss;
        
        ss      << "{"
        //        <<    generateInputPort(userPort)
        //        <<    generateOutputPort(userPort)
                //<<  generateInputPortAccess(userPort) // possibly needed
                <<  generateOutputPortAccess(userPort);
        printf ("UserPortTransform::generateReplacement last %d\n", last);
        if (last) ss << "if (__endofoutput == 1) break;";
        ss      << "}"
                ;
        
        return ss;
    }

    Source UserPortTransform::generateReplacement2(UserPort* userPort)
    {
        assert(userPort);
        
        Source ss;
        
        ss      << "{"
        //        <<    generateInputPort(userPort)
        //        <<    generateOutputPort(userPort)
                 // xavim generateInputPortAccess(userPort) possibly needed
                <<  generateOutputPortAccess2(userPort)
                << "}"
                ;
        
        return ss;
    }
    
    Source UserPortTransform::generateInputPort(UserPort* userPort)
    {
        Source ss;
        
        const std::vector<Port*> &ports= userPort->getInputPortVector();
        printf ("UserPortTransform::generateInputPort\n");
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
        //printf ("UserPortTransform::generateInputPort %s\n",
        //          port->getTask()->getName().c_str());
            ss << Transform::I(driver)->port()->generateAcquire(port);
            ss << Transform::I(driver)->port()->generateInputPeek(port);
            ss << Transform::I(driver)->port()->generatePop(port);
        }
        
        return ss;
    }
    
    Source UserPortTransform::generateOutputPort(UserPort* userPort)
    {
        Source ss;
        
        const std::vector<Port*> &ports= userPort->getOutputPortVector();
        printf ("UserPortTransform::generateOutputPort\n");
        for (unsigned i= 0; i < ports.size(); i++) {
            Port* port= ports.at(i);
        //printf ("UserPortTransform::generateInputPort %s\n",
                  //port->getTask()->getName().c_str());
            ss << Transform::I(driver)->port()->generateAcquire(port);
            ss << Transform::I(driver)->port()->generateOutputPeek(port);
            ss << Transform::I(driver)->port()->generatePush(port);
        }
        
        return ss;
    }
        //xavim generateInputPortAccess is possibly needed
    //Source UserPortTransform::generateInputPortAccess(UserPort* userPort)
    //{
    //    Source ss;
    //    const std::vector<Port*> &ports= userPort->getInputPortVector();
    //    for (unsigned i= 0; i < ports.size(); i++) {
    //       Port* port= ports.at(i);
           //AQUIss << Transform::I(driver)->port()->generateAcquire_itask(port);
    //    }
    //    return ss;
    //}
    Source UserPortTransform::generateOutputPortAccess(UserPort* userPort)
    {
        Source ss;
        const std::vector<Port*> &ports= userPort->getOutputPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
           Port* port= ports.at(i);
           ss << Transform::I(driver)->port()->generateAcquire_task(port);
        }
        //ss << "if (__endofoutput == 1) break;";
        return ss;
    }
    Source UserPortTransform::generateOutputPortAccess2(UserPort* userPort)
    {
        Source ss;
        const std::vector<Port*> &ports= userPort->getOutputPortVector();
        for (unsigned i= 0; i < ports.size(); i++) {
           Port* port= ports.at(i);
           ss << Transform::I(driver)->port()->generateAcquire_task2(port);
        }
        return ss;
    }

} /* end namespace Acotes */ } /* end namespace TL */
