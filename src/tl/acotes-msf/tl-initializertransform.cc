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
#include "tl-initializertransform.h"

#include <assert.h>
#include <sstream>
#include "ac-initializer.h"
#include "ac-task.h"

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * No Constructor
     * ****************************************************************/
    
    InitializerTransform::InitializerTransform(const std::string& d):driver(d) {
    }

        
    /* ****************************************************************
     * * Transform
     * ****************************************************************/
    
    void InitializerTransform::transform(Initializer* initializer)
    {
        assert(initializer);
        
        transformReplaceConstruct(initializer);
    }
    
    void InitializerTransform::transformReplaceConstruct(Initializer* initializer)
    {
        assert(initializer);
        
        TL::LangConstruct* initializerConstruct= initializer->getConstruct();
        AST_t initializerAST= initializerConstruct->get_ast();
        ScopeLink initializerScopeLink= initializerConstruct->get_scope_link();
    
        // Replace taskgroup construct
        Source replaceSource= generateReplacement(initializer);
        AST_t replaceTree= replaceSource.parse_statement(initializerAST, initializerScopeLink);
        initializerAST.replace(replaceTree);

    }

    Source InitializerTransform::generateReplacement(Initializer* initializer)
    {
        assert(initializer);
     
        Source ss;
       
        ss << initializer->getBody()->prettyprint();
       
        return ss;

    }
    
    
    /* ****************************************************************
     * * Generators
     * ****************************************************************/
    
    Source InitializerTransform::generate(Task* task)
    {
        assert(task);
        
        Source ss;
        
        const std::vector<Initializer*> initializers= Initializer::getMatching(task->getInitializerVector());
        for (unsigned i= 0; i < initializers.size(); i++) {
            Initializer* initializer= initializers.at(i);
            ss << generate(initializer);
        }
        
        return ss;
    }
    
    Source InitializerTransform::generate(Initializer* initializer) {
        assert(initializer);
        
        Source ss;
        
        ss << initializer->getBody()->prettyprint();
        
        return ss;
    }
        

    
} /* end namespace TL */ } /* end namespace Acotes */ 
