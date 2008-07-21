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
#include "tl-finalizertransform.h"

#include <assert.h>
#include <sstream>
#include "ac-finalizer.h"
#include "ac-task.h"

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * Constructor
     * ****************************************************************/
    
    FinalizerTransform::FinalizerTransform(const std::string& d) : driver(d)
    {
    }
    
    
        
    /* ****************************************************************
     * * Transform
     * ****************************************************************/
    
    void FinalizerTransform::transform(Finalizer* finalizer)
    {
        assert(finalizer);
        
        transformReplaceConstruct(finalizer);
    }
    
    void FinalizerTransform::transformReplaceConstruct(Finalizer* finalizer)
    {
        assert(finalizer);
        
        TL::LangConstruct* finalizerConstruct= finalizer->getConstruct();
        AST_t finalizerAST= finalizerConstruct->get_ast();
        ScopeLink finalizerScopeLink= finalizerConstruct->get_scope_link();
    
        // Replace taskgroup construct
        Source replaceSource= generateReplacement(finalizer);
        AST_t replaceTree= replaceSource.parse_statement(finalizerAST, finalizerScopeLink);
        finalizerAST.replace(replaceTree);

    }

    std::string FinalizerTransform::generateReplacement(Finalizer* finalizer)
    {
        assert(finalizer);
     
        std::stringstream ss;
       
        ss << finalizer->getBody()->prettyprint();
       
        return ss.str();

    }
    
    
    /* ****************************************************************
     * * Generators
     * ****************************************************************/
    
    std::string FinalizerTransform::generate(Task* task)
    {
        assert(task);
        
        std::stringstream ss;
        
        const std::vector<Finalizer*> finalizers= Finalizer::getMatching(task->getFinalizerVector());
        for (unsigned i= 0; i < finalizers.size(); i++) {
            Finalizer* finalizer= finalizers.at(i);
            ss << generate(finalizer);
        }
        
        return ss.str();
    }
    
    std::string FinalizerTransform::generate(Finalizer* finalizer) {
        assert(finalizer);
        
        std::stringstream ss;
        
        ss << finalizer->getBody()->prettyprint();
        
        return ss.str();
    }
        
    
} /* end namespace TL */ } /* end namespace Acotes */ 
