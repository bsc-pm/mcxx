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
#include "ac-finalizer.h"

#include <assert.h>

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * Tribal behaviour
     * ****************************************************************/

    std::vector<Finalizer*> Finalizer::instanceVector;
    
    Finalizer* Finalizer::create(TL::LangConstruct* construct,TL::LangConstruct* body)
    {
        assert(body);
        
        Finalizer* finalizer= new Finalizer();
        instanceVector.push_back(finalizer);
        finalizer->setConstruct(construct);
        finalizer->setBody(body);
        
        return finalizer;
    }
    
    std::vector<Finalizer*> Finalizer::getMatching(const std::vector<Finalizer*> &finalizers, const std::vector<TL::Symbol>& symbols) 
    {
        std::vector<Finalizer*> result;
    
        for (unsigned i= 0; i < finalizers.size(); i++) {
            Finalizer* finalizer= finalizers.at(i);
            if (finalizer->match(symbols)) {
                result.push_back(finalizer);
            }
        }
        
        return result;
    }
    
    Finalizer::Finalizer() 
    : construct(NULL), body(NULL)
    {
    }
        
        
    
    /* ****************************************************************
     * * Body
     * ****************************************************************/

    void Finalizer::setConstruct(TL::LangConstruct* construct)
    {
        assert(construct);
        assert(!this->construct /* call only once */);
        
        this->construct= construct;
    }
    void Finalizer::setBody(TL::LangConstruct* body)
    {
        assert(body);
        assert(!this->body /* call only once */);
        
        this->body= body;
    }

    
    
    /* ****************************************************************
     * * Variable collection support
     * ****************************************************************/
    
    bool Finalizer::match(const std::vector<TL::Symbol>& symbols) const 
    {
        bool result= true;
        
        for (unsigned i= 0; i < symbolVector.size() && result; i++) {
            TL::Symbol symbol= symbolVector.at(i);
            result= match(symbol, symbols);
        }
        
        return result;
    }
    
    bool Finalizer::match(TL::Symbol symbol, const std::vector<TL::Symbol>& symbols) 
    {
        bool result= false;
        
        for (unsigned i= 0; i < symbols.size() && !result; i++) {
            TL::Symbol other= symbols.at(i);
            result= symbol == other;
        }
        
        return result;
    }
        
    
} /* end namespace Acotes */ } /* end namespace TL */

