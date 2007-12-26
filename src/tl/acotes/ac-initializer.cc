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
#include "ac-initializer.h"

#include <assert.h>
#include <iostream>

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * Tribal behaviour
     * ****************************************************************/

    std::vector<Initializer*> Initializer::instanceVector;
    
    Initializer* Initializer::create(TL::LangConstruct* construct,TL::LangConstruct* body)
    {
        assert(body);
        
        Initializer* initializer= new Initializer();
        instanceVector.push_back(initializer);
        initializer->setConstruct(construct);
        initializer->setBody(body);
        
        return initializer;
    }
    
    std::vector<Initializer*> Initializer::getMatching(const std::vector<Initializer*> &initializers, const std::vector<TL::Symbol>& symbols) 
    {
        std::vector<Initializer*> result;
    
        for (unsigned i= 0; i < initializers.size(); i++) {
            Initializer* initializer= initializers.at(i);
            if (initializer->match(symbols)) {
                result.push_back(initializer);
            }
        }
        
        return result;
    }
    
    Initializer::Initializer() 
    : construct(NULL), body(NULL)
    {
    }
        
        
    
    /* ****************************************************************
     * * Body
     * ****************************************************************/

    void Initializer::setConstruct(TL::LangConstruct* construct)
    {
        assert(construct);
        assert(!this->construct /* call only once */);
        
        this->construct= construct;
    }
    void Initializer::setBody(TL::LangConstruct* body)
    {
        assert(body);
        assert(!this->body /* call only once */);
        
        this->body= body;
    }

    
    
    /* ****************************************************************
     * * Variable collection support
     * ****************************************************************/
    
    bool Initializer::match(const std::vector<TL::Symbol>& symbols) const 
    {
        bool result= true;
        
        for (unsigned i= 0; i < symbolVector.size() && result; i++) {
            TL::Symbol symbol= symbolVector.at(i);
            result= match(symbol, symbols);
        }
        
        return result;
    }
    
    bool Initializer::match(TL::Symbol symbol, const std::vector<TL::Symbol>& symbols) 
    {
        bool result= false;
        
        for (unsigned i= 0; i < symbols.size() && !result; i++) {
            TL::Symbol other= symbols.at(i);
            result= symbol == other;
        }
        
        return result;
    }
        
    
} /* end namespace Acotes */ } /* end namespace TL */

