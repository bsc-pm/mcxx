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
// File:   ac-initializer.h
// Author: drodenas
//
// Created on 26 / desembre / 2007, 11:57
//

#ifndef _AC_INITIALIZER_H
#define	_AC_INITIALIZER_H

#include <vector>
#include <tl-langconstruct.hpp>
#include <tl-symbol.hpp>

namespace TL { namespace Acotes {
    
    class Initializer {
        
    // -- Tribal behaviour
    public:
        static Initializer* create(TL::LangConstruct* construct, TL::LangConstruct* body);
        static std::vector<Initializer*> getMatching(const std::vector<TL::Symbol>& symbols) { return getMatching(instanceVector, symbols); }
        static std::vector<Initializer*> &getInstanceVector() { return instanceVector; }
    private:
        static std::vector<Initializer*> getMatching(const std::vector<Initializer*>& initializers, const std::vector<TL::Symbol>& symbols);
        Initializer();
        static std::vector<Initializer*> instanceVector;
        
        
    // -- Body
    public:
        TL::LangConstruct* getBody() const { return body; }
        TL::LangConstruct* getConstruct() const { return construct; }
    private:
        void setConstruct(TL::LangConstruct* construct);
        void setBody(TL::LangConstruct* body);
        TL::LangConstruct* construct;
        TL::LangConstruct* body;
        
    // -- Variable collection support
    public:
        void addSymbol(TL::Symbol symbol) { symbolVector.push_back(symbol); }
    private:
        bool match(const std::vector<TL::Symbol>& symbols) const;
        static bool match(TL::Symbol symbol, const std::vector<TL::Symbol>& symbols);
        std::vector<TL::Symbol> symbolVector;
    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _AC_INITIALIZER_H */

