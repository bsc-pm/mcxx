/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/

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

