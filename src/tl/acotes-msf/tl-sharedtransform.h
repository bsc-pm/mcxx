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
// File:   tl-sharedtransform.h
// Author: drodenas
//
// Created on 30 / desembre / 2007, 13:22
//

#ifndef _TL_SHAREDTRANSFORM_H
#define	_TL_SHAREDTRANSFORM_H

#include <string>
#include <tl-langconstruct.hpp>

namespace TL { namespace Acotes {

    class SharedCheck;
    class SharedConnection;
    class SharedUpdate;
    class State;
    
    class SharedTransform {
        
    // -- Constructor
    public:
        SharedTransform(const std::string& driver);
    private:
        const std::string& driver;
        
        
    // -- Transform
    public:
        virtual void transform(SharedCheck* sharedCheck);
        virtual void transform(SharedUpdate* sharedUpdate);
    private:
        virtual void transformReplacement(SharedCheck* sharedCheck);
        virtual Source generateReplacement(SharedCheck* sharedCheck);
        virtual Source generateCheck(SharedCheck* sharedCheck);
        virtual void transformReplacement(SharedUpdate* sharedUpdate);
        virtual Source generateReplacement(SharedUpdate* sharedUpdate);
        virtual Source generateUpdate(SharedUpdate* sharedUpdate);
        
        
        
    // -- Generator
    public:
        virtual Source generateShared(State* state);
        virtual Source generateSharedConnection(SharedConnection* sharedConnection);
        virtual Source generateAcquire(State* state);
        virtual Source generateCheck(State* state);
        virtual Source generateUpdate(State* state);
    private:
    };

} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_SHAREDTRANSFORM_H */

