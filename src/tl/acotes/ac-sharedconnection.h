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
// File:   ac-sharedconnection.h
// Author: drodenas
//
// Created on 31 / desembre / 2007, 11:05
//

#ifndef _AC_SHAREDCONNECTION_H
#define	_AC_SHAREDCONNECTION_H

namespace TL { namespace Acotes {
    
    class State;
    
    class SharedConnection {
    // -- Creation
    public:
        static SharedConnection* create(State* source, State* target);
    private:
        SharedConnection();
        
    // -- State handling
    public:
        State* getSource() const { return source; }
        State* getTarget() const { return target; }
    private:
        void setSource(State* state);
        void setTarget(State* state);
        State* source;
        State* target;
    };
    
} /* end namespace Acotes */ } /* end namespace TL */

#endif	/* _AC_SHAREDCONNECTION_H */

