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
// File:   tl-transform.h
// Author: drodenas
//
// Created on 17 / juliol / 2008, 18:00
//

#ifndef _TL_TRANSFORM_H
#define	_TL_TRANSFORM_H

#include <string>
#include <map>

namespace TL { namespace Acotes {
    
    class AcotesTransform;
    class FinalizerTransform;
    class ForReplicateTransform;
    class InitializerTransform;
    class PeekTransform;
    class TaskTransform;
    class TaskgroupTransform;
    class PortTransform;
    class PortConnectionTransform;
    class SharedTransform;
    class StateTransform;
    class TeamReplicateTransform;
    class UserPortTransform;
    class VariableTransform;
    
    class Transform {
        
    // -- Tribal behaviour
    public:
        static Transform* I(const std::string& name);
    private:
        static std::map<std::string,Transform*> instanceMap;
     
    // -- Constructor
    protected:
        Transform(const std::string& dirver);
        const std::string driver;
        
        
    // -- Transform behaviour
    public:
        virtual AcotesTransform*         acotes();
        virtual FinalizerTransform*      finalizer();
        virtual ForReplicateTransform*   forReplicate();
        virtual InitializerTransform*    initializer();
        virtual PeekTransform*           peek();
        virtual PortTransform*           port();
        virtual PortConnectionTransform* portConnection();
        virtual SharedTransform*         shared();
        virtual StateTransform*          state();
        virtual TaskTransform*           task();
        virtual TaskgroupTransform*      taskgroup();
        virtual TeamReplicateTransform*  teamReplicate();
        virtual UserPortTransform*       userPort();
        virtual VariableTransform*       variable();
    private:
        AcotesTransform*         _acotes;
        FinalizerTransform*      _finalizer;
        ForReplicateTransform*   _forReplicate;
        InitializerTransform*    _initializer;
        PeekTransform*           _peek;
        PortTransform*           _port;
        PortConnectionTransform* _portConnection;
        SharedTransform*         _shared;
        StateTransform*          _state;
        TaskTransform*           _task;
        TaskgroupTransform*      _taskgroup;
        TeamReplicateTransform*  _teamReplicate;
        UserPortTransform*       _userPort;
        VariableTransform*       _variable;
    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_TRANSFORM_H */

