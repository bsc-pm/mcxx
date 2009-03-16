/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - David Rodenas Pico
    Copyright (C) 2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
*/
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

