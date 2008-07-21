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

