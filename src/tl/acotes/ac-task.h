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
// File:   ac-task.h
// Author: drodenas
//
// Created on 19 / desembre / 2007, 15:35
//

#ifndef _AC_TASK_H
#define	_AC_TASK_H


#include <string>
#include <vector>

#include "tl-langconstruct.hpp"

namespace TL { namespace Acotes {
    
    class Port;
    class SharedCheck;
    class SharedUpdate;
    class State;
    class Taskgroup;
    class TeamReplicate;
    class UserPort;
    class Variable;
    
    class Task {
    // -- Tribal behaviour
    public:
        static Task* create(Taskgroup* taskgroup, Task* parent, TL::LangConstruct* construct, TL::LangConstruct* body);
    private:
        Task(const std::string& name);
        static std::vector<Task*> instanceVector;
        
    // -- Name
    public:
        const std::string& getName() const { return name; }
    private:
        std::string name;
        
    // -- LangConstruct support
    public:
        TL::LangConstruct* getBody() const { return body; }
        TL::LangConstruct* getConstruct() const { return construct; }
    private:
        void setBody(TL::LangConstruct* body);
        void setConstruct(TL::LangConstruct* construct);
        TL::LangConstruct* body;
        TL::LangConstruct* construct;

    // -- Taskgroup relationship
    public:
        Taskgroup* getTaskgroup() const { return taskgroup; };
        bool hasTaskgroup() const { return taskgroup; }
    private:
        void setTaskgroup(Taskgroup* taskgroup);
        Taskgroup* taskgroup;
        
    // -- Parent/Child relationship
    public:
        const std::vector<Task*> &getChildVector() const { return childVector; }
        bool isImplicitTask() const;
        Task* getParent() const { return parent; }
        bool hasParent() const { return parent; }
    private:
        void setParent(Task* parent);
        std::vector<Task*> childVector;
        Task* parent;
        
    // -- Port relationship
    public:
        int addPort(Port* port);
        const std::vector<Port*> &getPortVector() const { return portVector; }
        bool hasInputControlPort() const;
        bool hasInputControlPort(TL::Symbol symbol) const { return getInputControlPort(symbol); };
        Port* getInputControlPort(TL::Symbol symbol) const;
        bool hasOutputControlPort(TL::Symbol symbol) const { return getOutputControlPort(symbol); };
        Port* getOutputControlPort(TL::Symbol symbol) const;
    private:
        std::vector<Port*> portVector;

    // -- State relationship
    public:
        int addState(State* state);
        bool hasState(TL::Symbol symbol) const { return getState(symbol); }
        State* getState(TL::Symbol symbol) const;
        const std::vector<State*> &getStateVector() const { return stateVector; }
    private:
        std::vector<State*> stateVector;

    // -- Port connection creation
    public:
        void createPortConnections();
    private:
        void createChildPortConnections();
        void createVirtualPortandConnection();
        void createArtificalPortandConnection();
        void createArtificalPortandConnection(Port* port);
        void createBypassConnection();
        void createBypassConnection(TL::Symbol symbol);
        Task* createBypassConnection(TL::Symbol symbol, Task* output);
        
    // -- Variable relationship
    public:
        Variable* getVariable(TL::Symbol symbol);
        const std::vector<Variable*> &getVariableVector() const { return variableVector; }
        void addVariable(Variable* variable);
        Variable* getParentVariable(Variable* variable) const;
    private:
        std::vector<Variable*> variableVector;
        
    // -- Initializer/Finalizer relationship
    public:
        void addInitializer(TL::Symbol symbol) { initializerVector.push_back(symbol); }
        void addFinalizer(TL::Symbol symbol) { finalizerVector.push_back(symbol); }
        const std::vector<TL::Symbol>& getInitializerVector() { return initializerVector; }
        const std::vector<TL::Symbol>& getFinalizerVector() { return finalizerVector; }
    private:
        std::vector<TL::Symbol> initializerVector;
        std::vector<TL::Symbol> finalizerVector;
        
    // -- Bypass symbols
    public:
        void addBypass(TL::Symbol symbol) { bypassVector.push_back(symbol); }
        bool isBypass(TL::Symbol symbol) const;
        bool isBypass(Variable* variable) const;
        const std::vector<TL::Symbol> &getBypassVector() const { return bypassVector; }
    private:
        std::vector<TL::Symbol> bypassVector;
        
    // -- UserPort relationship
    public:
        void addUserPort(UserPort* userPort) { userPortVector.push_back(userPort); }
        const std::vector<UserPort*> &getUserPortVector() const { return userPortVector; }
    private:
        std::vector<UserPort*> userPortVector;
        
    // -- SharedUpdate/SharedCheck relationship
    public:
        void addSharedCheck(SharedCheck* sharedCheck) { sharedCheckVector.push_back(sharedCheck); }
        void addSharedUpdate(SharedUpdate* sharedUpdate) { sharedUpdateVector.push_back(sharedUpdate); }
        const std::vector<SharedCheck*>& getSharedCheckVector() { return sharedCheckVector; }
        const std::vector<SharedUpdate*>& getSharedUpdateVector() { return sharedUpdateVector; }
    private:
        std::vector<SharedCheck*> sharedCheckVector;
        std::vector<SharedUpdate*> sharedUpdateVector;
        
    // -- Team support
    public:
        void setTeam(int team) { this->team= team; } 
        int getTeam() const { return team; }
        bool isTeam() const { return team; }
        bool hasLeader() const;
    private:
        int team;
        
    // -- TeamReplicate relationship
    public:
        void addTeamReplicate(TeamReplicate* teamReplicate);
        const std::vector<TeamReplicate*> &getTeamReplicateVector() { return teamReplicateVector; }        
    private:
        std::vector<TeamReplicate*> teamReplicateVector;
    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _AC_TASK_H */

