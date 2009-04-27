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
// File:   ac-peek.h
// Author: drodenas
//
// Created on 4 / gener / 2008, 12:43
//

#ifndef _AC_PEEK_H
#define	_AC_PEEK_H

#include <tl-langconstruct.hpp>

namespace TL { namespace Acotes {

    class Port;
    class State;
    class Task;

    class Peek {
    // -- Creator
    public:
        static Peek* create(TL::LangConstruct* construct, TL::LangConstruct* body, Port* port, State* history, State* index);
    private:
        Peek();
        
    // -- LangConstruct support
    public:
        TL::LangConstruct* getConstruct() const { return construct; }
        TL::LangConstruct* getBody() const { return body; }
    private:
        void setConstruct(TL::LangConstruct* construct);
        void setBody(TL::LangConstruct* body);
        TL::LangConstruct* construct;
        TL::LangConstruct* body;
        
    // -- Task
    public:
        bool hasTask() const { return getTask(); }
        Task* getTask() const;
        
    // -- Input port relationship
    public:
        Port* getPort() const { return port; }
    private:
        void setPort(Port* port);
        Port* port;
        
    // -- History variable
    public:
        State* getHistory() const { return history; }
        int getPeekWindow();
    private:
        void setHistory(State* state); 
        State* history;
        
    // -- Optative index variable
    public:
        bool hasIndex() const { return getIndex(); }
        State* getIndex() const { return index; }
    private:
        void setIndex(State* state);
        State* index;
    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _AC_PEEK_H */

