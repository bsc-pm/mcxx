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
// File:   ac-teamreplicate.h
// Author: drodenas
//
// Created on 1 / gener / 2008, 16:35
//

#ifndef _AC_TEAMREPLICATE_H
#define	_AC_TEAMREPLICATE_H

#include "tl-langconstruct.hpp"


namespace TL { namespace Acotes {
    
    class Task;
    
    class TeamReplicate {
    // -- Creator
    public:
        static TeamReplicate* create(TL::LangConstruct* construct, TL::LangConstruct* body, Task* task);
    private:
        TeamReplicate();
        
    // -- LangConstruct handling 
    public:
        TL::LangConstruct* getConstruct() const { return construct; }
        TL::LangConstruct* getBody() const { return body; }
    private:
        void setConstruct(TL::LangConstruct* construct);
        void setBody(TL::LangConstruct* body);
        TL::LangConstruct* construct;
        TL::LangConstruct* body;
        
    // -- Task relationship
    public:
        Task* getTask() const { return task; }
    private:
        void setTask(Task* task);
        Task* task;
    };
    
} /* end namespace Acotes */ } /* end namespace TL */



#endif	/* _AC_TEAMREPLICATE_H */

