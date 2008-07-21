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
// File:   tl-sharedtransform.h
// Author: drodenas
//
// Created on 30 / desembre / 2007, 13:22
//

#ifndef _TL_SHAREDTRANSFORM_H
#define	_TL_SHAREDTRANSFORM_H

#include <string>

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
        virtual std::string generateReplacement(SharedCheck* sharedCheck);
        virtual std::string generateCheck(SharedCheck* sharedCheck);
        virtual void transformReplacement(SharedUpdate* sharedUpdate);
        virtual std::string generateReplacement(SharedUpdate* sharedUpdate);
        virtual std::string generateUpdate(SharedUpdate* sharedUpdate);
        
        
        
    // -- Generator
    public:
        virtual std::string generateShared(State* state);
        virtual std::string generateSharedConnection(SharedConnection* sharedConnection);
        virtual std::string generateAcquire(State* state);
        virtual std::string generateCheck(State* state);
        virtual std::string generateUpdate(State* state);
    private:
    };

} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_SHAREDTRANSFORM_H */

