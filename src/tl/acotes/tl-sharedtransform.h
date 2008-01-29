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
    // -- Transform
    public:
        static void transform(SharedCheck* sharedCheck);
        static void transform(SharedUpdate* sharedUpdate);
    private:
        static void transformReplacement(SharedCheck* sharedCheck);
        static std::string generateReplacement(SharedCheck* sharedCheck);
        static std::string generateCheck(SharedCheck* sharedCheck);
        static void transformReplacement(SharedUpdate* sharedUpdate);
        static std::string generateReplacement(SharedUpdate* sharedUpdate);
        static std::string generateUpdate(SharedUpdate* sharedUpdate);
        
        
        
    // -- Generator
    public:
        static std::string generateShared(State* state);
        static std::string generateSharedConnection(SharedConnection* sharedConnection);
        static std::string generateAcquire(State* state);
        static std::string generateCheck(State* state);
        static std::string generateUpdate(State* state);
    private:
        
    // -- No Constructor
    private:
        SharedTransform();
    };

} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_SHAREDTRANSFORM_H */

