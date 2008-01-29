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
// File:   tl-userporttransform.h
// Author: drodenas
//
// Created on 29 / desembre / 2007, 17:22
//

#ifndef _TL_USERPORTTRANSFORM_H
#define	_TL_USERPORTTRANSFORM_H

#include <string>

namespace TL { namespace Acotes {

    class UserPort;
    
    class UserPortTransform {
    // -- Transform
    public:
        static void transform(UserPort* userPort);
    private:
        static void transformReplacement(UserPort* userPort);
        
    // -- Generator
    private:
        static std::string generateReplacement(UserPort* userPort);
        static std::string generateInputPort(UserPort* userPort);
        static std::string generateOutputPort(UserPort* userPort);
        
    // -- No Constructor
    private:
        UserPortTransform();
    };

} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_USERPORTTRANSFORM_H */

