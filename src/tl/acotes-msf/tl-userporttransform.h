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
// File:   tl-userporttransform.h
// Author: drodenas
//
// Created on 29 / desembre / 2007, 17:22
//

#ifndef _TL_USERPORTTRANSFORM_H
#define	_TL_USERPORTTRANSFORM_H

#include <string>
#include <tl-langconstruct.hpp>

namespace TL { namespace Acotes {

    class UserPort;
    
    class UserPortTransform {
        
    // -- Constructor
    public:
        UserPortTransform(const std::string& driver);
    protected:
        const std::string driver;

    // -- Transform
    public:
        virtual void transform(UserPort* userPort);
        virtual void transform(UserPort* userPort, int last);
        virtual void transform2(UserPort* userPort);
    private:
        virtual void transformReplacement(UserPort* userPort, int last);
        virtual void transformReplacement2(UserPort* userPort);
        
    // -- Generator
    private:
        virtual Source generateReplacement(UserPort* userPort, int);
        virtual Source generateReplacement2(UserPort* userPort);
        virtual Source generateInputPort(UserPort* userPort);
        virtual Source generateOutputPort(UserPort* userPort);
        virtual Source generateOutputPortAccess(UserPort* userPort);
        virtual Source generateOutputPortAccess2(UserPort* userPort);
    };

} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_USERPORTTRANSFORM_H */

