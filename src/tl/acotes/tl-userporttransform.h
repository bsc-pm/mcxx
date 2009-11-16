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
    private:
        virtual void transformReplacement(UserPort* userPort);
        
    // -- Generator
    private:
        virtual std::string generateReplacement(UserPort* userPort);
        virtual std::string generateInputPort(UserPort* userPort);
        virtual std::string generateOutputPort(UserPort* userPort);
    };

} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_USERPORTTRANSFORM_H */

