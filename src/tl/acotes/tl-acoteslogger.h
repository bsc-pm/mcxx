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
// File:   tl-acoteslogger.h
// Author: drodenas
//
// Created on 18 / desembre / 2007, 20:29
//

#ifndef _TL_ACOTESLOGGER_H
#define	_TL_ACOTESLOGGER_H

#include <iostream>
#include <string>
#include <tl-langconstruct.hpp>

namespace TL { namespace Acotes {
    
    class AcotesLogger 
    {
    // -- Log methods
    public:
        static std::ostream& info(TL::LangConstruct* langConstruct);
        static std::ostream& debug(TL::LangConstruct* langConstruct);
        static std::ostream& warning(TL::LangConstruct* langConstruct);
        static std::ostream& error(const std::string& locus);
        static std::ostream& error(TL::LangConstruct* langConstruct);
        static int  getErrorCount() { return errorCount; }
        static bool wasAnyError() { return errorCount; }
    private:
        static int errorCount;
        
    // -- No instance
    private:
        AcotesLogger();
    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_ACOTESLOGGER_H */

