/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
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



/*
<testinfo>
test_generator=config/mercurium-analysis
test_nolink=yes
</testinfo>
*/

#include <string>

std::string UNKNOWN = "UNKNOWN";

int foo(int n)
{
    int result;

    if (n > 2)
    {
        if (n < 100)
        {
            if (n > 50)
                return 1;
            return 2;
        }
        else
        {
            #pragma analysis_check assert live_in(n) live_out(n)
            result = n / 0;
        }
        
        #pragma analysis_check assert reaching_definition_in(n: UNKNOWN) reaching_definition_out(result: n/0)
        result = n / 0;
    }
    
    #pragma analysis_check assert reaching_definition_in(result: n/0)
    return result;
}