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

#include <stdio.h>

int HasNextBurst(FILE *fp){
    int status = 0; 
    char line[256];
    int i;

    int HasEqual=0; 
    int EndOfFile =0;
    while ((!HasEqual)&&(!EndOfFile)) {
        HasEqual= 0;
        i=0;
        while (i<255) {
            line[i]=fgetc(fp);
            if ((int)(line[i])) {
                EndOfFile=1;
                break;
            }
            if (line[i]=='=')
                HasEqual=1;
        }
        line[i+1]='\0';
    }
    #pragma analysis_check assert range(HasEqual:0:1:0; EndOfFile:0:1:0)
    if (HasEqual) {
        status=1;
    }
    // Added for checking purposes
    if (EndOfFile)
        ;
    #pragma analysis_check assert range(status:0:1:0)
    return status;
}
