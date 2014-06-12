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
test_generator=config/mercurium-serial-simd-avx2
</testinfo>
*/

#include <stdio.h>

int main()
{
    int i;
    int s = 0;
    int t = 0;
    int d = 0;
    float e = 0.0f;
    float f = 0.0f;
#pragma omp simd reduction(+:s,f,t) 
        for(i=0; i<100; i++)
        {
            s += (i+1);
            f += (i+1.0f);
        }

#pragma omp simd reduction(-:d, e) 
        for(i=0; i<100; i++)
        {
            d -= (i+1);
            e -= (i+1.0f);
        }

    printf("%d %f %d %f %d\n", s, f, d, e, t);

    if ((s != 5050) || (f != 5050.0f)
            || (d != -5050) || (e != -5050.0f)
            || (t != 0))
        return 1;

    return 0;
}
