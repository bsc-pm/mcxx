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

float coeffs[10] = {0.9, 1.1, 1.2, 1.3, 1.4,
    1.5, 1.6, 1.7, 1.8, 1.9};

// This test checks that variable 'tmp', which is local to the loop
// is not falsely detected as an induction variable
void foo (float *out, float d0)
{
    int i;

    #pragma analysis_check assert induction_var(i:0:15:1)
    for (i=0; i < 16; i++)
    {
        float tmp  = coeffs[0] * d0;
        tmp += coeffs[1] * d0;
        out[i] = tmp;
    }
}

void bar (float *out, float d0)
{
    int i, tmp;

    #pragma analysis_check assert induction_var(i:0:15:1)
    for (i=0; i < 16; i++)
    {
        tmp = coeffs[0] * d0;
        tmp += coeffs[1] * d0;
        out[i] = tmp;
    }
}
