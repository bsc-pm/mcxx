/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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
test_generator=config/mercurium-omp
</testinfo>
*/

#include <stdio.h>
#include <stdlib.h>

#define N 100

#pragma omp declare reduction(mymin:float: omp_out = omp_out > omp_in ? omp_in : omp_out ) initializer(omp_priv = 2147483647)

int omp_get_num_threads(void);
int omp_get_thread_num(void);

int main (int argc, char **argv)
{
   #pragma omp declare reduction(mymin:int: omp_out = omp_out > omp_in ? omp_in : omp_out ) initializer(omp_priv = 2147483647)

   int i,x = N + 1;
   float a[N];

   for ( i = 0; i < N ; i++ ) a[i] = i;

#ifdef NANOX
    #pragma omp for reduction(mymin:x)
#else
    #pragma omp parallel for reduction(mymin:x)
#endif
   for ( i = 0; i < N ; i++ )
   {
        x = a[i] < x ? a[i] : x;
   }

   if ( x != 0 ) abort();
   return 0;
}
