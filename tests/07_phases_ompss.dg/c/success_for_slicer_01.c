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
test_generator="config/mercurium-ompss"
test_compile_fail_nanos6_mercurium=yes
test_compile_fail_nanos6_imcc=yes
</testinfo>
*/
#include <stdio.h>
#include <omp.h>

#define SIZE 100

int main ( int argc, char* argv[] )
{
   int i, error = 0, step = 1;
   int A[SIZE];

   for (i = 0; i<SIZE; i++) A[i]=0;

#pragma omp for schedule(ompss_dynamic)
   for (i = 0; i<SIZE; i+=1)
      A[i]++;

#pragma omp for schedule(ompss_dynamic)
   for (i = 0; i<SIZE; i+=step)
      A[i]--;

   for (i = 0; i<SIZE; i++) if (A[i]!= 0) error++;

   fprintf(stdout,"Result with %d threads is %s\n",
             omp_get_num_threads(),
             error?"UNSUCCESSFUL":"Successful");

   return error;
}
