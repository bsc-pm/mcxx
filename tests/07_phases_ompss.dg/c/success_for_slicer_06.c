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
test_generator="config/mercurium-ompss"
test_compile_fail_nanos6_mercurium=yes
test_compile_fail_nanos6_imcc=yes
</testinfo>
*/

#include <nanos.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define ARRAY_SIZE 23

#define TEST_SLICER(chunksize) \
   for (i=0; i<ARRAY_SIZE; i++) A[i] = nanos_get_wd_id(nanos_current_wd()); \
   if ( !check_chunksize( A, ARRAY_SIZE, chunksize ) )  error++;

bool check_chunksize( int* A, int N, int cs )
{
   int i, j;
   for (i=0; i<N; i+=j) {
      int current_id = A[i];
      for (j=1; j<cs && i+j<N; j++) {
         if ( A[i+j] != current_id )
            return false;
      }
   }
   return true;
}

int main( int argc, char *argv[] )
{
   int i;
   int error = 0;
   int A[ARRAY_SIZE];

   #pragma omp for schedule(static,2)
   TEST_SLICER( 2 );

   #pragma omp for schedule(dynamic,2)
   TEST_SLICER( 2 );

   #pragma omp for schedule(static,13)
   TEST_SLICER( 13 );

   #pragma omp for schedule(dynamic,13)
   TEST_SLICER( 13 );

   if ( error ) {
      //fprintf(stderr, "%s: %s\n", argv[0], "unsuccessful");
      return EXIT_FAILURE;
   }
   else {
      //fprintf(stderr, "%s : %s\n", argv[0], "successful");
      return EXIT_SUCCESS;
   }
}
