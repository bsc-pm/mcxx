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
test_generator=config/mercurium-ompss
test_ENV="NX_THROTTLE=dummy"
</testinfo>
*/


int A[1024];
int B[1024];
int A_indexes[4];
int A_sizes[4];

int main() {

    A_indexes[0] = 0;
    A_sizes[0] = 25;

    A_indexes[1] = 25;
    A_sizes[1] = 200;

    A_indexes[2] = 225;
    A_sizes[2] = 500;

    A_indexes[3] = 725;
    A_sizes[3] = 524;

#pragma omp task in(B) inout( { A[ A_indexes[i] ; A_sizes[i] ] , i = 0 ; 4 } )
   {
   }

#pragma omp taskwait

   return 0;
}
