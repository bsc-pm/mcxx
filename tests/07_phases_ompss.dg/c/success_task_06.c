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
test_nolink=yes
</testinfo>
*/

extern int me;
extern int nodes;

void matmul ( int m, int n, double (*A)[m], double (*B)[n], double (*C)[n] )
{
    double (*a)[m];
    double (*rbuf)[m];
    double (*orig_rbuf)[m];
    void   *ptmp;
    int up, down;
    int i;
    int it;
    int tag = 1000;
    int size = m*n;

    int stats;

    for( it = 0; it < nodes; it++ ) {

      #pragma omp task input (a[0:n-1], B[0:m-1]) inout (C[i:i+n-1][0:n-1]) firstprivate (n,m)
        {
              // Dummy references
            n, n, m, 1.0, (double *)a, m, (double *)B, n, 1.0, (double *)&C[i][0], n;
        }

      if (it < nodes-1) {
         #pragma omp task input (a[0:n-1]) output (rbuf[0:n-1]) inout(stats) firstprivate (size,m,n,tag,down,up)
          {
              // Dummy references
            a, size, down, tag, rbuf, size, up, tag, &stats;
          }

      }

      i = (i+n)%m;                 //next C block circular
      ptmp=a; a=rbuf; rbuf=ptmp;   //swap pointers
    }

#pragma omp taskwait
}
