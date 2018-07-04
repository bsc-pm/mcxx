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
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
test_CFLAGS=-std=gnu99
</testinfo>
*/
#include<stdio.h>
#include<assert.h>

#define N 10

int main()
{
    int res = 0;
    int v[N][N];
    for (int i = 0; i < N; ++i)
        for (int j= 0; j < N; ++j)
            v[i][j] = i*N + j+1;

    for (int i = 0; i < N; ++i)
    {
#ifdef __NANOS6__
        #pragma oss task weakreduction(+: res) in(v) firstprivate(i)
#else
        #pragma oss task reduction(+: res) in(v) firstprivate(i)
#endif
        {
#ifdef __NANOS6__
            #pragma oss task reduction(+: res) in(v) firstprivate(i)
#endif
            res  += v[i][0];
            for (int j = 0+1; j < N; ++j)
            {
                #pragma omp task reduction(+: res) in(v) firstprivate(i)
                {
                    res += v[i][j];
                }
            }
#ifndef __NANOS6__
            #pragma omp taskwait
#endif
        }

    }

    #pragma omp task in(res)
    {
        assert(res == (( (N*N) * ((N*N)+1)) / 2));
    }

    #pragma omp taskwait
}
