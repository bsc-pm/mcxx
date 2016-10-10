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
</testinfo>
*/

#include <unistd.h>
#include <assert.h>

#pragma omp task inout(v[0;N][0;M])
template <typename T, int N, int M>
void producer(T (&v)[N][M])
{
    sleep(1);
    for (int i = 0; i < N; ++i)
        for (int j = 0; j < M; ++j)
            v[i][j] += j + i * N;
}

int main()
{
    const int n = 10;
    const int m = 20;
    int v[n][m];

    #pragma omp task out(v)
    {
        for (int i = 0; i < n; ++i)
            for (int j = 0; j < m; ++j)
                v[i][j] = 0;
    }

    #pragma omp task in(v)
    {
        for (int i = 0; i < n; ++i)
            for (int j = 0; j < m; ++j)
                assert(v[i][j] == 0);
    }

    producer(v);

    #pragma omp task in(v)
    {
        for (int i = 0; i < n; ++i)
            for (int j = 0; j < m; ++j)
                assert(v[i][j] == j + i * n);
    }

    producer(v);

    #pragma omp task in(v)
    {
        for (int i = 0; i < n; ++i)
            for (int j = 0; j < m; ++j)
                assert(v[i][j] == 2*(j + i * n));
    }

    #pragma omp taskwait
}
