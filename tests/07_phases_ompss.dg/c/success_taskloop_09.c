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
</testinfo>
*/

#include<stdio.h>
#include<string.h>
#include<assert.h>

void validate(int *v, int start, int end, int val)
{
    for (int i = start; i < end; ++i)
        assert(v[i] == val);
}

#define N 50

#define BEG  0
#define MID 25
#define END N

#define INIT \
    memset(v, 0, N*sizeof(int));

#define VALIDATE \
        validate(v, BEG, MID, /* value */ 1); \
        validate(v, MID, END, /* value */ 0);

int main()
{
    int v[N];

    for (int k = 1; k <= N; ++k) {
        INIT
        #pragma omp taskloop shared(v) grainsize(k)
        for (int i = BEG; i < MID; ++i)
            v[i]++;
        VALIDATE

        INIT
        #pragma omp taskloop shared(v) grainsize(k)
        for (int i = BEG; i <= MID-1; ++i)
            v[i]++;
        VALIDATE

        INIT
        #pragma omp taskloop shared(v) grainsize(k)
        for (int i = MID-1; i > BEG-1; --i)
            v[i]++;
        VALIDATE

        INIT
        #pragma omp taskloop shared(v) grainsize(k)
        for (int i = MID-1; i >= BEG; --i)
            v[i]++;
        VALIDATE
    }

    for (int k = 1; k <= N; ++k) {
        INIT
        #pragma omp taskloop shared(v) num_tasks(k)
        for (int i = BEG; i < MID; ++i)
            v[i]++;
        VALIDATE

        INIT
        #pragma omp taskloop shared(v) num_tasks(k)
        for (int i = BEG; i <= MID-1; ++i)
            v[i]++;
        VALIDATE

        INIT
        #pragma omp taskloop shared(v) num_tasks(k)
        for (int i = MID-1; i > BEG-1; --i)
            v[i]++;
        VALIDATE

        INIT
        #pragma omp taskloop shared(v) num_tasks(k)
        for (int i = MID-1; i >= BEG; --i)
            v[i]++;
        VALIDATE
    }
}
