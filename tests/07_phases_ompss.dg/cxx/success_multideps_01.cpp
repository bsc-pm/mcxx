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
test_CXXFLAGS="--no-copy-deps"
</testinfo>
*/
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <stdlib.h>

void generate0(int length, int *a, int val)
{
#pragma omp task out( { a[k], k = 0;length } )
    {
        usleep(50);
        int i;
        for (i = 0; i < length; i++)
        {
            a[i] = val;
        }
    }
}

void consume0(int length, int *a, int val)
{
#pragma omp task in( { a[k], k = 0;length } )
    {
        usleep(50);
        int i;
        for (i = 0; i < length; i++)
        {
            if (a[i] != val)
            {
                fprintf(stderr, "a[%d] == %d but should be %d\n", i, a[i], val);
                abort();
            }
        }
    }
}

void generate1(int start, int length, int *a, int val)
{
#pragma omp task out( { a[k], k = start;length } )
    {
        usleep(50);
        int i;
        for (i = start; i < (start + length); i++)
        {
            a[i] = val;
        }
    }
}

void consume1(int start, int length, int *a, int val)
{
#pragma omp task in( { a[k], k = start;length } )
    {
        usleep(50);
        int i;
        for (i = start; i < (start + length); i++)
        {
            if (a[i] != val)
            {
                fprintf(stderr, "a[%d] == %d but should be %d\n", i, a[i], val);
                abort();
            }
        }
    }
}


enum { NUM_ITEMS = 100, NUM_TASKS = 10000 };
int vec[NUM_ITEMS];

int main(int argc, char *argv[])
{
    fprintf(stderr, "Initializing %d items...\n", NUM_ITEMS);
    int i;
    for (i = 0; i < NUM_ITEMS; i++)
    {
        vec[i] = i;
    }

    fprintf(stderr, "First round: Creating %d tasks...\n", NUM_TASKS);
    for (i = 0; i < NUM_TASKS; i++)
    {
        int start = (i % NUM_ITEMS);
        int length = 20;
        if ((start + length) >= NUM_ITEMS)
        {
            length = NUM_ITEMS - start;
        }

        int val = i;

        generate0(length, vec + start, val);
        consume0(length, vec + start, val);
    }

    fprintf(stderr, "Second round: Creating %d tasks...\n", NUM_TASKS);
    for (i = 0; i < NUM_TASKS; i++)
    {
        int start = (i % NUM_ITEMS);
        int length = 20;

        if ((start + length) >= NUM_ITEMS)
        {
            length = NUM_ITEMS - start;
        }

        int val = i;

        generate1(start, length, vec, val);
        consume1(start, length, vec, val);
    }
    fprintf(stderr, "Waiting tasks...\n");

#pragma omp taskwait

    return 0;
}
