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
test_generator=config/mercurium-ompss
</testinfo>
*/

#include<assert.h>

struct C
{
    int x;
    int y[5];
};

#pragma omp task out(c->y[0:4])
void producer(struct C* c)
{
    for (int i = 0; i < 5; ++i)
    {
        c->y[i] = 4;
    }
}

#pragma omp task inout(c->y[0:4])
void consumer(struct C* c)
{
}

int main()
{
    struct C c;

    c.x = 1;

    for (int i = 0; i < 5; ++i)
    {
        c.y[i] = -1;
    }

    producer(&c);
    consumer(&c);

#pragma omp taskwait

    for (int i = 0; i < 5; ++i)
    {
        c.y[i] = 4;
    }
}
