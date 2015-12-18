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
#include <cstdio>
#include <cstdlib>

struct my_data_t
{
    int my_data;
};


void reducer(my_data_t* out, my_data_t* in)
{
    out->my_data += in->my_data;
}

void init(my_data_t* priv)
{
    priv->my_data = 0;
}

#pragma omp declare reduction (plus: my_data_t : reducer(&omp_out,&omp_in)) initializer(init(&omp_priv))

my_data_t foo(my_data_t* v, int n)
{
    my_data_t sum;
    sum.my_data = 0;

#pragma omp parallel for reduction(plus : sum)
    for (int i = 0; i < n; i++)
    {
        reducer(&sum, &v[i]);
    }
    return sum;
}

const int NUM_ITEMS = 1000;

int main(int, char**)
{
    my_data_t m[NUM_ITEMS];
    int sum = 0;
    for (int i = 0; i <  NUM_ITEMS; i++)
    {
        m[i].my_data = i;
        sum += i;
    }

    my_data_t s = foo(m, NUM_ITEMS);

    if (s.my_data != sum)
    {
        fprintf(stderr, "%d != %d\n", s.my_data, sum);
        abort();
    }

    return 0;
}
