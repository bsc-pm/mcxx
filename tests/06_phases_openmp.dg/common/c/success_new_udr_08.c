/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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

test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes

test_compile_fail_nanox_instrument=yes
test_compile_faulty_nanox_instrument=yes
</testinfo>
*/

int omp_get_num_threads(void);
int omp_get_thread_num(void);

typedef struct {
  double real;
  double imaginary;
} my_complex_t;

my_complex_t complex_add (my_complex_t a, my_complex_t b) 
{ 
    my_complex_t m = {0.0, 0.0};
    return m;
}

my_complex_t complex_mul (my_complex_t a, my_complex_t b)
{ 
    my_complex_t m = {0.0, 0.0};
    return m;
}

#pragma omp declare reduction(complex_add : my_complex_t : _out = complex_add(_in,_out)) identity({0,0})
#pragma omp declare reduction(complex_mul : my_complex_t : _out = complex_mul(_in,_out)) identity({1,0})

#define N 100
my_complex_t vector[N];

void f(my_complex_t x, my_complex_t y)
{
    int i;

#ifdef NANOX
    #pragma omp for reduction(complex_add:x) reduction(complex_mul:y)
#else
    #pragma omp parallel for reduction(complex_add:x) reduction(complex_mul:y)
#endif
    for ( i = 0; i < N ; i++ ) 
    {
        x = complex_add(x,vector[i]);
        y = complex_mul(y,vector[i]);
    }
}

