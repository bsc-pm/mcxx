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
test_compile_fail_nanos6_mercurium=yes
test_compile_fail_nanos6_imcxx=yes
</testinfo>
*/
#include<assert.h>

#define N 10
struct A
{
    bool _sign;

    A() : _sign(false) {}
    A(bool b) : _sign(b) {}

    void operator() (int *v, int n)
    {
        #pragma omp for
        for (int i = 0; i < n; ++i)
        {
            v[i] = i;
        }
    }

    A operator -()
    {
        A aux;
        #pragma omp task out(aux) in(*this)
        {
            aux = *this;
            aux._sign = !aux._sign;
        }
        #pragma omp taskwait
        return aux;
    }

    bool operator ==( const A & a)
    {
        bool res;
        #pragma omp task out(res) in(*this, a)
        {
            res = (this->_sign == a._sign);
        }
        #pragma omp taskwait
        return res;
    }
};

int main()
{
    int v[N];
    A A_true(true);

    A_true(v, N);

    /* checking array */
    for (int i = 0; i < N; ++i)
    {
        assert(v[i] == i);
    }

    A A_false = -A_true;
    assert(A_true._sign == true);
    assert(A_false._sign == false);

    assert(!(A_true == A_false));
}
