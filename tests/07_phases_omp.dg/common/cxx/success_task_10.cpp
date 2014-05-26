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
test_generator=config/mercurium-omp
</testinfo>
*/
#include <cstdlib>

template <typename T>
struct C
{
    T p;
    // This should not be needed but our transformation requires it
    C() : p(0) { }
    C(const C &c) : p(c.p) { }
    C(T _p) : p(_p) { }

    C& operator++(int)
    {
        p++;
    }
};

template <typename T>
struct B
{
    static void f(C<T>&);
};

template <typename T>
void B<T>::f(C<T>& x)
{
#pragma omp task firstprivate(x)
    {
        x++;
    }
}

int main(int argc, char *argv[])
{
    {
        B<int> b;
        C<int> c(2);
        b.f(c);
#pragma omp taskwait

        if (c.p != 2) abort();
    }
    {
        B<long> b;
        C<long> c(3L);
        b.f(c);
#pragma omp taskwait

        if (c.p != 3L) abort();
    }

    return 0;
}
