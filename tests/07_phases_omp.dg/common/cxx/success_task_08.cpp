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
#include <stdlib.h>

struct A
{
    A() : n(new int(3))
    {
    }
    ~A()
    {
        delete n;
    }

    A(const A& a)
        : n (new int(*a.n))
    {
    }

    void set(int n)
    {
        *this->n = n;
    }

    int get() const
    {
        return *this->n;
    }

    private:

    int *n;

    // Do not call this one
    A& operator=(const A& a)
    {
        if (this != &a)
        {
            *this->n = *a.n;
        }
    }
};

int main(int, char**)
{
    A a;

#pragma omp task firstprivate(a)
    {
        a.set(42);
    }
#pragma omp taskwait

    if (a.get() == 42) abort();

#pragma omp task shared(a)
    {
        a.set(42);
    }
#pragma omp taskwait

    if (a.get() != 42) abort();

    return 0;
}
