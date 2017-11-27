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
#include<cassert>

class Object
{
    int _x;
    public:
        Object() : _x(42) {}
        Object(int x) : _x(x) {}
        Object(const Object& o) : _x(o._x) {}
        ~Object() {}

        int get_x() const { return _x; }
};

struct A
{
    int foo(const Object& o)
    {
        int error;
        #pragma omp task shared(error)
            error = (o.get_x() != 13);

        #pragma omp taskwait
        return error;
    }
};

template < class E>
struct B
{
    int foo(const E& e)
    {
        int error;
        #pragma omp task shared(error)
            error = (e.get_x() != 13);

        #pragma omp taskwait
        return error;
    }
};

int main()
{
    Object o(13);

    A a;
    assert(a.foo(o) == 0);

    B<Object> b;
    assert(b.foo(o) == 0);
}
