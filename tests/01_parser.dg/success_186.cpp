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
test_generator=config/mercurium
</testinfo>
*/
template <typename T>
float* f(int n) { }

template <int N>
double* f(int n) { }

struct B
{
    template <typename T>
        float* f(const T &t) { }

    template <typename Q, typename T>
        double* f(const T &t) { }
};

template <typename S>
struct C
{
    template <typename T>
        float* f(const T &t) { }

    template <typename Q, typename T>
        double* f(const T &t) { }
};

void g()
{
    float *pf;
    double *pd;

    pf = f<int>(3);
    pd = f<10>(3);

    char pc;
    B b;
    pf = b.f(pc);

    short ps;
    pd = b.f<float>(ps);

    C<int> c;
    pf = c.f(pc);

    pd = c.f<float>(ps);
}
