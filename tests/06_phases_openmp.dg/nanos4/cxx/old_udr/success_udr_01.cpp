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
test_generator=config/mercurium-nanos4
test_CXXFLAGS=--variable=new_udr:0
</testinfo>
*/

struct MyComplex {
    private:
        double real;
        double imaginary;
    public:
        MyComplex();
        MyComplex(const MyComplex&);
        MyComplex(double, double);
};

MyComplex operator+(const MyComplex&, const MyComplex &);
MyComplex operator-(const MyComplex&, const MyComplex &);
MyComplex operator*(const MyComplex&, const MyComplex &);

#pragma omp declare reduction(+, -:MyComplex) identity(constructor(0, 0))
#pragma omp declare reduction(*:MyComplex) identity(constructor(1, 0))

#define N 100
MyComplex vector[N];

void f(MyComplex x, MyComplex y)
{
    int i;
#pragma omp parallel for reduction(+:x) reduction(*:y)
    for ( i = 0; i < N ; i++ ) 
    {
        x = x + vector[i];
        y = y * vector[i];
    }
}
