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

#include <stdio.h>
#include <stdlib.h>
#include "omp.h"

#define N 100

class myInt {
   int x;

public:
   myInt() : x(0) {}

   myInt & operator+= (const myInt &b) {  this->x += b.x; return *this; }
   myInt & operator+= (const int b) { this->x += b; return *this; }
   int getX() const { return x; }
};

#pragma omp declare reduction( + : myInt : omp_out += omp_in)

int main (int argc, char **argv)
{
   int i,s=0;
   int a[N];
   myInt x;

   for ( i = 0; i < N ; i++ ) {
       a[i] = i;
       s += i;
   }

   #pragma omp parallel for reduction(+:x)
   for ( i = 0; i < N ; i++ )
   {
        x += a[i];
   }

   if ( x.getX() != s ) abort();
   return 0;
}
