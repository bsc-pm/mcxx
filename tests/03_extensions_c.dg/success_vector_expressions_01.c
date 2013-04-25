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
test_generator=config/mercurium-extensions
</testinfo>
*/


void f(void)
{
    int   __attribute__((generic_vector)) A;
    float __attribute__((generic_vector)) B;

    int a;
    float b;

    A = A + a;
    //B = A + b;
    A = a + A;
    B = b + B;
    A = A + A;
    //B = A + B;

    A = A - a;
    //B = A - b;
    A = a - A;
    B = b - B;
    A = A - A;
    //B = A - B;

    A = A * a;
    //B = A * b;
    A = a * A;
    B = b * B;
    A = A * A;
    //B = A * B;

    A = A / a;
    //B = A / b;
    A = a / A;
    B = b / B;
    A = A / A;
    //B = A / B;

    A = A | a;
    A = a | A;
    A = A | A;

    A = A & a;
    A = a & A;
    A = A & A;

    A = A ^ a;
    A = a ^ A;
    A = A ^ A;

    A += A + a;
  //B += A + b;
    A += a + A;
    B += b + B;
    A += A + A;
  //B += A + B;

    A -= A + a;
  //B -= A + b;
    A -= a + A;
    B -= b + B;
    A -= A + A;    
  //B -= A + B;

    A *= A + a;
  //B *= A + b;
    A *= a + A;
    B *= b + B;
    A *= A + A;
  //B *= A + B;

   A /= A + a;
 //B /= A + b;
   A /= a + A;
   B /= b + B;
   A /= A + A;
 //B /= A + B;

//   A &= A + a;
//   A &= a + A;
//   A &= A + A;

//   A |= A + a;
//   A |= a + A;
//   A |= A + A;
//
//   A ^= A + a;
//   A ^= a + A;
//   A ^= A + A;

   //%, <<, >>, >>=, <<=, ++, --, etc... not supported yet.

}
