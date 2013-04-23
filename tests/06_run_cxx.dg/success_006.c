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
test_generator=config/mercurium-run
</testinfo>
*/
#include<assert.h>
#include<string.h>

typedef struct B_t
{
    int z0, z1;
} B;

typedef struct A_t
{
    char name[64];
    B m;

} A;


A a0 = { "FOO", {4, 5} };
A a1 = { { "FOO" }, {4, 5} };
A a2 = { "FOO" "FAA" , {4, 5} };
A a3 = { 'F','O','O', {65, 65} };

int main()
{
   assert(strcmp(a0.name, "FOO") ==0);
   assert(strcmp(a1.name, "FOO") ==0);
   assert(strcmp(a2.name, "FOOFAA") ==0);
   assert(strcmp(a3.name, "FOOA") ==0);


    assert(a0.m.z0 == 4);
    assert(a1.m.z0 == 4);
    assert(a2.m.z0 == 4);
    assert(a3.m.z0 == 0);


    assert(a0.m.z1 == 5);
    assert(a1.m.z1 == 5);
    assert(a2.m.z1 == 5);
    assert(a3.m.z1 == 0);

    return 0;
}
