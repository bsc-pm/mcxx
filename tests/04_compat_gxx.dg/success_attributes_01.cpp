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

int x[2] __attribute__((aligned (8)));

int f(int a __attribute__((unused)), int b __attribute__((unused))) {}
int h(int a __attribute__((unused)), int b __attribute__((unused))) {}
 

class A
 {
     public:
         int g(char* ori, char* d) __attribute__((nonnull (1,2)));
 };
 int A::g(char* ori, char* d) { }
 

 struct T 
 { 
     int x[2] __attribute__ ((aligned (8))); 
 };
 struct T var2;
 
 struct S 
 { 
     short foo[3]; 
 } __attribute__ ((aligned (8)));
 struct S var;


int main() {}
