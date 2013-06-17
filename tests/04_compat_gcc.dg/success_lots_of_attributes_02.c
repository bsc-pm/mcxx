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


// Do not worry about the warnings
__attribute__ ((deprecated)) int  a1, b1;
int __attribute__ ((deprecated)) c1, d1;
int e1 __attribute__ ((deprecated)), f1;

struct U
{
};

__attribute__ ((deprecated)) struct U  a2, b2;
struct U __attribute__ ((deprecated)) c2, d2;
 struct U e2 __attribute__ ((deprecated)), f2;

typedef struct V {} X;

__attribute__ ((deprecated)) X  a3, b3;
X __attribute__ ((deprecated)) c3, d3;
 X e3 __attribute__ ((deprecated)), f3;

int foo()
{
    a1; b1;
    c1; d1;
    e1; f1;
    a2; b2;
    c2; d2;
    e2; f2;
    a3; b3;
    c3; d3;
    e3; f3;
}
