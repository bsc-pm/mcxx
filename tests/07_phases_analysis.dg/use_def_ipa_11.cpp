/*--------------------------------------------------------------------
 * (C) Copyright 2006-2012 Barcelona Supercomputing Center
 *                        Centro Nacional de Supercomputacion
 * 
 * This file is part of Mercurium C/C++ source-to-source compiler.
 * 
 * See AUTHORS file in the top level directory for information
 * regarding developers and contributors.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 * 
 * Mercurium C/C++ source-to-source compiler is distributed in the hope
 * that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.  See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with Mercurium C/C++ source-to-source compiler; if
 * not, write to the Free Software Foundation, Inc., 675 Mass Ave,
 * Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/


/*
 <testinfo>
 test_generator=config/mercurium-analysis
 </testinfo>
 */


const int a = 10;
int b;

const int e[2] = {0, 1};
int f[2];

const int* g = &a;          // Pointer to constant
int* const h = &b;          // Constant pointer
const int* const i = &b;    // Constant pointer to constant
int *j;

void rec(int p1, int &p2, int *p3, int *&p4, int *p5)
{
    p1 = b + *p4;
    p2 = a + *p5;
    
    p3++;
    p4 = j;
    
    int x;
    j = &x;
    
    const int *v = g;
    int* const w = h;
    
    int *z;
    #pragma analysis_check assert upper_exposed(h, *h, j, *j, p3, z, *z, g, a, b, x) defined(*j, j, z)
    rec(*h, *j, p3+1, z, &x);
}