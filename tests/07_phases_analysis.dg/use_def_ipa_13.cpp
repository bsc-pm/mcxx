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


#include <cstdlib>

struct R {
    int b[2];
    R() {}
};
struct S {
    int a;
    struct R r;
    S() {}
};

struct Q {
    int* a;
    int b[2];
    Q() {}
};
struct T {
    int* a;
    Q* s;
    int* b;
    int* c;
    T() {}
};

void foo1(int a, int &b, ...);
void foo2(int* a, int *&b, int **d, ...);
void foo3(int a, int &b, int &c);
void foo4(int a[], int (*b)[2], int* c, ...);

const int N = 4;
int main(int argc, char** argv)
{
    // *** scalars *** //
    struct S s[10];
    s[5].a;
    s[2].r.b[1];
    
    #pragma analysis_check assert upper_exposed(s[2].r.b[1]) undefined(s[5].a)
    foo1(s[2].r.b[1], s[5].a);
    
    
    // *** pointers *** //
    T t1;
    t1.a = (int*) malloc(sizeof(int)*2);
    t1.s = (Q*) malloc(sizeof(Q)*2);
    
    #pragma analysis_check assert undefined(t1.a[0:1], (*t1.s).a, *(*t1.s).a, *t1.b, *t1.c)
    foo2(t1.a, t1.s->a, &t1.b, t1.c);
    
    // 
    #pragma analysis_check assert undefined((*t1.s).b[0:1], *(*t1.s).a) upper_exposed(*t1.a) 
    foo3(*t1.a, *t1.s->a, *t1.s->b);
    
    
    // *** arrays *** //
    int c1[N];
    int c2[2];
    int c3[2];
    int c4[2];
    
    #pragma analysis_check assert undefined(c1[0:3], c2[0:1], c3[0:1], c4[0:1])
    foo4(c1, &c2, c3, &c4[0]);
    
    return 0;
}
