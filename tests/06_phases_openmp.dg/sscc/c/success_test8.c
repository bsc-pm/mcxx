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
test_generator=config/mercurium-ss2omp
</testinfo>
*/

#include <stdlib.h>

#define F1_VAL 111111
#define F2_VAL 222222
#define F3_VAL 333333

#pragma css task inout(a) target device(smp)
void f1(int *a)
{
    *a = F1_VAL;
}

#pragma css task inout(a[1]) target device(smp)
void f2(int *a)
{
    *a = F2_VAL;
}

#pragma css task inout(a) target device(smp)
void f3(int a[1])
{
    *a = F3_VAL;
}

int k;

int main(int argc, char* argv[])
{
    int *m = &k;

    *m = 0;
    f1(m);
#pragma omp taskwait
    if (*m != F1_VAL) abort();
    
    f2(m);
#pragma omp taskwait
    if (*m != F2_VAL) abort();

    f3(m);
#pragma omp taskwait
    if (*m != F3_VAL) abort();

    return 0;
}
