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
test_generator="config/mercurium-ompss"
test_compile_fail_nanos6_mercurium=yes
test_compile_fail_nanos6_imcc=yes
</testinfo>
*/

#if !defined(__ICC) || (__ICC >= 1300)

#include <stdlib.h>

void f1(void)
{
  void g(int *x)
  {
     (*x)++;
  }

  int y;
  y = 1;

#pragma omp task inout(y)
  {
  g(&y);
  }
#pragma omp taskwait
  if (y != 2) abort();
}

void f2(void)
{
#pragma omp task inout(*x)
  void g(int *x)
  {
     (*x)++;
  }

  int y;
  y = 1;

  g(&y);

#pragma omp taskwait
  if (y != 2) abort();
}

int main(int argc, char *argv[])
{
    f1();
    f2();

    return 0;
}

#else

int main(int argc, char *argv[])
{
    return 0;
}

#endif
