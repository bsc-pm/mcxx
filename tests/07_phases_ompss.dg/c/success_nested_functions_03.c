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

#include <assert.h>

int main_wd = 0;

int main(int argc, char *argv[])
{
    int y;
    main_wd = nanos_get_wd_id(nanos_current_wd());

#pragma omp target device(smp) no_copy_deps
#pragma omp task inout(*x)
    void g(int *x);

    y = 1;
    g(&y);
    g(&y);
    g(&y);
    g(&y);
    g(&y);

#pragma omp taskwait

    return 0;
}

void g(int *x)
{
    // We use this to check that we are actually running in a task
    int local_wd = nanos_get_wd_id(nanos_current_wd());
    assert(local_wd != main_wd);

    (*x)++;
}
