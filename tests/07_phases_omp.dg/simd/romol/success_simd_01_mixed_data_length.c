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
test_generator=config/mercurium-serial-simd-romol 
test_ignore=yes
</testinfo>
*/

int test(void)
{
#if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 4)
    int i;
    unsigned char __attribute__((aligned(64))) a[102];
    float __attribute__((aligned(64))) b[102];


#pragma omp simd 
    for (i=0; i<101; i++)
    {
        a[i] = (unsigned char)2;
    }

#pragma omp simd 
    for (i=0; i<101; i++)
    {
        b[i] = 10.0f;
    }

    a[101] = 8;
    b[101] = 7.0f;

#pragma omp simd
    for (i=0; i<101; i++)
    {
        b[i] += 6.0f;
        a[i] = b[i];

    }

    for (i=0; i<101; i++)
    {
        if (a[i] != 16)
        {
            printf("ERROR: a[%d] == %d\n", i, a[i]);
            return 1;
        }

        if (b[i] != 16)
        {
            printf("ERROR: b[%d] == %d\n", i, a[i]);
            return 1;
        }

    }

    if (a[101] != 8)
    {
        printf("ERROR: a[%d] == %d\n", i, a[101]);
        return 1;
    }

    if (b[101] != 7.0f)
    {
        printf("ERROR: b[%d] == %d\n", i, a[101]);
        return 1;
    }

#else
#warning "This compiler is not supported"
#endif
    return 0;
}

int main(int argc, char *argv[])
{
    return test();
}
