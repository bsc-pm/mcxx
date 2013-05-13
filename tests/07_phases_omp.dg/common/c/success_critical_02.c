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
#FIXME - We need to fix this test
test_ignore=yes
# test_generator=config/mercurium-omp
</testinfo>
*/

#include <stdlib.h>

int main(int argc, char *argv[])
{
    unsigned char flagA = 0;
    unsigned char flagB = 0;
#pragma omp parallel shared(flagA, flagB)
    {
        int i;
        for (i = 0; i < 100; i++)
        {
            int j;
            for (j = 0; j < 100; j++)
            {
#pragma omp critical(A)
                {
                    unsigned char val_of_flagA = j & 0x1;
                    if (flagA != val_of_flagA)
                        __builtin_abort();
                    flagA = (~val_of_flagA) & 0x1;
                }
#pragma omp critical(B)
                {
                    unsigned char val_of_flagB = j & 0x1;
                    if (flagB != val_of_flagB)
                        __builtin_abort();
                    flagB = (~val_of_flagB) & 0x1;
                }
            }
        }
    }

    return 0;
}
