/*
<testinfo>
test_generator=config/mercurium-omp
test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes
</testinfo>
*/
/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

// This test might hang
int main(int argc, char *argv[])
{
    unsigned char gate = 0;
#pragma omp parallel shared(gate)
    {
#pragma omp master
        {
            int i, j;
            for (i = 0; i < 100000; i++)
            {
                for (j = 0; j < 100000; j++)
                {
                }
            }
            gate = 1;
#pragma omp flush
        }

        // Make all threads busy wait here
        while (gate == 0);
    }

    return 0;
}
