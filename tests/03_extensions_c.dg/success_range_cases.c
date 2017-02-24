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
#include<assert.h>

void testing_range_cases_int(int n, int res)
{
    int val = -1;
    switch (n)
    {
        case -10 ... 10U:
        case 12 ... 15:
            val = 42;
            break;
        default:
            break;
    }
    assert(val == res);
}

void testing_range_cases_char(char n, int res)
{
    int val = -1;
    switch (n)
    {
        case 'a' ... 'z':
            val = 1;
            break;
        case '_':
            val = 42;
            break;
        case 'A' ... 'Z':
            val = 2;
            break;
        case 12 ... 13:
        default:
            break;
    }
    assert(val == res);
}


void testing_range_cases_unsigned_int(unsigned int n, int res)
{
    int val = -1;
    switch (n)
    {
        case 12 ... 15:
            val = 42;
            break;
        default:
            break;
    }
    assert(val == res);
}



int main()
{
    testing_range_cases_unsigned_int(11, -1);
    testing_range_cases_unsigned_int(16, -1);
    testing_range_cases_unsigned_int(12, 42);
    testing_range_cases_unsigned_int(15, 42);
    testing_range_cases_int(-11,-1);
    testing_range_cases_int(-10,42);
    testing_range_cases_int(-5, 42);
    testing_range_cases_int(5,  42);
    testing_range_cases_int(10, 42);
    testing_range_cases_int(12, 42);
    testing_range_cases_int(15, 42);
    testing_range_cases_int(11, -1);
    testing_range_cases_int(16, -1);
    testing_range_cases_char('a', 1);
    testing_range_cases_char('z', 1);
    testing_range_cases_char('A', 2);
    testing_range_cases_char('Z', 2);
    testing_range_cases_char('_', 42);
    testing_range_cases_char('.', -1);
}
