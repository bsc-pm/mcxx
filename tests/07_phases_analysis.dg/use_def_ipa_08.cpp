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
 test_nolink=yes
 </testinfo>
 */

// #include<iostream>
// #include <assert.h>
// #include <cstdarg>

void foo(int n, ...);
// void foo(int n, int x, int y, int* res);
// {
//     va_list arg_ptr;
//     va_start(arg_ptr, n);
//     
//     int tmp_res = 0;
//     for(int i=0; i<n-1; ++i)
//         tmp_res += va_arg(arg_ptr, int);
//     int* res = va_arg(arg_ptr, int*);
//     *res = tmp_res;
//     
//     va_end(arg_ptr);
// }

int main()
{
    int n = 3;
    int x = 2, y = 2;
    int empty_res;
    int* res = &empty_res;
    #pragma analysis_check assert upper_exposed(x, y, n, res) undefined(*res)
    foo(n, x, y, res);
    
//     assert(*res == 4);
    return 0;
}
