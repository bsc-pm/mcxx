/*--------------------------------------------------------------------
 * (C) Copyright 2006-2012 Barcelona Supercomputing Center
 *                         Centro Nacional de Supercomputacion
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
 - *-------------------------------------------------------------------*/



/*
<testinfo>
test_generator=config/mercurium-analysis
test_nolink=yes
</testinfo>
*/

int z = 10;

void foo(int y)
{
    int task_res = 0;
    int res2 = 0;
    int x = 5;
    int l = 5;
    #pragma analysis_check assert live_in(task_res, x, y, z) live_out(task_res, z) dead(l)
    #pragma omp task shared(task_res) private(l)
    {
        l = 10;
        task_res += x + y + z + l;
    }
    
    res2 = task_res + z;
    #pragma omp taskwait
    res2 += task_res + y + l;
}
