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

#define N  4

unsigned long long int square[N][N];

void sequential_(int x, int y)
{
    #pragma analysis_check assert defined(square[x][y], x, y) \
                           upper_exposed(square[x][y], square[x-1][y], square[x][y-1])
    for (x = 1; x < N; x++) {
        for (y = 1; y < N; y++) {
            square[x][y] = square[x-1][y] + square[x][y] + square[x][y-1];
        }
    }
}

void sequential(int a, int b)
{
    #pragma analysis_check assert defined(square[a][b]) \
                           upper_exposed(square[a][b], square[a-1][b], square[a][b-1], a, b)
    sequential_(a, b);
}

void ompss(int h, int j)
{
    #pragma analysis_check assert defined(square[h][j]) \
                           upper_exposed(square[h][j], square[h-1][j], square[h][j], square[h][j-1], h, j)
    sequential(h, j);
}