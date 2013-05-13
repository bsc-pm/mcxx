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
typedef struct
{
    int w,x,y;
} B;


typedef struct
{
    B a,b,c;
    int z1;
    int z2;
} A;


int main()
{
    A a = { { -3, -2, -1 } , 0, 1, 2, { 3, 4, 5 }, 1, 2 };

    int x = { 2 };

    int y[4][3] =
    {
        { 1, 3, 5 },
        { 2, 4, 6 },
        { 3, 5, 7 },
    };

    int z[4][3] = { 1, 3, 5, 2, 4, 6, 3, 5, 7 };
}
