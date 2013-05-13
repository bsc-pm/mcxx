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
_Complex int i_a = 3i;
_Complex int i_c = 3j;

_Complex unsigned int ui_a = 3iu;
_Complex unsigned int ui_c = 3ju;

_Complex long l_a = 3il;
_Complex long l_c = 3jl;

_Complex double d_a = 3.0i;
_Complex double d_c = 3.0j;

_Complex float f_a = 3.0fi;
_Complex float f_c = 3.0fj;

void f(void)
{
    i_a + i_c;
    i_a = i_c;
    i_a += i_c;

    f_a + f_c;
    f_a = f_c;
    f_a += f_c;

    f_a = d_a;
    d_a = f_a;

    f_a = i_a;
}
