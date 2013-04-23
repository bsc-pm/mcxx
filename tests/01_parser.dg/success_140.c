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

typedef unsigned long size_t;

const char* __string2_1bptr_p(const char*);
int __strcmp_cg(const char*, const char*, int);
int __strcmp_gc(const char*, const char*, int);


#define strcmp(s1,s2) __extension__ ({ size_t __s1_len, __s2_len; (__builtin_constant_p (s1) && __builtin_constant_p (s2) && (__s1_len = __builtin_strlen (s1), __s2_len = __builtin_strlen (s2), (!__string2_1bptr_p (s1) || __s1_len >= 4) && (!__string2_1bptr_p (s2) || __s2_len >= 4)) ? __builtin_strcmp (s1, s2) : (__builtin_constant_p (s1) && __string2_1bptr_p (s1) && (__s1_len = __builtin_strlen (s1), __s1_len < 4) ? (__builtin_constant_p (s2) && __string2_1bptr_p (s2) ? __builtin_strcmp (s1, s2) : __strcmp_cg (s1, s2, __s1_len)) : (__builtin_constant_p (s2) && __string2_1bptr_p (s2) && (__s2_len = __builtin_strlen (s2), __s2_len < 4) ? (__builtin_constant_p (s1) && __string2_1bptr_p (s1) ? __builtin_strcmp (s1, s2) : __strcmp_gc (s1, s2, __s2_len)) : __builtin_strcmp (s1, s2)))); })

int f(const char* a, const char *b)
{
    if ( strcmp(a, b) )
    {
        a = (char*)0;
        b = (char*)0;
    }
}
