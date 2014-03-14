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

signed int *i;
void f1(__typeof__(__sync_fetch_and_add(i, 1)));
void f1(__typeof__(__sync_fetch_and_add_4(i, 1)));
void f1(signed int);

unsigned int *ui;
void f2(unsigned int);
void f2(__typeof__(__sync_fetch_and_add(ui, 1)));
void f2(__typeof__(__sync_fetch_and_add_4(ui, 1)));

signed long *l;
void f3(signed long);
void f3(__typeof__(__sync_fetch_and_add(l, 1)));
#if __SIZEOF_LONG__ == 4
void f3(__typeof__(__sync_fetch_and_add_4(l, 1)));
#elif __SIZEOF_LONG__ == 8
void f3(__typeof__(__sync_fetch_and_add_8(l, 1)));
#else
#error unknown long
#endif

unsigned long *ul;
void f4(unsigned long);
void f4(__typeof__(__sync_fetch_and_add(ul, 1)));
#if __SIZEOF_LONG__ == 4
void f4(__typeof__(__sync_fetch_and_add_4(ul, 1)));
#elif __SIZEOF_LONG__ == 8
void f4(__typeof__(__sync_fetch_and_add_8(ul, 1)));
#else
#error unknown long
#endif

signed long long *ll;
void f5(signed long long);
void f5(__typeof__(__sync_fetch_and_add(ll, 1)));
void f5(__typeof__(__sync_fetch_and_add_8(ll, 1)));

unsigned long long *ull;
void f6(unsigned long long);
void f6(__typeof__(__sync_fetch_and_add(ull, 1)));
void f6(__typeof__(__sync_fetch_and_add_8(ull, 1)));

signed char *c;
void f7(signed char);
void f7(__typeof__(__sync_fetch_and_add(c, 1)));
void f7(__typeof__(__sync_fetch_and_add_1(c, 1)));

unsigned char *uc;
void f8(unsigned char);
void f8(__typeof__(__sync_fetch_and_add(uc, 1)));
void f8(__typeof__(__sync_fetch_and_add_1(uc, 1)));

signed short *s;
void f9(signed short);
void f9(__typeof__(__sync_fetch_and_add(s, 1)));
void f9(__typeof__(__sync_fetch_and_add_2(s, 1)));

unsigned short *us;
void f10(unsigned short);
void f10(__typeof__(__sync_fetch_and_add(us, 1)));
void f10(__typeof__(__sync_fetch_and_add_2(us, 1)));
