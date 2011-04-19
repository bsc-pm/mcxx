/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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
test_generator=config/mercurium-nanos4
test_CFLAGS=--variable=new_udr:0
</testinfo>
*/

#include <stdio.h>
#include <omp.h>

typedef struct {
	int value;
} myint;

void myint_add (myint *a, myint *b)
{
	b->value += a->value;
}

#pragma omp declare reduction(myint_add:myint) identity({0}) order(right)

int main (int argc, char *argv[])
{
	myint total = {0};

	#pragma omp parallel reduction(myint_add:total)
	{
		myint num = {omp_get_thread_num()};
		myint_add(&num, &total);
	}

	printf("total = %d\n",total.value);
}
