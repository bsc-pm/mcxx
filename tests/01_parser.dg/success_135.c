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

typedef struct FILE_tag FILE;
FILE* fopen(const char*, const char*);
void* malloc(unsigned long);
void exit(int);
int fprintf(FILE*, const char*, ...);

int
main ()
{

    struct foo { char c; _Bool x; };
    struct foo *p = (struct foo *) malloc(sizeof(struct foo));
    int diff;
    FILE *f=fopen("conftestval", "w");
    if (!f) exit(1);
    diff = ((char *)&p->x) - ((char *)&p->c);
    fprintf(f, "%d\n", (diff >= 0) ? diff : -diff);

  ;
  return 0;
}

