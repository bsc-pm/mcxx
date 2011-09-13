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
test_generator=config/mercurium-nanox
</testinfo>
*/

#pragma omp task deadline(a) release_deadline(b) on_error(b)
void success_deadline_release_deadline_on_error(int f) {}

#pragma omp task release_deadline(b) on_error(b)
void success_release_deadline_on_error(int f) {}

#pragma omp task deadline(a) release_deadline(b)
void success_deadline_release_deadline(int f) {}

#pragma omp task deadline(a) on_error(b)
void success_deadline_on_error(int f) {}

#pragma omp task release_deadline(b)
void success_release_deadline(int f) {}

#pragma omp task deadline(b)
void success_deadline(int f) {}

#pragma omp task onerror(a:b)
void success_on_error(int f) {}


int main(){}
