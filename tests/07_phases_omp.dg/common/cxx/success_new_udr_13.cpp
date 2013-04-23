/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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
test_generator=config/mercurium-omp
</testinfo>
*/

#include <algorithm>
#include <functional>
#include <vector>

#pragma omp declare reduction( + : std::vector<int> : \
std::transform(omp_in.begin( ), omp_in.end( ), \
omp_out.begin( ), omp_out.begin ( ), std::plus<int >() ) )

#pragma omp declare reduction( merge: std::vector<int>: \
omp_out.insert(omp_out.end(), omp_in.begin(), omp_in.end() ) )

int main (int argc, char* argv[])
{
   std::vector<int> v1(5);
   std::vector<int> v2(5);

   #pragma omp parallel for reduction(merge : v2)
   for (int i=0; i<5; i++)
   {
      v1 = v2;
   }

   return 0;
}
