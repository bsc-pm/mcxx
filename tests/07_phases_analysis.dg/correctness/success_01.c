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
test_generator=config/mercurium-analysis
test_nolink=yes
</testinfo>
*/

#include <omp.h>
#include <stdlib.h>

#if 1
int solve_1(int* g, int loc, int* allGuesses, int num_guesses)
{
    int i, sol=0;
    
    for (i = 0; i < num_guesses; i++) {
        if(loc<10)
        {
            #pragma analysis_check assert correctness_dead(sol) correctness_race(g[loc])
            #pragma omp task
            {
                g[loc] = allGuesses[i];
                if (solve_1(g, loc+1, allGuesses, num_guesses))
                    #pragma omp critical
                    sol = 1;
            }
        }
        else
        {
            g[loc] = allGuesses[i];
            if (solve_1(g, loc+1, allGuesses, num_guesses))
                #pragma omp critical
                sol = 1;
            g[loc] = 0;
        }
    }
    
    #pragma omp taskwait
    return sol;
}
#endif

#if 1
int solve_2(int* g, int loc, int* allGuesses, int num_guesses, int len)
{
    int i, j, sol=0;

    for (i = 0; i < num_guesses; i++) {
        if(loc<10)
        {
            #pragma analysis_check assert correctness_auto_storage(sol) correctness_race(sol) correctness_incoherent_fp(j)
            #pragma omp task shared(sol)
            {
                int* next_g = (int*) calloc(len, sizeof(int));
                for (j = 0; j < len; j++)
                    next_g[j] = g[j];
                next_g[loc] = allGuesses[i];
                if (solve_2(g, loc+1, allGuesses, num_guesses, len))
                    #pragma omp critical
                    sol = 1;
            }
        }
        else
        {
            int* next_g = (int*) calloc(len, sizeof(int));
            int j;
            for (j = 0; j < len; j++)
                next_g[j] = g[j];
            next_g[loc] = allGuesses[i];
            if (solve_2(g, loc+1, allGuesses, num_guesses, len))
                #pragma omp critical
                sol = 1;
        }
    }

    return sol;
}
#endif
