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

#include <stdlib.h>

const unsigned int N = 1024;
const unsigned int CHUNK_SIZE = 128;
const unsigned int N_CHUNKS = 8;

#if 1
double dot_product_1(double A[N], double B[N])
{
    long actual_size;
    int j;
    double acc;

    double *C = malloc (N_CHUNKS*sizeof(double));   

    acc=0.0;
    j=0;
    long i;
    for (i=0; i<N; i+=CHUNK_SIZE) {
        actual_size = (N-CHUNK_SIZE>=CHUNK_SIZE) ? CHUNK_SIZE : (N-CHUNK_SIZE);

        #pragma analysis_check assert correctness_auto_storage(A, B, C) \
                                      correctness_incoherent_in_pointed(A, B) \
                                      correctness_incoherent_out(A, B) correctness_incoherent_in(C) \
                                      correctness_race(C[j])
        #pragma omp task label( dot_prod ) firstprivate(j, i, actual_size) in(C) inout(A, B)
        {
            C[j]=0;

            long ii;
            for (ii=0; ii<actual_size; ii++)
                C[j]+= A[i+ii] * B[i+ii];
        }

        #pragma analysis_check assert correctness_race(acc, C[j]) correctness_auto_storage(acc)
        #pragma omp task label(increment) firstprivate(j) shared(acc)
        acc += C[j];

        j++;
    }

    return(acc);
}
#endif

#if 1
double dot_product_2(double A[N], double B[N])
{
    long actual_size;
    int j;
    double acc;
    
    double *C = malloc (N_CHUNKS*sizeof(double));   
    
    acc=0.0;
    j=0;
    long i;
    for (i=0; i<N; i+=CHUNK_SIZE) {
        actual_size = (N-CHUNK_SIZE>=CHUNK_SIZE) ? CHUNK_SIZE : (N-CHUNK_SIZE);
        
        #pragma analysis_check assert correctness_incoherent_out_pointed(C) \
                                      correctness_incoherent_out([N]A, [N]B) \
                                      correctness_race(C[j])
        #pragma omp task label(dot_prod) firstprivate(j, i, actual_size, N) out(C) inout([N]A, [N]B)
        {
            C[j]=0;
            long ii;
            for (ii=0; ii<actual_size; ii++)
                C[j]+= A[i+ii] * B[i+ii];
        }
        
        #pragma analysis_check assert correctness_race(acc, C[j])
        #pragma omp task label(increment) firstprivate(j) shared(acc)
        acc += C[j];
        
        j++;
    }
    
    #pragma omp taskwait
    return(acc);
}
#endif

#if 1
double dot_product_3(double A[N], double B[N])
{
    long actual_size;
    int j;
    double acc;
    double *C = malloc (N_CHUNKS*sizeof(double));

    acc=0.0;
    j=0;
    long i;
    for (i=0; i<N; i+=CHUNK_SIZE) {
        actual_size = (N-CHUNK_SIZE>=CHUNK_SIZE) ? CHUNK_SIZE : (N-CHUNK_SIZE);

        #pragma analysis_check assert correctness_race(C[j]) correctness_incoherent_out_pointed(C)
        #pragma omp task label(dot_prod) firstprivate(j, i, actual_size, N) out(C) in([N]A, [N]B)
        {
            C[j]=0;
            long ii;
            for (ii=0; ii<actual_size; ii++)
                C[j]+= A[i+ii] * B[i+ii];
        }

        #pragma analysis_check assert correctness_dead(acc) correctness_race(C[j])
        #pragma omp task label(increment) firstprivate(j) in(C[j])
        acc += C[j];

        j++;
    }

    #pragma omp taskwait
    return(acc);
}
#endif

#if 1
double dot_product_4(double A[N], double B[N])
{
    long actual_size;
    int j;
    double acc;
    double *C = malloc (N_CHUNKS*sizeof(double));

    acc=0.0;
    j=0;
    long i;
    for (i=0; i<N; i+=CHUNK_SIZE) {
        actual_size = (N-CHUNK_SIZE>=CHUNK_SIZE) ? CHUNK_SIZE : (N-CHUNK_SIZE);

        #pragma analysis_check assert correctness_race(C[j]) correctness_incoherent_p(j)
        #pragma omp task label(dot_prod) private(j) firstprivate(i, actual_size, N) out(C[j]) in([N]A, [N]B)
        {
            C[j]=0;
            long ii;
            for (ii=0; ii<actual_size; ii++)
                C[j]+= A[i+ii] * B[i+ii];
        }

        #pragma analysis_check assert correctness_race(C[j]) correctness_incoherent_in(C[j+1]) correctness_dead(acc)
        #pragma omp task label(increment) firstprivate(j) commutative(acc) in(C[j+1])
        acc += C[j];

        j++;
    }

    #pragma omp taskwait
    return(acc);
}
#endif
