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

void matmul(double  *A, double *B, double *C, unsigned long NB)
{
    int i, j, k, I;
    double tmp;
    for (i = 0; i < NB; i++)
    {
        I=i*NB;
        for (j = 0; j < NB; j++)
        {
            tmp=C[I+j];
            for (k = 0; k < NB; k++)
            {
                tmp+=A[I+k]*B[k*NB+j];
            }
            C[I+j]=tmp;
        }
    }
}

#if 1
void compute_1(unsigned long NB, unsigned long DIM,
             double *A[DIM][DIM], double *B[DIM][DIM], double *C[DIM][DIM])
{
    unsigned i, j, k;
  
    for (i = 0; i < DIM; i++)
        for (j = 0; j < DIM; j++)
            for (k = 0; k < DIM; k++)
                #pragma analysis_check assert correctness_incoherent_in_pointed(A, B) \
                                              correctness_incoherent_out_pointed(C) \
                                              correctness_auto_storage(A, B, C)
                #pragma omp task in(A, B) out(C)
                matmul ((double *)A[i][k], (double *)B[k][j], (double *)C[i][j], NB);
}
#endif

#if 1
void compute_2(unsigned long NB, unsigned long DIM,
               double *A[DIM][DIM], double *B[DIM][DIM], double *C[DIM][DIM])
{
    unsigned i, j, k;
    
    for (i = 0; i < DIM; i++)
        for (j = 0; j < DIM; j++)
            for (k = 0; k < DIM; k++)
                #pragma analysis_check assert correctness_incoherent_out([NB*NB](A[i][k]), [NB*NB](B[k][j]))
                #pragma omp task out([NB*NB](A[i][k]), [NB*NB](B[k][j])) inout([NB*NB](C[i][j]))
                matmul ((double *)A[i][k], (double *)B[k][j], (double *)C[i][j], NB);
            
    #pragma omp taskwait
}
#endif

#if 1
void compute_3(unsigned long NB, unsigned long DIM,
               double *A[DIM][DIM], double *B[DIM][DIM], double *C[DIM][DIM])
{
    unsigned i, j, k;
    
    for (i = 0; i < DIM; i++)
        for (j = 0; j < DIM; j++)
            for (k = 0; k < DIM; k++)
                #pragma analysis_check assert correctness_incoherent_in_pointed(A[i][k], B[k][j]) \
                                              correctness_incoherent_out_pointed(C[i][j])
                #pragma omp task in(A[i][k], B[k][j]) out(C[i][j])
                matmul ((double *)A[i][k], (double *)B[k][j], (double *)C[i][j], NB);
            
    #pragma omp taskwait
}
#endif

#if 1
void compute_4(unsigned long NB, unsigned long DIM,
               double *A[DIM][DIM], double *B[DIM][DIM], double *C[DIM][DIM])
{
    unsigned i, j, k;
    
    for (i = 0; i < DIM; i++)
        for (j = 0; j < DIM; j++)
            for (k = 0; k < DIM; k++)
                #pragma analysis_check assert correctness_incoherent_out_pointed(C[i][j]) \
                                              correctness_incoherent_in_pointed(C[i][j]) \
                                              correctness_incoherent_out(A[i][k], B[k][j])
                #pragma omp task out(A[i][k], B[k][j]) inout(C[i][j])
                matmul ((double *)A[i][k], (double *)B[k][j], (double *)C[i][j], NB);
            
    #pragma omp taskwait
}
#endif

#if 1
void compute_5(unsigned long NB, unsigned long DIM,
               double *A[DIM][DIM], double *B[DIM][DIM], double *C[DIM][DIM])
{
    unsigned i, j, k;
    
    for (i = 0; i < DIM; i++)
        for (j = 0; j < DIM; j++)
            for (k = 0; k < DIM; k++)
                #pragma analysis_check assert correctness_incoherent_in_pointed(A[i], B[k], C[i]) \
                                              correctness_incoherent_out_pointed(C[i])
                #pragma omp task in(A[i], B[k]) inout(C[i])
                matmul ((double *)A[i][k], (double *)B[k][j], (double *)C[i][j], NB);
            
    #pragma omp taskwait
}
#endif