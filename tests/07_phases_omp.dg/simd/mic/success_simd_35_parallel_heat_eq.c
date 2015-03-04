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
test_CFLAGS="--only-adjacent-accesses --variable=prefetch_distance:4,1 --prefetch-in-place"
test_generator=config/mercurium-parallel-simd-mic
</testinfo>
*/


#include <stdio.h>
#include <omp.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

void init(int nx, float *A) {
  long i;
  long last = nx;
  unsigned int seed = 1;

  for(i=0;i<last;i++) {
    A[i]=(float)rand_r(&seed)/RAND_MAX;
  }
}

const float coeffs[] = {1.1f, 1.2f, 1.3f, 1.4f,
                        1.5f, 1.6f, 1.7f, 1.8f,
                        1.9f, 1.10f, 1.11f, 1.12f,
                        1.13f, 1.14f, 0.5f};


void __attribute__((noinline)) stencil_14_sc(
        float * __restrict__ A0,
        float * __restrict__ Anext,
        int nx, int tx, int timesteps)
{
#pragma omp parallel firstprivate(A0, Anext, nx, timesteps) shared(coeffs) default(none)
    {
        float *temp_ptr;
        int i, t;

        for (t = 0; t < timesteps; t++) {
            // _Pragma("omp parallel for default(shared) private(i,j,k)")
            //#pragma omp for private (k, j, i) schedule(dynamic)

            __assume_aligned(Anext, 64);
            __assume_aligned(A0, 64);
            __assume((nx-28)%16 == 0);

#pragma omp for schedule(static)
            for (i = 14; i < nx - 14; i+=1)
            {
                float tmp = coeffs[0] * (A0[i + 1] + A0[i - 1]) ;
                tmp += coeffs[1] * (A0[i + 2] + A0[i - 2]) ;
                tmp += coeffs[2] * (A0[i + 3] + A0[i - 3]) ;
                tmp += coeffs[3] * (A0[i + 4] + A0[i - 4]) ;
                tmp += coeffs[4] * (A0[i + 5] + A0[i - 5]) ;
                tmp += coeffs[5] * (A0[i + 6] + A0[i - 6]) ;
                tmp += coeffs[6] * (A0[i + 7] + A0[i - 7]) ;
                tmp += coeffs[7] * (A0[i + 8] + A0[i - 8]) ;
                tmp += coeffs[8] * (A0[i + 9] + A0[i - 9]) ;
                tmp += coeffs[9] * (A0[i + 10] + A0[i - 10]) ;
                tmp += coeffs[10] * (A0[i + 11] + A0[i - 11]) ;
                tmp += coeffs[11] * (A0[i + 12] + A0[i - 12]) ;
                tmp += coeffs[12] * (A0[i + 13] + A0[i - 13]) ;
                tmp += coeffs[13] * (A0[i + 14] + A0[i - 14]) ;
                tmp += coeffs[14] * A0[i];

                Anext[i] = tmp;
            }
            temp_ptr = A0;
            A0 = Anext;
            Anext = temp_ptr;
        }
    }
}
 
void __attribute__((noinline)) stencil_14(
        float * __restrict__ A0,
        float * __restrict__ Anext,
        int nx, int tx, int timesteps)
{

#pragma omp parallel firstprivate(A0, Anext, nx, timesteps) shared(coeffs,stderr) default(none)
    {
        float *temp_ptr;
        int i, j, t;

        const int num_threads = omp_get_num_threads();
        const int thread_id = omp_get_thread_num();
        const int simd_its = ((nx-28)/16); // nx-28 is suitable
        
        int thread_chunk = simd_its / num_threads;
        int chunk_reminder = simd_its % num_threads;

        int thread_init, thread_end;

        if ((chunk_reminder-1) >= thread_id)
        {
           thread_chunk++;
           thread_init = thread_chunk * thread_id;
           thread_end = thread_init + thread_chunk;
        }
        else
        {
           thread_init = thread_chunk * thread_id + chunk_reminder;
           thread_end = thread_init + thread_chunk;;
        }

        thread_init = thread_init * 16;
        thread_end = thread_end * 16;
        const int thread_its = thread_end - thread_init;
       
        __assume_aligned(Anext, 64);
        __assume_aligned(A0, 64);
        __assume((thread_its%16) == 0);

        for (t = 0; t < timesteps; t++)
        {
            if (thread_its > 0)
            {
                float * __restrict A0_p = &A0[thread_init];
                float * __restrict Anext_p = &Anext[thread_init];

                __assume_aligned(Anext_p, 64);
                __assume_aligned(A0_p, 64);

                __assume((thread_init%16) == 0);
                __assume((thread_end%16) == 0);
                __assume((thread_its%16) == 0);


#pragma omp simd suitable(thread_its) overlap(A0_p:4,0,0) 
                for (i = 0; i < thread_its; i++)
                {
                    float tmp = coeffs[0] * (A0_p[i + 1 ] + A0_p[i - 1]) ;
                    tmp += coeffs[1]  * (A0_p[i + 2 ] + A0_p[i - 2]) ;
                    tmp += coeffs[2]  * (A0_p[i + 3 ] + A0_p[i - 3]) ;
                    tmp += coeffs[3]  * (A0_p[i + 4 ] + A0_p[i - 4]) ;
                    tmp += coeffs[4]  * (A0_p[i + 5 ] + A0_p[i - 5]) ;
                    tmp += coeffs[5]  * (A0_p[i + 6 ] + A0_p[i - 6]) ;
                    tmp += coeffs[6]  * (A0_p[i + 7 ] + A0_p[i - 7]) ;
                    tmp += coeffs[7]  * (A0_p[i + 8 ] + A0_p[i - 8]) ;
                    tmp += coeffs[8]  * (A0_p[i + 9 ] + A0_p[i - 9]) ;
                    tmp += coeffs[9]  * (A0_p[i + 10] + A0_p[i - 10]);
                    tmp += coeffs[10] * (A0_p[i + 11] + A0_p[i - 11]);
                    tmp += coeffs[11] * (A0_p[i + 12] + A0_p[i - 12]);
                    tmp += coeffs[12] * (A0_p[i + 13] + A0_p[i - 13]);
                    tmp += coeffs[13] * (A0_p[i + 14] + A0_p[i - 14]);
                    tmp += coeffs[14] *  A0_p[i];

                    Anext_p[i] = tmp;
                }
            }

            temp_ptr = A0;
            A0 = Anext;
            Anext = temp_ptr;

#pragma omp barrier
        }
    }
}


int main(int argc,char *argv[])
{
  int i;
  int nx;
  int tx;
  int timesteps, length;
  float *Anext;
  float *A0;
  
  float spt;
  
  length = 14;

  nx = 512 + 2*length;
  timesteps = 1;

  if (((nx-28)%16) != 0) {
      printf("Error: nx-28 is not multiple of 16\n");
      return 1;
  }
 

  /* allocate arrays */ 
  if (posix_memalign((void **)&A0, 4096, nx*sizeof(float) + 2*sizeof(float)) != 0 ||
          posix_memalign((void **)&Anext, 4096, nx*sizeof(float) + 2*sizeof(float)) != 0)
  {
      fprintf(stderr, "POSIX_MEMALIGN ERROR\n");
      return 1;
  }

  A0 = A0 + 2;
  Anext = Anext + 2;

  init(nx,Anext);
  init(nx,A0);

  /* stencil function */ 
  stencil_14(A0+14, Anext+14, nx, tx, timesteps);


  float *A0_sc, *Anext_sc;
  int j, k;
  int errors = 0;
  
  /* allocate arrays */ 
  if (posix_memalign((void **)&A0_sc, 4096, nx*sizeof(float)) != 0 ||
          posix_memalign((void **)&Anext_sc, 4096, nx*sizeof(float)) != 0)
  {
      fprintf(stderr, "POSIX_MEMALIGN ERROR\n");
      return 1;
  }
 
  init(nx,Anext_sc);
  init(nx,A0_sc);
 

  /* stencil function */ 
  stencil_14_sc(A0_sc, Anext_sc, nx, 
          tx, timesteps);

#pragma omp parallel for private(k, j, i) firstprivate(Anext, Anext_sc, nx) shared(stderr, errors) schedule(static) num_threads(183) default(none)
  for (i=0; i<nx; i++)
  {
      float simd_value = Anext[i];
      float ref_value = Anext_sc[i];
      float abs = fabsf(simd_value - ref_value);

      if (isnan(simd_value) || isnan(ref_value) ||
              isnan(abs) || abs > 0.001f)
      {
          fprintf(stderr, "Error: abs(Anext[%d]=%f - Anext_sc[%d]=%f) = %f\n",
                  i,
                  Anext[i],
                  i,
                  Anext_sc[i],
                  abs);

          errors = 1;
      }
  }

  if (errors == 0)
      fprintf(stderr, "SUCCESS!\n");
  else
      fprintf(stderr, "FAIL\n");

  free(A0_sc);
  free(Anext_sc);


  /* free arrays */
  free(Anext-2);
  free(A0-2);

  return EXIT_SUCCESS;
}



