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

#if !defined(__ICC) || (__ICC >= 1400)

#include <stdio.h>
#include <math.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <omp.h>

typedef struct
{
  struct timeval start;
  struct timeval end;
  double elapsed_time;
} timing_t;

void timing_start(timing_t* t)
{
    memset(t, 0, sizeof(*t));
    
    gettimeofday(&(t->start), NULL);
}

void timing_end(timing_t* t)
{
    gettimeofday(&(t->end), NULL);

    double start_value = t->start.tv_sec*1e6 + t->start.tv_usec;
    double end_value = t->end.tv_sec*1e6 + t->end.tv_usec;

    double diff_value = end_value - start_value;

    t->elapsed_time = diff_value / 1e6;
}

double timing_elapsed(const timing_t* t)
{
    return (t->elapsed_time);
}

void matmul_block(int N, int BS, float *a, float *b, float *c)
{
    // Assumption: BS evenly divides N
    int i, j, k;
    for (i = 0; i < BS; i++)
    {
        for (j = 0; j < BS; j++)
        {
            for (k = 0; k < BS; k++)
            {
                c[N*i + j] += a[N*i + k] * b[N*k + j];
            }
        }
    }
}

void matmul(int N, int BS, float a[N][N], float b[N][N])
{
   int i, j, k;
   float (*c1)[N] = calloc(sizeof(float), N * N);

   fprintf(stderr, "Parallel with %d threads\n", omp_get_max_threads());
   timing_t parallel_time;
   timing_start(&parallel_time);
   for (i = 0; i < N; i+=BS) {
      for (j = 0; j < N; j+=BS) {
         for (k = 0; k < N; k+=BS) {
#pragma omp task depend ( in: a[i:BS][k:BS], b[k:BS][j:BS] ) \
                 depend ( inout: c1[i:BS][j:BS] )
             {
                 matmul_block(N, BS, &a[i][k], &b[k][j], &c1[i][j]);
             }
         }
      }
   }
#pragma omp taskwait
   timing_end(&parallel_time);
   fprintf(stderr, "Parallel ended. %.2f sec\n", timing_elapsed(&parallel_time));

   float (*c2)[N] = calloc(sizeof(float), N * N);
   // Serial
   timing_t serial_time;
   timing_start(&serial_time);
   fprintf(stderr, "Serial\n");
   for (i = 0; i < N; i++) {
      for (j = 0; j < N; j++) {
         for (k = 0; k < N; k++) {
                c2[i][j] += a[i][k] * b[k][j];
         }
      }
   }
   timing_end(&serial_time);

   fprintf(stderr, "Serial ended. %.2f sec\n", timing_elapsed(&serial_time));

   // Check
   fprintf(stderr, "Check\n");
   for (i = 0; i < N; i++) {
      for (j = 0; j < N; j++) {
          if (fabsf(c1[i][j] - c2[i][j]) > 1e-5)
          {
              fprintf(stderr, "Check failure. Error at [%d][%d] %.5f != %.5f\n", i, j, c1[i][j], c2[i][j]);
              abort();
          }
      }
   }
   fprintf(stderr, "Check is OK\n");

   fprintf(stderr, "Speedup is %.2f\n", timing_elapsed(&serial_time) / timing_elapsed(&parallel_time));

   free(c1);
   free(c2);
}

#define SIZE 1024
#define BLOCK_SIZE 64

float a[SIZE][SIZE];
float b[SIZE][SIZE];

float rand_FloatRange(float a, float b)
{
    return ((b-a)*((float)random()/RAND_MAX))+a;
}

int main(int argc, char* argv[])
{
    srand(clock());

    // Random fill the matrices
    int i, j;
    for (i = 0; i < SIZE; i++)
    {
        for (j = 0; j < SIZE; j++)
        {
            a[i][j] = rand_FloatRange(-1e9, 1e9);
            b[i][j] = rand_FloatRange(-1e9, 1e9);
        }
    }

#pragma omp parallel default(shared)
    {
#pragma omp single
        {
            matmul(SIZE, BLOCK_SIZE, a, b);
        }
    }
    return 0;
}

#else
// ICC <14.0 miscompiles VLA address calculations

int main(int argc, char *argv[])
{
    return 0;
}

#endif
