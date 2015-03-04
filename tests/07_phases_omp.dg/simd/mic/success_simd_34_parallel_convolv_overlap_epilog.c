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

#define         NORMAL_FACTOR 	3
#define         VECTOR_SIZE     64

int __attribute__((noinline)) doit (
              int * const __restrict__ input_image,
              int * const __restrict__ output_image,
              int * const __restrict__ filter,
        const int W, const int H, const int K, const int normal_factor)
{
    int mh;
    
    __assume_aligned(input_image, 64);
    __assume_aligned(output_image, 64);
    __assume_aligned(filter, 64);

    __assume((W%16) == 0);
    __assume((H%16) == 0);

#pragma omp for private(mh) schedule(dynamic) 
    for (mh = 0; mh < H-K; mh++)
    {				/* rows, top-to-bottom */
        int i, j, mv;
        int * const __restrict__ output_image_p = &output_image[mh*W];
        int * const __restrict__ input_image_p = &input_image[mh*W];
        //int * __restrict__ input_image_p = &input_image[mv*W];

        #pragma omp simd aligned(input_image, input_image_p, filter, output_image, output_image_p: 64) suitable (W, H) overlap(input_image_p:4,0,0)
        for (mv= 0; mv < W-K; mv++)
        {			/* columns, LEFT-to-RIGHT */
            int sum = 0;

            for (i = 0; i < K; i++)
            {
                //input_image_p = &input_image[(mv + i)*W];
                //input_image_p = input_image + (mv + i)*W + mh;
                //filter_p = &filter[i*K];
                int * const __restrict__ filter_p = &filter[i*K];

                for (j = 0; j < K; j++)
                {
                    sum += input_image_p[i*W + mv + j] * filter_p[j];
                }
            }
            
            output_image_p[mv] = (sum >> normal_factor);
        }
    }

    return 0;
}

int __attribute__((noinline)) doit_sc (
        int *input_image, int *output_image, int *filter,
        int W, int H, int K, int normal_factor)
{
    int i;
    int j;
    int mv;
    int mh;

    __assume_aligned(input_image, 64);
    __assume_aligned(output_image, 64);
    __assume_aligned(filter, 64);

    __assume((W%16) == 0);
    __assume((H%16) == 0);


#pragma omp parallel for default(none) \
    private(mv, mh, i, j) \
    firstprivate(input_image, output_image, filter, H, W, K, normal_factor) \
    schedule(static) num_threads(183)
    for (mv = 0; mv < W-K; mv++)
    {				/* rows, top-to-bottom */
        for (mh = 0; mh < H-K; mh++)
        {			/* columns, LEFT-to-RIGHT */
            int sum = 0;
            for (i = 0; i < K; i++)
            {
                for (j = 0; j < K; j++)
                {		/* FIR outer-loop... */
                    sum += input_image[(mh + i)*W + mv + j] * filter[i*K + j];
                } 
            }
            output_image[mh * W + mv] = (sum >> normal_factor);
        }
    }

    return 0;
}

int main (int argc, char **argv)
{
    struct timeval startTime, endTime;
    int k, i, j;
    int status = 0;

    const int H  = 320; // Image H
    const int W  = 32;  // Image W
    const int K  = 22;  // Filter size
    const int iters  = 1;

    int *input_image, *output_image, *filter;

    if(posix_memalign((void **) &input_image,
                VECTOR_SIZE, H*W * sizeof(int)) != 0)
    {
        exit(1);
    }
    if(posix_memalign((void **) &output_image,
                VECTOR_SIZE, H*W * sizeof(int)) != 0)
    {
        exit(1);
    }
    if(posix_memalign((void **) &filter,
                VECTOR_SIZE, K*K * sizeof(int)) != 0)
    {
        exit(1);
    }

#pragma omp parallel for private(i, j)
    for (i = 0; i < H; i++)
    {
        for (j = 0; j < W; ++j)
        {
            input_image[i * W + j] = i + j;
            output_image[i * W + j] = 0;
        }
    }


    /* Set the values of the filter matrix to a Gaussian kernel.
       This is used as a low-pass filter which blurs the image so as to
       de-emphasize the response of some isolated points to the edge
       detection (Sobel) kernels. */
    for (i = 0; i < K; i++)
    {
        for (j = 0; j < K; ++j)
        {
            filter[i*K +j] = (1 + j % 2) * (i % 2 + 1);
        }
    }

#pragma omp parallel private(k) \
    firstprivate(input_image, output_image, filter, H, W, K,iters)
    {
        for (k = 0; k < iters; k++)
        {
            doit ((int *)input_image, output_image, filter,
                    W, H, K, NORMAL_FACTOR);
        }
    }


    int *output_image_sc;

    if(posix_memalign((void **) &output_image_sc,
                VECTOR_SIZE, H*W * sizeof(int)) != 0)
    {
        exit(1);
    }

#pragma omp parallel for private(i, j)
    for (i = 0; i < H; i++)
    {
        for (j = 0; j < W; ++j)
        {
            output_image_sc[i * W + j] = 0;
        }
    }


    doit_sc(input_image, output_image_sc, filter,
            W, H, K, NORMAL_FACTOR);

    for (i = 0; i < H; i++)
    {
        for (j = 0; j < W; j++)
        {
            if (output_image_sc[i*W+j] != output_image[i*W+ j])
            {
                printf ("Bad [%d][%d]: %d instead of %d\n", 
                        i, j,
                        output_image[i * W + j],
                        output_image_sc[i * W + j]);
                status++;
            }
        }
    }
    if (status == 0)
        printf ("ok\n");
    else
        printf ("Bad output\n");

    free (output_image_sc);
    
    free (filter);
    free (input_image);
    free (output_image);

    return status;
}
