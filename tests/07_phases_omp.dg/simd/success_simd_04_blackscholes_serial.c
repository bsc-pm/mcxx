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
test_generator="config/mercurium-serial-simd svml"
</testinfo>
*/


#include <math.h>
#include <malloc.h>
#include <stdlib.h>


#define LIMIT1 10.0f
#define LIMIT2 100.0f
#define LIMIT3 1.0f
#define LIMIT4 0.01f
#define LIMIT5 0.05f
#define LIMIT6 0.10f

#pragma omp simd 
float func(float X)
{
    float y, a, t;

    const float c1 =  0.319381530f;
    const float c2 = -0.356563782f;
    const float c3 =  1.781477937f;
    const float c4 = -1.821255978f;
    const float c5 =  1.330274429f;

    const float oneBySqrt2pi = 0.398942280f;

    a = fabsf(X);
    t = 1.0f / (1.0f + 0.2316419f * a);

    y = 1.0f - oneBySqrt2pi * expf(-X * X / 2.0f) *
        t * (c1 +
                t * (c2 +
                    t * (c3 +
                        t * (c4 + t * c5))));

    return (X < 0.0f) ? (1.0f - y) : y;
}

void blackscholes(
        const int width, 
        const int height,
        float* input,
        float* output1,
        float* output2)
{
    int y;

#pragma omp simd
    for (y = 0; y < (width * height * 4); ++y)
    {
        float d1, d2;
        float sig;
        float minus;
        float s = LIMIT1 * input[y] + LIMIT2 * (1.0f - input[y]);
        float k = LIMIT1 * input[y] + LIMIT2 * (1.0f - input[y]);
        float t = LIMIT3 * input[y] + LIMIT1 * (1.0f - input[y]);
        float r = LIMIT4 * input[y] + LIMIT5 * (1.0f - input[y]);
        float sig2 = LIMIT4 * input[y] + LIMIT6 * (1.0f - input[y]);

        sig = sig2 * sqrtf(t);

        d1 = (logf(s / k) + (r + sig2 * sig2 / 2.0f) * t) / sig;
        d2 = d1 - sig;

        minus = k * expf(-r * t);
        output1[y] = s * func(d1) - minus * func(d2);
        output2[y]  = minus * func(-d2) - s * func(-d1);
    }
}

void blackscholes_sc(
        const int width, 
        const int height,
        float* input,
        float* output1,
        float* output2)
{
    int y;

    for (y = 0; y < (width * height * 4); ++y)
    {
        float d, dd;
        float sig;
        float minus;
        float s = LIMIT1 * input[y] + LIMIT2 * (1.0f - input[y]);
        float t = LIMIT3 * input[y] + LIMIT1 * (1.0f - input[y]);
        float r = LIMIT4 * input[y] + LIMIT5 * (1.0f - input[y]);
        float sig2 = LIMIT4 * input[y] + LIMIT6 * (1.0f - input[y]);

        sig = sig2 * sqrtf(t);

        d = (logf(s / s) + (r + sig2 * sig2 / 2.0f) * t) / sig;
        dd = d - sig;

        minus = s * expf(-r * t);
        output1[y] = s * func(d) - minus * func(dd);
        output2[y]  = minus * func(-dd) - s * func(-d);
    }
}

int main (int argc, char* argv[])
{
    const int width = 40;
    const int height = 40;

    float* input, *output1, *output2;
    float* input_sc, *output1_sc, *output2_sc;
   
   
    if(posix_memalign((void **) &input,     64, 4 * width*height * sizeof(float)) != 0)
    {
        exit(1);
    }
    if(posix_memalign((void **) &output1, 64, 4 * width*height * sizeof(float)) != 0)
    {
        exit(1);
    }
    if(posix_memalign((void **) &output2,  64, 4 * width*height * sizeof(float)) != 0)
    {
        exit(1);
    }
    if(posix_memalign((void **) &input_sc,     64, 4 * width*height * sizeof(float)) != 0)
    {
        exit(1);
    }
    if(posix_memalign((void **) &output1_sc, 64, 4 * width*height * sizeof(float)) != 0)
    {
        exit(1);
    }
    if(posix_memalign((void **) &output2_sc,  64, 4 * width*height * sizeof(float)) != 0)
    {
        exit(1);
    }

    int i;
    for (i=0; i<(width*height*4); i++)
    {
        input[i] = (i*0.9f)/(i+1);
        input_sc[i] = (i*0.9f)/(i+1);
    }

    blackscholes(width, height, input, output1, output2);
    blackscholes_sc(width, height, input_sc, output1_sc, output2_sc);
    
 
    for (i=0; i<(width*height*4); i++)
    {
        if(fabsf(input_sc[i] - input[i]) > 0.01f)
        {
            printf("ERROR\n");
            exit(1);
        }
        if(fabsf(output1_sc[i] - output1[i]) > 0.01f)
        {
            printf("ERROR\n");
            exit(1);
        }
        if(fabsf(output2_sc[i] - output2[i]) > 0.01f)
        {
            printf("ERROR\n");
            exit(1);
        }
    }

    return 0;
}

