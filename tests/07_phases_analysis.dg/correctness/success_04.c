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

#define N 2     /* size of problem space (x, y from -N to N) */

/* Structure definition for complex numbers */
typedef struct {
    double real, imag;
} complex;

#if 1
void mandelbrot1(int height,
                int width,
                double real_min,
                double imag_min,
                double scale_real,
                double scale_imag,
                int maxiter, 
                int ** output)
{
    /* Calculate points and save */
    int col;
    for (int row = 0; row < height; ++row)
    {
        #pragma analysis_check assert correctness_race() correctness_incoherent_fp(col)
        #pragma omp task
        for (col = 0; col < width; ++col) 
        {
            complex z, c;
            z.real = z.imag = 0;

            /* Scale display coordinates to actual region  */
            c.real = real_min + ((double) col * scale_real);

            /* Calculate z0, z1, .... until divergence or maximum iterations */
            int k = 0;
            double lengthsq, temp;
            do  {
                temp = z.real*z.real - z.imag*z.imag + c.real;
                z.imag = 2*z.real*z.imag + c.imag;
                z.real = temp;
                lengthsq = z.real*z.real + z.imag*z.imag;
                ++k;
            } while (lengthsq < (N*N) && k < maxiter);

            output[row][col]=k;
        }
    }
}
#endif

int row2, col2; // variables used to traverse the problem space

#if 1
void mandelbrot2(int height,
                int width,
                double real_min,
                double imag_min,
                double scale_real,
                double scale_imag,
                int maxiter,
                int ** output)
{
    complex z, c;
    for (row2 = 0; row2 < height; ++row2) {
        #pragma analysis_check assert correctness_race() correctness_incoherent_fp(col2, z, c)
        #pragma omp task firstprivate(row2) firstprivate(col2)
        for (col2 = 0; col2 < width; ++col2) {

            z.real = z.imag = 0;

            /* Scale display coordinates to actual region  */
            c.real = real_min + ((double) col2 * scale_real);
            c.imag = imag_min + ((double) (height-1-row2) * scale_imag);
            int k = 0;
            double lengthsq, temp;
            do  {
                temp = z.real*z.real - z.imag*z.imag + c.real;
                z.imag = 2*z.real*z.imag + c.imag;
                z.real = temp;
                lengthsq = z.real*z.real + z.imag*z.imag;
                ++k;
            } while (lengthsq < (N*N) && k < maxiter);

            output[row2][col2]=k;
        }
    }
}
#endif

#if 1
void mandelbrot3(int height,
                int width,
                double real_min,
                double imag_min,
                double scale_real,
                double scale_imag,
                int maxiter,
                int ** output)

{
    complex z, c;
    for (row2 = 0; row2 < height; ++row2) {
        #pragma analysis_check assert correctness_race(row2, col2) correctness_incoherent_fp(z, c)
        #pragma omp task
        for (col2 = 0; col2 < width; ++col2) {

            z.real = z.imag = 0;

            /* Scale display coordinates to actual region  */
            c.real = real_min + ((double) col2 * scale_real);
            c.imag = imag_min + ((double) (height-1-row2) * scale_imag);
            int k = 0;
            double lengthsq, temp;
            do  {
                temp = z.real*z.real - z.imag*z.imag + c.real;
                z.imag = 2*z.real*z.imag + c.imag;
                z.real = temp;
                lengthsq = z.real*z.real + z.imag*z.imag;
                ++k;
            } while (lengthsq < (N*N) && k < maxiter);

            output[row2][col2]=k;
        }
    }
}
#endif

#if 1
void mandelbrot4(int height,
                 int width,
                 double real_min,
                 double imag_min,
                 double scale_real,
                 double scale_imag,
                 int maxiter,
                 int ** output)

{
    complex z, c;
    #pragma omp parallel private(row2)
    for (row2 = 0; row2 < height; ++row2) {
        #pragma analysis_check assert correctness_race(row2, col2, z, c)
        #pragma omp task
        for (col2 = 0; col2 < width; ++col2) {

            z.real = z.imag = 0;

            /* Scale display coordinates to actual region  */
            c.real = real_min + ((double) col2 * scale_real);
            c.imag = imag_min + ((double) (height-1-row2) * scale_imag);
            int k = 0;
            double lengthsq, temp;
            do  {
                temp = z.real*z.real - z.imag*z.imag + c.real;
                z.imag = 2*z.real*z.imag + c.imag;
                z.real = temp;
                lengthsq = z.real*z.real + z.imag*z.imag;
                ++k;
            } while (lengthsq < (N*N) && k < maxiter);

            output[row2][col2]=k;
        }
    }
}
#endif
