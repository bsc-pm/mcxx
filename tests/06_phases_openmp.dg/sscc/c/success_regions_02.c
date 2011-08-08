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
test_generator=config/mercurium-ss2omp
</testinfo>
*/

// This is needed for posix_memalign
#define _POSIX_C_SOURCE 200112L


#include <complex.h>
#ifdef I
#undef I
#endif

#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef M_PI 
#define M_PI 3.14159265358979323846 
#endif

#define SIGMA -1.0
#define SIGMA2PI (SIGMA)*M_PI*2.0


//
// FXT code that implements an FFT that we use in tasks instead of FFTW
//

// The following functions enclosed between "BEGIN FXT" and "END FXT" are derived from the FXT library which has the following copyright:
// // This file is part of the FXT library.
// // Copyright (C) 2010 Joerg Arndt
// // License: GNU General Public License version 3 or later,
// // see the file COPYING.txt in the main directory.


// BEGIN FXT
#define swap2(x, y) {double _Complex t = x; x = y; y = t; }

static inline void revbin_permute_4(double _Complex *f) {
	// unrolled version for length 4
	swap2(f[1], f[2]);
}  // # of swaps = 1

static inline void revbin_permute_8(double _Complex *f) {
	// unrolled version for length 8
	swap2(f[1], f[4]);
	swap2(f[3], f[6]);
}  // # of swaps = 2

static inline void revbin_permute_16(double _Complex *f) {
	// unrolled version for length 16
	swap2(f[1], f[8]);
	swap2(f[2], f[4]);
	swap2(f[3], f[12]);
	swap2(f[5], f[10]);
	swap2(f[7], f[14]);
	swap2(f[11], f[13]);
}  // # of swaps = 6

static inline void revbin_permute_32(double _Complex *f) {
	// unrolled version for length 32
	swap2(f[1], f[16]);
	swap2(f[2], f[8]);
	swap2(f[3], f[24]);
	swap2(f[5], f[20]);
	swap2(f[6], f[12]);
	swap2(f[7], f[28]);
	swap2(f[9], f[18]);
	swap2(f[11], f[26]);
	swap2(f[13], f[22]);
	swap2(f[15], f[30]);
	swap2(f[19], f[25]);
	swap2(f[23], f[29]);
}  // # of swaps = 12

static inline void revbin_permute_64(double _Complex *f) {
	// unrolled version for length 64
	swap2(f[1], f[32]);
	swap2(f[2], f[16]);
	swap2(f[3], f[48]);
	swap2(f[4], f[8]);
	swap2(f[5], f[40]);
	swap2(f[6], f[24]);
	swap2(f[7], f[56]);
	swap2(f[9], f[36]);
	swap2(f[10], f[20]);
	swap2(f[11], f[52]);
	swap2(f[13], f[44]);
	swap2(f[14], f[28]);
	swap2(f[15], f[60]);
	swap2(f[17], f[34]);
	swap2(f[19], f[50]);
	swap2(f[21], f[42]);
	swap2(f[22], f[26]);
	swap2(f[23], f[58]);
	swap2(f[25], f[38]);
	swap2(f[27], f[54]);
	swap2(f[29], f[46]);
	swap2(f[31], f[62]);
	swap2(f[35], f[49]);
	swap2(f[37], f[41]);
	swap2(f[39], f[57]);
	swap2(f[43], f[53]);
	swap2(f[47], f[61]);
	swap2(f[55], f[59]);
}  // # of swaps = 28

static inline void revbin_permute_leq_64(double _Complex *f, unsigned long n) {
	switch ( n ) {
//    case 1: break;
//    case 2: break;
		case 4: revbin_permute_4(f); break;
		case 8: revbin_permute_8(f); break;
		case 16: revbin_permute_16(f); break;
		case 32: revbin_permute_32(f); break;
		case 64: revbin_permute_64(f); break;
		default: ;  // ouch...
	}
}

static inline unsigned long revbin_0(unsigned long x) {
	// Return x with reversed bit order.
	// Alternative version, dynamically generated masks:
	unsigned long s = (8UL * sizeof(unsigned long)) >> 1;
	unsigned long m = ~0UL >> s;
	while ( s ) {
		x = ( (x & m) << s ) ^ ( (x & (~m)) >> s );
		s >>= 1;
		m ^= (m<<s);
	}
	return  x;
}

static inline unsigned long revbin(unsigned long x, unsigned long ldn) {
	// Return word with the ldn least significant bits
	//   (i.e. bit_0 ... bit_{ldn-1})  of x reversed,
	//   the other bits are set to zero.
	return revbin_0(x) >> ( 8L * sizeof(unsigned long) - ldn );
}

static inline void idx_swap(double _Complex *f, unsigned long k, unsigned long r) {
	double _Complex tmp = f[k];
	f[k] = f[r];
	f[r] = tmp;
}

static inline unsigned long ld(unsigned long x) {
	// Return floor(log2(x)),
	// i.e. return k so that 2^k <= x < 2^(k+1)
	// If x==0, then 0 is returned (!)
	unsigned long k = 0;
	while ( x >>= 1 ) {
		++k;
	}
	return k;
}

void revbin_permute(double _Complex *f, unsigned long n) {
	if ( n<=64 ) {
		revbin_permute_leq_64(f, n);
		return;
	}
	
	const unsigned long ldn = ld(n);
	const unsigned long nh = (n>>1);
	
	const unsigned long n1  = n - 1;    // = 11111111
	const unsigned long nx1 = nh - 2;   // = 01111110
	const unsigned long nx2 = n1 - nx1; // = 10111101
	
	unsigned long k = 0,  r = 0;
	while ( k < (n/4) ) {
		// n>=16, n/2>=8, n/4>=4
		// ----- k%4 == 0:
		if ( r>k ) {
			idx_swap(f, k, r);  // <nh, <nh 11
			idx_swap(f, n1^k, n1^r);  // >nh, >nh 00
			idx_swap(f, nx1^k, nx1^r);  // <nh, <nh 11
			idx_swap(f, nx2^k, nx2^r);  // >nh, >nh 00
		}
		
		++k;
		r ^= nh;
		
		// ----- k%4 == 1:
		if ( r>k ) {
			idx_swap(f, k, r);  // <nh, >nh 10
			idx_swap(f, n1^k, n1^r);  // >nh, <nh 01
		}
		
		++k;
		r = revbin(k, ldn);
		
		// ----- k%4 == 2:
		if ( r>k ) {
			idx_swap(f, k, r);  // <nh, <nh 11
			idx_swap(f, n1^k, n1^r); // >nh, >nh 00
		}
		
		++k;
		r ^= nh;
		
		// ----- k%4 == 3:
		if ( r>k ) {
			idx_swap(f, k, r);    // <nh, >nh 10
			idx_swap(f, nx1^k, nx1^r);   // <nh, >nh 10
		}
		
		++k;
		r = revbin(k, ldn);
	}
}

static inline void fft_dit2(double _Complex *f, unsigned long ldn, int is) {
	// Decimation in time (DIT) radix-2 FFT
	const unsigned long n = 1UL << ldn;
	const double pi = is * M_PI;
	
	revbin_permute(f, n);
    unsigned long ldm, j, r;
	for (ldm=1; ldm <= ldn; ++ldm) {
		const unsigned long m = (1UL << ldm);
		const unsigned long mh = (m >> 1);
		const double phi = pi / (double)mh;
		for (j=0; j < mh; ++j) {
			double _Complex w = sin( phi * (double)j ) + _Complex_I * cos( phi * (double)j );
			for (r=0; r < n; r += m) {
				unsigned long i0 = r + j;
				unsigned long i1 = i0 + mh;
				
				double _Complex u = f[i0];
				double _Complex v = f[i1] * w;
				
				f[i0] = u + v;
				f[i1] = u - v;
			}
		}
	}
}

#undef swap2

// END FXT


//
// FFTW compatibility functions and data structures
//
#define FFTW_FORWARD 1
#define FFTW_BACKWARD -1

typedef enum {
	FFTW_ESTIMATE=0,
	FFTW_MEASURE,
	FFTW_PATIENT
} fftw_planning_patience;

typedef struct {
	int sign;
	unsigned long ldn;
} fftw_plan;


static fftw_plan fftw_plan_dft_1d(size_t N, void *data_in, void *data_out, int direction,  fftw_planning_patience patience) {
	unsigned long ldn = 1;
	
	while ((1UL << ldn) < N) {
		ldn++;
	}
	fftw_plan result = {direction, ldn};
	return result;
}

static inline void fftw_execute_dft(fftw_plan plan, double _Complex *data_in, double _Complex *data_out) {
	assert(data_in == data_out);
	fft_dit2(data_in, plan.ldn, plan.sign);
}


static void fftw_import_wisdom_from_file(FILE *file) {
}

static void fftw_export_wisdom_to_file(FILE *file) {
}

static void *fftw_malloc(size_t size) {
	void *result;
	
	posix_memalign(&result, size, size);
	return result;
}

static void fftw_free(void *data) {
	free(data);
}


//
// Transposition code
//

// Micro block size
#ifndef mBS
#define mBS 4
#endif

#ifndef tw_mBS
#define tw_mBS 2
#endif

#ifndef middleBS
#define middleBS 8
#endif

static inline __attribute__((always_inline)) void micro_trsp_blk(long N, long N_SQRT, double _Complex (*data)[N_SQRT])
{
#if mBS == 1
#elif mBS == 4
	register double _Complex a00, a01, a02, a03;
	register double _Complex a10, a11, a12, a13;
	register double _Complex a20, a21, a22, a23;
	register double _Complex a30, a31, a32, a33;
	
	a00 = data[0][0]; a01 = data[0][1]; a02 = data[0][2]; a03 = data[0][3];
	a10 = data[1][0]; a11 = data[1][1]; a12 = data[1][2]; a13 = data[1][3];
	a20 = data[2][0]; a21 = data[2][1]; a22 = data[2][2]; a23 = data[2][3];
	a30 = data[3][0]; a31 = data[3][1]; a32 = data[3][2]; a33 = data[3][3];
	
	data[0][0] = a00; data[0][1] = a10; data[0][2] = a20; data[0][3] = a30;
	data[1][0] = a01; data[1][1] = a11; data[1][2] = a21; data[1][3] = a31;
	data[2][0] = a02; data[2][1] = a12; data[2][2] = a22; data[2][3] = a32;
	data[3][0] = a03; data[3][1] = a13; data[3][2] = a23; data[3][3] = a33;
#else
#error Unimplemented micro block size
#endif
}

static inline __attribute__((always_inline)) void micro_trsp_swap(long N, long N_SQRT, double _Complex (*data1)[N_SQRT], double _Complex (*data2)[N_SQRT])
{
#if mBS == 1
	register double _Complex a00 = data1[0][0];
	register double _Complex b00 = data2[0][0];
	data1[0][0] = b00;
	data2[0][0] = a00;
#elif mBS == 4
	register double _Complex a00, a01, a02, a03;
	register double _Complex a10, a11, a12, a13;
	register double _Complex a20, a21, a22, a23;
	register double _Complex a30, a31, a32, a33;
	
	register double _Complex b00, b01, b02, b03;
	register double _Complex b10, b11, b12, b13;
	register double _Complex b20, b21, b22, b23;
	register double _Complex b30, b31, b32, b33;
	
	a00 = data1[0][0]; a01 = data1[0][1]; a02 = data1[0][2]; a03 = data1[0][3];
	a10 = data1[1][0]; a11 = data1[1][1]; a12 = data1[1][2]; a13 = data1[1][3];
	a20 = data1[2][0]; a21 = data1[2][1]; a22 = data1[2][2]; a23 = data1[2][3];
	a30 = data1[3][0]; a31 = data1[3][1]; a32 = data1[3][2]; a33 = data1[3][3];
	
	b00 = data2[0][0]; b01 = data2[0][1]; b02 = data2[0][2]; b03 = data2[0][3];
	b10 = data2[1][0]; b11 = data2[1][1]; b12 = data2[1][2]; b13 = data2[1][3];
	b20 = data2[2][0]; b21 = data2[2][1]; b22 = data2[2][2]; b23 = data2[2][3];
	b30 = data2[3][0]; b31 = data2[3][1]; b32 = data2[3][2]; b33 = data2[3][3];
	
	data1[0][0] = b00; data1[0][1] = b10; data1[0][2] = b20; data1[0][3] = b30;
	data1[1][0] = b01; data1[1][1] = b11; data1[1][2] = b21; data1[1][3] = b31;
	data1[2][0] = b02; data1[2][1] = b12; data1[2][2] = b22; data1[2][3] = b32;
	data1[3][0] = b03; data1[3][1] = b13; data1[3][2] = b23; data1[3][3] = b33;
	
	data2[0][0] = a00; data2[0][1] = a10; data2[0][2] = a20; data2[0][3] = a30;
	data2[1][0] = a01; data2[1][1] = a11; data2[1][2] = a21; data2[1][3] = a31;
	data2[2][0] = a02; data2[2][1] = a12; data2[2][2] = a22; data2[2][3] = a32;
	data2[3][0] = a03; data2[3][1] = a13; data2[3][2] = a23; data2[3][3] = a33;
#else
#error Unimplemented micro block size
#endif
}

static inline __attribute__((always_inline)) void micro_tw_trsp_blk(long N, long N_SQRT, long I, double _Complex (*data)[N_SQRT])
{
#if tw_mBS == 1
	register double _Complex w00 = cos(SIGMA2PI*I*I/N) + _Complex_I * sin(SIGMA2PI*I*I/N);
	register double _Complex a00 = data[0][0] * w00;
	data[0][0] = a00;
#elif tw_mBS == 2
	register double _Complex a00, a01;
	register double _Complex a10, a11;
	
	register double _Complex w00;
	register double _Complex w10, w11;
	
	w00 = cexp(_Complex_I * SIGMA2PI*I*I/N);
	w10 = cexp(_Complex_I * SIGMA2PI*(I+1.0)*I/N);
	w11 = cexp(_Complex_I * SIGMA2PI*(I+1.0)*(I+1.0)/N);
	
	a00 = data[0][0] * w00; a01 = data[0][1] * w10;
	a10 = data[1][0] * w10; a11 = data[1][1] * w11;
	
	data[0][0] = a00; data[0][1] = a10;
	data[1][0] = a01; data[1][1] = a11;
#else
#error Unimplemented micro block size
#endif
}

static inline __attribute__((always_inline)) void micro_tw_trsp_swap(long N, long N_SQRT, long I, long J, double _Complex (*data1)[N_SQRT], double _Complex (*data2)[N_SQRT])
{
#if tw_mBS == 1
	register double _Complex w00 = cos(SIGMA2PI*I*J/N) + _Complex_I * sin(SIGMA2PI*I*J/N);
	register double _Complex a00 = data1[0][0] * w00;
	register double _Complex b00 = data2[0][0] * w00;
	data1[0][0] = b00;
	data2[0][0] = a00;
#elif tw_mBS == 2
	register double _Complex a00, a01;
	register double _Complex a10, a11;
	
	register double _Complex b00, b01;
	register double _Complex b10, b11;
	
	register double _Complex w00, w01;
	register double _Complex w10, w11;
	
	w00 = cexp(_Complex_I * SIGMA2PI*I*J/N);
	w01 = cexp(_Complex_I * SIGMA2PI*I*(J+1.0)/N);
	w10 = cexp(_Complex_I * SIGMA2PI*(I+1.0)*J/N);
	w11 = cexp(_Complex_I * SIGMA2PI*(I+1.0)*(J+1.0)/N);
	
	a00 = data1[0][0] * w00; a01 = data1[0][1] * w01;
	a10 = data1[1][0] * w10; a11 = data1[1][1] * w11;
	
	b00 = data2[0][0] * w00; b01 = data2[0][1] * w10;
	b10 = data2[1][0] * w01; b11 = data2[1][1] * w11;
	
	data1[0][0] = b00; data1[0][1] = b10;
	data1[1][0] = b01; data1[1][1] = b11;
	
	data2[0][0] = a00; data2[0][1] = a10;
	data2[1][0] = a01; data2[1][1] = a11;
#else
#error Unimplemented micro block size
#endif
}

static void middle_trsp_blk(long N, long N_SQRT, double _Complex data[N_SQRT][N_SQRT]) {
    long i, j;
	for (i=0; i < middleBS; i+=mBS) {
		for (j=0; j < i; j+=mBS) {
			micro_trsp_swap(N, N_SQRT, (double _Complex (*)[N_SQRT]) &data[i][j], (double _Complex (*)[N_SQRT]) &data[j][i]);
		}
		micro_trsp_blk(N, N_SQRT, (double _Complex (*)[N_SQRT]) &data[i][i]);
	}
}

static void middle_trsp_swap (long N, long N_SQRT, double _Complex data1[N_SQRT][N_SQRT], double _Complex data2[N_SQRT][N_SQRT]) {
    long i, j;
    for (i=0; i < middleBS; i+=mBS) {
		for (j=0; j < middleBS; j+=mBS) {
			micro_trsp_swap(N, N_SQRT, (double _Complex (*)[N_SQRT]) &data1[i][j], (double _Complex (*)[N_SQRT]) &data2[j][i]);
		}
	}
}

static void middle_tw_trsp_blk(long N, long N_SQRT, long I, double _Complex data[N_SQRT][N_SQRT]) {
    long i, j;
    for (i=0; i < middleBS; i+=tw_mBS) {
		for (j=0; j < i; j+=tw_mBS) {
			micro_tw_trsp_swap(N, N_SQRT, I+i, I+j, (double _Complex (*)[N_SQRT]) &data[i][j], (double _Complex (*)[N_SQRT]) &data[j][i]);
		}
		micro_tw_trsp_blk(N, N_SQRT, I+i, (double _Complex (*)[N_SQRT]) &data[i][i]);
	}
}

static void middle_tw_trsp_swap (long N, long N_SQRT, long I, long J, double _Complex data1[N_SQRT][N_SQRT], double _Complex data2[N_SQRT][N_SQRT]) {
    long i, j;	
    for (i=0; i < middleBS; i+=tw_mBS) {
		for (j=0; j < middleBS; j+=tw_mBS) {
			micro_tw_trsp_swap(N, N_SQRT, I+i, J+j, (double _Complex (*)[N_SQRT]) &data1[i][j], (double _Complex (*)[N_SQRT]) &data2[j][i]);
		}
	}
}


//
// Aux code
//

fftw_plan plan, reference_plan;

double _Complex * Alloc_FFT_Matrix (long N) {
	double _Complex *ptr;
	
	ptr = fftw_malloc (N*sizeof(double _Complex));
	if (ptr == NULL) {
		fprintf (stderr, "Cannot obtain aligned memory buffer. Dying...\n");
		exit (1);
	}
	return ptr;
}


#pragma css task input(N_SQRT, FFT_BS, I) output(A)
void zz_initializeBlock(long N_SQRT, long FFT_BS, long I, double _Complex A[FFT_BS][N_SQRT]) {
    long i, j;    
	for (i=0; i < FFT_BS; i++) {
		for (j=0; j < N_SQRT; j++) {
			A[i][j] = I+i+j;
		}
	}
}

void initialize(long N_SQRT, long FFT_BS, double _Complex A[N_SQRT][N_SQRT]) {
    long i;    
	for (i = 0; i < N_SQRT; i+=FFT_BS) {
		zz_initializeBlock(N_SQRT, FFT_BS, i, &A[i]);
	}
}


//
// Tasks
//

#pragma css task input(N, N_SQRT, TR_BS) inout(data{0:TR_BS}{0:TR_BS})
void trsp_blk(long N, long N_SQRT, long TR_BS, double _Complex data[N_SQRT][N_SQRT]) {
    long i, j;    
	for (i=0; i < TR_BS; i+=middleBS) {
		for (j=0; j < i; j+=middleBS) {
			middle_trsp_swap(N, N_SQRT, (double _Complex (*) [N_SQRT]) &data[i][j], (double _Complex (*) [N_SQRT]) &data[j][i]);
		}
		middle_trsp_blk(N, N_SQRT, (double _Complex (*) [N_SQRT]) &data[i][i]);
	}
}

#pragma css task input(N, N_SQRT, TR_BS) inout(data1{0:TR_BS}{0:TR_BS}, data2{0:TR_BS}{0:TR_BS})
void trsp_swap (long N, long N_SQRT, long TR_BS, double _Complex data1[N_SQRT][N_SQRT], double _Complex data2[N_SQRT][N_SQRT]) {
    long i, j;
    for (i=0; i < TR_BS; i+=middleBS) {
		for (j=0; j < TR_BS; j+=middleBS) {
			middle_trsp_swap(N, N_SQRT, (double _Complex (*) [N_SQRT]) &data1[i][j], (double _Complex (*) [N_SQRT]) &data2[j][i]);
		}
	}
}

#pragma css task input(N, N_SQRT, TR_BS, I) inout(data{0:TR_BS}{0:TR_BS})
void tw_trsp_blk(long N, long N_SQRT, long TR_BS, long I, double _Complex data[N_SQRT][N_SQRT]) {
    long i, j;	
    for (i=0; i < TR_BS; i+=middleBS) {
		for (j=0; j < i; j+=middleBS) {
			middle_tw_trsp_swap(N, N_SQRT, I+i, I+j, (double _Complex (*) [N_SQRT]) &data[i][j], 
                                (double _Complex (*) [N_SQRT]) &data[j][i]);
		}
		middle_tw_trsp_blk(N, N_SQRT, I+i, (double _Complex (*) [N_SQRT]) &data[i][i]);
	}
}

#pragma css task input (N, N_SQRT, TR_BS, I, J) inout(data1{0:TR_BS}{0:TR_BS}, data2{0:TR_BS}{0:TR_BS})
void tw_trsp_swap (long N, long N_SQRT, long TR_BS, long I, long J, double _Complex data1[N_SQRT][N_SQRT], 
                   double _Complex data2[N_SQRT][N_SQRT]) {
    long i, j;
    for (i=0; i < TR_BS; i+=middleBS) {
		for (j=0; j < TR_BS; j+=middleBS) {
			middle_tw_trsp_swap(N, N_SQRT, I+i, J+j, (double _Complex (*) [N_SQRT]) &data1[i][j], 
                                (double _Complex (*) [N_SQRT]) &data2[j][i]);
		}
	}
}

#pragma css task input(N_SQRT, FFT_BS) inout(data{0:FFT_BS}{}) highpriority
static void FFT1D (long N_SQRT, long FFT_BS, double _Complex data[N_SQRT][N_SQRT]) {
	long i;
    for (i=0; i < FFT_BS; i++) {
		fftw_execute_dft(plan, (double _Complex *) &data[i][0], (double _Complex *) &data[i][0]);
	}
}

#pragma css task input(N_SQRT, FFT_BS) inout(data{0:FFT_BS}{}) highpriority
static void FFT1D_2 (long N_SQRT, long FFT_BS, double _Complex data[N_SQRT][N_SQRT]) {
	long i;
    for (i=0; i < FFT_BS; i++) {
		fftw_execute_dft(plan, (double _Complex *) &data[i][0], (double _Complex *) &data[i][0]);
	}
}


//
// Main code
//
void FFT_1D (long N, long N_SQRT, long FFT_BS, long TR_BS, double _Complex A[N_SQRT][N_SQRT]) {
	// Transpose
	long I, J;
	for (I=0; I<N_SQRT; I+=TR_BS) {
		trsp_blk (N, N_SQRT, TR_BS, (double _Complex (*) [N_SQRT]) &A[I][I]);
		for (J=I+TR_BS; J<N_SQRT; J+=TR_BS)
			trsp_swap (N, N_SQRT, TR_BS, (double _Complex (*) [N_SQRT]) &A[I][J],(double _Complex (*) [N_SQRT])  &A[J][I]);

	}
	
	// First FFT round
	for (J=0; J<N_SQRT; J+=FFT_BS)
		FFT1D(N_SQRT, FFT_BS, (double _Complex (*) [N_SQRT]) &A[J][0]);
	
	// Twiddle and Transpose
	for (I=0; I<N_SQRT; I+=TR_BS) {
		tw_trsp_blk (N, N_SQRT, TR_BS, I, (double _Complex (*) [N_SQRT]) &A[I][I]);
		for (J=I+TR_BS; J<N_SQRT; J+=TR_BS)
			tw_trsp_swap (N, N_SQRT, TR_BS, I, J, (double _Complex (*) [N_SQRT]) &A[I][J], (double _Complex (*) [N_SQRT]) &A[J][I]);
	}
	
	// Second FFT round
	for (J=0; J<N_SQRT; J+=FFT_BS)
		FFT1D_2(N_SQRT, FFT_BS, (double _Complex (*) [N_SQRT]) &A[J][0]);
	
	// Transpose
	for (I=0; I<N_SQRT; I+=TR_BS) {
		trsp_blk (N, N_SQRT, TR_BS, (double _Complex (*) [N_SQRT]) &A[I][I]);
		for (J=I+TR_BS; J<N_SQRT; J+=TR_BS)
			trsp_swap (N, N_SQRT, TR_BS, (double _Complex (*) [N_SQRT]) &A[I][J], (double _Complex (*) [N_SQRT]) &A[J][I]);
	}
}

static void setup_fftw_plan(long N_SQRT) {
	FILE *file;
	
	file = fopen("fftw.wisdom", "r");
	if (file != NULL) {
		fftw_import_wisdom_from_file(file);
		fclose(file);
	}
	
	// Prepare a plan for a row
	double _Complex *data = fftw_malloc(sizeof(double _Complex) * N_SQRT);
	
	// Train the FFTW engine
	plan = fftw_plan_dft_1d(N_SQRT, data, data, FFTW_FORWARD, FFTW_PATIENT);
	
	fftw_free(data);
	
	// Save the wisdom
	file = fopen("fftw.wisdom", "w");
	fftw_export_wisdom_to_file(file);
	fclose(file);
}

static void setup_fftw_reference_plan(long N) {
	FILE *file;
	
	file = fopen("fftw.wisdom", "r");
	if (file != NULL) {
		fftw_import_wisdom_from_file(file);
		fclose(file);
	}
	
	// Prepare a plan for a row
	double _Complex *data = fftw_malloc(sizeof(double _Complex) * N);
	
	// Train the FFTW engine
	reference_plan = fftw_plan_dft_1d(N, data, data, FFTW_FORWARD, FFTW_ESTIMATE);
	
	fftw_free(data);
	
	// Save the wisdom
	file = fopen("fftw.wisdom", "w");
	fftw_export_wisdom_to_file(file);
	fclose(file);
}

int main(int argc, char **argv)
{
	double _Complex *A __attribute__ ((aligned (128)));
	double _Complex *reference __attribute__ ((aligned (128)));
	
	long N_SQRT = 1024L;
//	long N_SQRT = 256L;
	long N = N_SQRT * N_SQRT;
	long FFT_BS = N_SQRT / 128;
	long TR_BS = FFT_BS * 4;
	
	if (FFT_BS > N_SQRT) {
		fprintf(stderr, "Error: either the FFT block size must be <= %li or elements >= %li\n", N_SQRT, FFT_BS*FFT_BS);
		return 1;
	}
	if (TR_BS > N_SQRT) {
		fprintf(stderr, "Error: either the transpose block size must be <= %li or elements >= %li\n", N_SQRT, TR_BS*TR_BS);
		return 1;
	}
	
	A = Alloc_FFT_Matrix(N);
	
	#pragma css start
	initialize(N_SQRT, FFT_BS, (void *)A);
	setup_fftw_plan(N_SQRT);
    
	// NOTE: the real FFTW destroys the contents during planing if there was not enough previous wisdom
	
	#pragma css barrier
	reference = Alloc_FFT_Matrix(N);
	setup_fftw_reference_plan(N);
	memcpy(reference, A, N * sizeof(double _Complex));
    
	FFT_1D (N, N_SQRT, FFT_BS, TR_BS, (double _Complex (*) [N_SQRT])A);
	#pragma css finish
   
	fftw_execute_dft(reference_plan, reference, reference);
	
    long i;
	for (i=0; i < N; i++) {
		if (cabs((A[i] - reference[i]) / A[i]) > 0.0001) {
			printf("FAILED\n");
			printf("Element %li: %f\n", i, cabs((A[i] - reference[i]) / A[i]));
			return 1;
		}
	}
	
	fftw_free(A);
	fftw_free(reference);
    
    remove("fftw.wisdom");
    
	return 0;
}
