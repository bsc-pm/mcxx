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

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h> 


#define NB (4L-2L)
#define B 16L

#define INITIAL_VALUE(I, i, J, j) ((I+i)*(NB+2L)*(B) + (J+j))
#define ITERATION_INCREMENT ((NB+2L)*(NB+2)*B*B)


static const long _B = B;


#pragma css task input(I, J) output(A{0:_B}{0:_B})
static void zz_fillBlock(long I, long J, long A[(NB+2L)*B][(NB+2L)*B]) {
    long i, j;
	for (i=0; i < B; i++) {
		for (j=0; j < B; j++) {
			A[i][j] = INITIAL_VALUE(I, i, J, j);
		}
	}
}


static void alloc_and_genmat (long (**A)[(NB+2L)*B])
{
	//*A = (long (*)[(NB+2)*B])malloc(sizeof(long)*((NB+2)*B)*((NB+2)*B));
	posix_memalign((void **)A, sizeof(long)*(NB+2L)*B*B, sizeof(long)*((NB+2L)*B)*((NB+2L)*B));
	if (*A == NULL) {
		printf("Memory allocation error %liMB\n", (sizeof(long)*((NB+2L)*B)*((NB+2L)*B))/(1024L*1024L));
		exit(1);
	}
	
	long i, j;
	for (i=0; i < (NB+2L)*B; i+=B) {
		for (j=0; j < (NB+2L)*B; j+=B) {
			zz_fillBlock(i, j, (long (*)[(NB+2L)*B])   &((*A)[i][j]));
		}
	}
}


#pragma css task input(iter, I, J, A{0}{1:_B}, A{_B+1}{1:_B}, A{1:_B}{0}, A{1:_B}{_B+1}) inout(A{1:_B}{1:_B}) 
void stencil_op(long iter, long I, long J, long A[(NB+2L)*B][(NB+2L)*B])
{
    long j;
	// Verify values for the north halo
	if (I+1L == 1*B) {
		// Boundary halo
		for (j=1; j <= B; j++) {
			if (A[0][j] != INITIAL_VALUE(I, 0, J, j)) {
				printf("Invalid north boundary halo\n");
				printf("it %li: [%li][%li] [%i][%li], %li != %li\n", iter, I, J, 0, j, A[0][j], INITIAL_VALUE(I, 0, J, j));
				abort();
			}
		}
	} else {
		// Inner halo: must have already executed the stencil operation
		for (j=1; j <= B; j++) {
			if (A[0][j] != INITIAL_VALUE(I, 0, J, j) + ITERATION_INCREMENT*(iter+1L)) {
				printf("Invalid north inner halo\n");
				printf("it %li: [%li][%li] [%i][%li], %li != %li\n", iter, I, J, 0, j, A[0][j], INITIAL_VALUE(I, 0, J, j) +
ITERATION_INCREMENT*(iter+1L));
				abort();
			}
		}
	}
	
	// Verify values for the south halo
	if (I+1L == (1+NB-1)*B) {
		// Boundary halo
		for (j=1; j <= B; j++) {
			if (A[B+1][j] != INITIAL_VALUE(I, B+1, J, j)) {
				printf("Invalid south boundary halo\n");
				printf("it %li: [%li][%li] [%li][%li], %li != %li\n", iter, I, J, B+1, j, A[B+1][j], INITIAL_VALUE(I, B+1, J, j));
				abort();
			}
		}
	} else {
		// Inner halo: must not have executed the stencil operation
		for (j=1; j <= B; j++) {
			if (A[B+1][j] != INITIAL_VALUE(I, B+1, J, j) + ITERATION_INCREMENT*iter) {
				printf("Invalid south inner halo\n");
				printf("it %li: [%li][%li] [%li][%li], %li != %li\n", iter, I, J, B+1, j, A[B+1][j], INITIAL_VALUE(I, B+1, J, j) +
ITERATION_INCREMENT*iter);
				abort();
			}
		}
	}
	
	long i;
	// Verify values for the west halo
	if (J+1L == 1*B) {
		// Boundary halo
		for (i=1; i <= B; i++) {
			if (A[i][0] != INITIAL_VALUE(I, i, J, 0)) {
				printf("Invalid west boundary halo\n");
				printf("it %li: [%li][%li] [%li][%i], %li != %li\n", iter, I, J, i, 0, A[i][0], INITIAL_VALUE(I, i, J, 0));
				abort();
			}
		}
	} else {
		// Inner halo: must have already executed the stencil operation
		for (i=1; i <= B; i++) {
			if (A[i][0] != INITIAL_VALUE(I, i, J, 0) + ITERATION_INCREMENT*(iter+1L)) {
				printf("Invalid west inner halo\n");
				printf("it %li: [%li][%li] [%li][%i], %li != %li\n", iter, I, J, i, 0, A[i][0], INITIAL_VALUE(I, i, J, 0) +
ITERATION_INCREMENT*(iter+1L));
				abort();
			}
		}
	}
	
	// Verify values for the east halo
	if (J+1L == (1+NB-1)*B) {
		// Boundary halo
		for (i=1; i <= B; i++) {
			if (A[i][B+1] != INITIAL_VALUE(I, i, J, B+1)) {
				printf("Invalid east boundary halo\n");
				printf("it %li: [%li][%li] [%li][%li], %li != %li\n", iter, I, J, i, B+1, A[i][B+1], INITIAL_VALUE(I, i, J, B+1));
				abort();
			}
		}
	} else {
		// Inner halo: must not have executed the stencil operation
		for (i=1; i <= B; i++) {
			if (A[i][B+1] != INITIAL_VALUE(I, i, J, B+1) + ITERATION_INCREMENT*iter) {
				printf("Invalid east inner halo\n");
				printf("it %li: [%li][%li] [%li][%li], %li != %li\n", iter, I, J, i, B+1, A[i][B+1], INITIAL_VALUE(I, i, J, B+1) +
ITERATION_INCREMENT*iter);
				abort();
			}
		}
	}
	
	for (i=1; i <= B; i++) {
		for (j=1; j <= B; j++) {
			A[i][j] += ITERATION_INCREMENT;
		}
	}
}


int main(int argc, char **argv) {
	long (*A)[(NB+2)*B];
	
	#pragma css start
	alloc_and_genmat(&A);
	
	#pragma css barrier
    
    int iters;
    long i, j;
	for (iters=0; iters<1; iters++) {
		for (i=B; i < (NB+1)*B; i+=B) {
			for (j=B; j < (NB+1)*B; j+=B) {
				stencil_op(iters, i-1, j-1, (long (*) [(NB+2)*B])(&A[i-1][j-1]));
			}
		}
	}
	#pragma css finish
	
	return 0;
}
