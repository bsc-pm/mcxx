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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


// N and MIN_BS must be powers of 2
long N;
long MIN_SORT_SIZE;
long MIN_MERGE_SIZE;

#define T int


int qsort_helper(const void *a, const void *b) {
	T *realA = (T *)a;
	T *realB = (T *)b;
	return *realA - *realB;
}


#if 0

#pragma css task input(n) inout(data)
void basicsort(long n, T data[n]) {
	//printf("Sort n=%i, %x\n\n", n, data);
	qsort(data, n, sizeof(T), qsort_helper);
}

#else

//
// NOTE: This "branch" of the preprocessor if has been taken from the cilksort
// example from the Cilk distribution and has the following copyright:
//

/*
 * Copyright (c) 2000 Massachusetts Institute of Technology
 * Copyright (c) 2000 Matteo Frigo
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */


#define INSERTION_SIZE 20

static void insertion_sort(T *low, T *high)
{
	T *p, *q;
	T a, b;
	for (q = low + 1; q <= high; ++q) {
		a = q[0];
		for (p = q - 1; p >= low && (b = p[0]) > a; p--) {
			p[1] = b;
		}
		p[1] = a;
	}
}

static T med3(T a, T b, T c) {
	if (a < b)
		if (b < c)
			return b;
		else
			if (a < c)
				return c;
			else
				return a;
	else
		if (b > c)
			return b;
		else
			if (a > c)
				return c;
			else
				return a;
}

/*
 * simple approach for now; a better median-finding
 * may be preferable
 */
static T choose_pivot(T *low, T *high)
{
	return med3(*low, *high, low[(high-low)/2UL]);
}

static T *seqpart(T *low, T *high) {
	T pivot;
	T h, l;
	T *curr_low = low;
	T *curr_high = high;

	pivot = choose_pivot(low, high);

	while (1) {
		while ((h = *curr_high) > pivot)
			curr_high--;

		while ((l = *curr_low) < pivot)
			curr_low++;

		if (curr_low >= curr_high)
			break;

		*curr_high-- = l;
		*curr_low++ = h;
	}

	/*
	* I don't know if this is really necessary.
	* The problem is that the pivot is not always the
	* first element, and the partition may be trivial.
	* However, if the partition is trivial, then
	* *high is the largest element, whence the following
	* code.
	*/
	if (curr_high < high)
		return curr_high;
	else
		return curr_high - 1UL;
}

static void seqquick(T *low, T *high) {
	while (high - low > INSERTION_SIZE) {
		T *p = seqpart(low, high);
		seqquick(low, p);
		low = p + 1;
	}
	
	insertion_sort(low, high);
}

#pragma css task input(n) inout(data)
void basicsort(long n, T data[n]) {
    seqquick(data, data + n - 1UL);
}

#endif


static inline int pivots_are_aligned(T *left, T *right, long n, long leftStart, long rightStart) {
	if (leftStart == 0L || rightStart == 0L || leftStart == n || rightStart == n) {
		return 1;
	}
	
	if (left[leftStart] <= right[rightStart] && right[rightStart-1L] <= left[leftStart]) {
		return 1;
	}
	if (right[rightStart] <= left[leftStart] && left[leftStart-1L] <= right[rightStart]) {
		return 1;
	}
	
	return 0;
}


static inline int must_decrease_left(T *left, T *right, long n, long leftStart, long rightStart) {
	return (left[leftStart] > right[rightStart]);
}


static inline long min(long a, long b) {
	if (a < b) {
		return a;
	} else {
		return b;
	}
}


static inline void find_pivot(T *left, T *right, long n, long start, long *leftStart, long *rightStart) {
	*leftStart = start/2L;
	*rightStart = start/2L;
	
	if (start == 0L) {
		return;
	}
	
	int jumpSize;
	if (pivots_are_aligned(left, right, n, *leftStart, *rightStart)) {
		return;
	} else if (must_decrease_left(left, right, n, *leftStart, *rightStart)) {
		jumpSize = min(start/2L, n - start/2L) / 2L;
		*leftStart -= jumpSize;
		*rightStart += jumpSize;
	} else {
		jumpSize = min(start/2L, n - start/2L) / 2L;
		*leftStart += jumpSize;
		*rightStart -= jumpSize;
	}
	
	while (1) {
		if (pivots_are_aligned(left, right, n, *leftStart, *rightStart)) {
			return;
		} else if (must_decrease_left(left, right, n, *leftStart, *rightStart)) {
			jumpSize = (jumpSize+1L)/2L; // At least jump by 1
			*leftStart -= jumpSize;
			*rightStart += jumpSize;
		} else {
			jumpSize = (jumpSize+1L)/2L; // At least jump by 1
			*leftStart += jumpSize;
			*rightStart -= jumpSize;
		}
	}
}


#pragma css task input(n, start, length, left, right) inout(result{start:length})
void merge_task(long n, T left[n], T right[n], T result[n*2], long start, long length) {
	long leftStart, rightStart;
	find_pivot(left, right, n, start, &leftStart, &rightStart);
	
	result += start;
	while (length != 0L) {
		if (leftStart == n) {
			*result = right[rightStart];
			rightStart++;
			result++;
		} else if (rightStart == n) {
			*result = left[leftStart];
			leftStart++;
			result++;
		} else if (left[leftStart] <= right[rightStart]) {
			*result = left[leftStart];
			leftStart++;
			result++;
		} else {
			*result = right[rightStart];
			rightStart++;
			result++;
		}
		length--;
	}
}


void merge_rec(long n, T left[n], T right[n], T result[n*2], long start, long length) {
	if (length <= MIN_MERGE_SIZE) {
		// Base case
		merge_task(n, (T *) left, (T *) right, (T *)result, start, length);
	} else {
		merge_rec(n, (T *) left, (T *) right, (T *) result, start, length/2);
		merge_rec(n, (T *) left, (T *) right, (T *) result, start + length/2, length/2);
	}
}


void multisort(long n, T data[n], T tmp[n]) {
	if (n >= MIN_SORT_SIZE*4L) {
		// Recursive decomposition
		multisort(n/4L, (T *) &data[0], (T *) &tmp[0]);
		multisort(n/4L, (T *) &data[n/4L], (T *) &tmp[n/4L]);
		multisort(n/4L, (T *) &data[n/2L], (T *) &tmp[n/2L]);
		multisort(n/4L, (T *) &data[3L*n/4L], (T *) &tmp[3L*n/4L]);   
        
		merge_rec(n/4L, (T *) &data[0], (T *) &data[n/4L], (T *) &tmp[0], 0, n/2L);
		merge_rec(n/4L, (T *) &data[n/2L], (T *) &data[3L*n/4L], (T *) &tmp[n/2L], 0, n/2L);
        
		merge_rec(n/2L, (T *) &tmp[0], (T *) &tmp[n/2L], (T *) &data[0], 0, n);
        
    } else {
		// base case
		basicsort(n, (T *) &data[0]);
    }
}


#pragma css task input(length) output(data)
static void zz_initialize(long length, T data[length]) {
    long i;
	for (i = 0; i < length; i++) {
		if (i==0) {
			data[i] = rand();
		} else {
			data[i] = (data[i-1] * i * 104723L) % N;
		}
	}
}


#pragma css task input(length, falsedep) input(data)
static void zz_touch(long length, T falsedep[length], T data[length]) {
    long i;
	for (i = 0; i < length; i++) {
		data[i] = 0;
	}
}


int main(int argc, char **argv) {
	N = 16L * (1024L * 1024L);
	MIN_SORT_SIZE = 8L * 1024L;
	MIN_MERGE_SIZE = 4L * MIN_SORT_SIZE;
	
	// Data
	T *data, *tmp, *reference;
	posix_memalign((void **)&data, sizeof(T)*N, sizeof(T)*N);
	posix_memalign((void **)&tmp, sizeof(T)*N, sizeof(T)*N);
	posix_memalign((void **)&reference, sizeof(T)*N, sizeof(T)*N);
	
	
	#pragma css start
	long initializionSize = N/4L, i; //N/32L, i;
	for (i = 0; i < N; i += initializionSize) {
		long length = initializionSize;
		if (i+length > N) {
			length = N - i;
		}
		zz_initialize(length, (T (*)) (&data[i]));
	}
	for (i = 0; i < N; i += initializionSize) {
		long length = initializionSize;
		if (i+length > N) {
			length = N - i;
		}
		zz_touch(length, (T *) &data[i], (T *) &tmp[i]);
	}
	#pragma css barrier
	
	memcpy(reference, data, sizeof(T)*N);
    
	multisort(N, data, tmp);
	#pragma css finish
    
	qsort(reference, N, sizeof(T), qsort_helper);
    
	if (memcmp(reference, data, sizeof(T)*N) == 0) {
		return 0;
	} else {
		printf("FAILED\n");
		return 1;
	}
}
