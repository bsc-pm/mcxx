/*
<testinfo>
test_CFLAGS="--only-adjacent-accesses"
test_generator=config/mercurium-parallel-simd-mic
test_ignore=yes
</testinfo>
*/


#pragma omp simd uniform(i:1) mask
#pragma omp simd linear(i:1) nomask
float foo(int i);


void serialB(float *pdZ, int N)
{
    int b;
#pragma omp simd suitable(N) 
	for(b=0; b<N; b++){
	  pdZ[b]= foo(b); 
    }

#pragma omp simd suitable(N)
	for(b=0; b<N; b++){
      if (pdZ[b] < (N>>1))
    	  pdZ[b] = foo(N); 
    }


}


