/*
<testinfo>
test_CFLAGS="--only-adjacent-accesses"
test_generator=config/mercurium-parallel-simd-mic
test_ignore=yes
</testinfo>
*/


#pragma omp simd linear(i:1)
float foo(int i);


void goo(float *pdZ, int N)
{
    int b;
#pragma omp simd suitable(N) 
	for(b=0; b<N; b++){
	  pdZ[b]= foo(b); 
    }
}

int main(int argc, char * argv[])
{
    const int N = 6400;
    int i;

    float *pdZ;

    if (posix_memalign((void **)&pdZ, 64, N * sizeof(float)) != 0)
    {
        exit(1);
    }

    goo(pdZ, N);

    return 0;
}
