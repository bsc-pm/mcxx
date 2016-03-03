/*
<testinfo>
test_CFLAGS="--only-adjacent-accesses --only-aligned-accesses"
test_generator=config/mercurium-serial-simd-mic
</testinfo>
*/

void foo(float **pdZ, float **a, int N)
{
    int b;
#pragma omp simd aligned(pdZ[:],a[:])
	for(b=0; b<N; b++){
	  pdZ[5][b]= a[5][b]; 
    }
}

int main(int argc, char * argv[])
{
    const int N = 6400;
    int i;

    float *pdZ[N];
    float *a[N];

    for (i=0; i<N; i++)
    {
        if (posix_memalign((void **)&pdZ[i], 64, N * sizeof(float)) != 0)
        {
            exit(1);
        }
    }

    for (i=0; i<N; i++)
    {
        if (posix_memalign((void **)&a[i], 64, N * sizeof(float)) != 0)
        {
            exit(1);
        }
    }

    foo(pdZ, a, N);

    return 0;
}
