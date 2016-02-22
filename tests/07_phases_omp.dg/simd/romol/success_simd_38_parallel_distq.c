
/*
<testinfo>
test_CFLAGS="--only-adjacent-accesses"
test_generator=config/mercurium-parallel-simd-mic
</testinfo>
*/


#include <stdio.h>

#pragma omp declare simd 
float min(float a, float b)
{
    return a < b ? a : b;
}

#pragma omp declare simd 
float distsq(float x, float y)
{
    float tmp = x - y;
    return tmp * tmp;
}

void compute (float *a, float *b, float *c, float *d, int N, int num_its)
{
    int it;
    for (it=0; it < num_its; it++)
    {
        int i;

        #pragma omp simd for nowait 
        for (i=0; i<N; i++)
        {
            d[i] = min(distsq(a[i], b[i]), c[i]);
        }
    }
}


float min_ref(float a, float b)
{
    return a < b ? a : b;
}

float distsq_ref(float x, float y)
{
    float tmp = x - y;
    return tmp * tmp;
}

void __attribute__((noinline)) compute_ref (float *a, float *b, float *c, float *d, int N, int num_its)
{
    int it;
    for (it=0; it < num_its; it++)
    {
        int i;

        for (i=0; i<N; i++)
        {
            d[i] = min_ref(distsq_ref(a[i], b[i]), c[i]);
        }
    }
}






int main(int argc, char * argv[])
{
    const int N = 6400;
    const int num_its = 1;

    float *a, *b, *c, *d, *ref;

    if(posix_memalign((void **) &a, 64, N * sizeof(float)) != 0)
    {
        exit(1);
    }
    if(posix_memalign((void **) &b, 64, N * sizeof(float)) != 0)
    {
        exit(1);
    }
    if(posix_memalign((void **) &c, 64, N * sizeof(float)) != 0)
    {
        exit(1);
    }
    if(posix_memalign((void **) &d, 64, N * sizeof(float)) != 0)
    {
        exit(1);
    }
    if(posix_memalign((void **) &ref, 64, N * sizeof(float)) != 0)
    {
        exit(1);
    }

    int i;

    for (i=0; i<N; i++)
    {
        float r = rand();
        a[i] = r+1.0f;
        b[i] = r-2.0f;
        c[i] = r+2.0f;
        d[i] = 0.0f;
        ref[i] = 0.0f;
    }


#pragma omp parallel
    {
        compute(a, b, c, d, N, num_its);
    }

#pragma omp parallel
    {
        compute_ref(a, b, c, ref, N, 1);
    }

    for (i=0; i<N; i++)
    {
        float ref_e = ref[i];
        float d_e = d[i];

        if (ref_e != d_e)
        {
            fprintf(stderr, "%d: ref %f != d %f\n", i, ref_e, d_e); 
            return 1;
        }
    }

    fprintf(stderr, "SUCCESS!\n"); 

    return 0;
}
