/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
</testinfo>
*/

#include <stdbool.h>
#include <assert.h>

#define N 10

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

int main()
{
    int x;

    // Addition

    x = 0;
    for (int i = 0; i < N; ++i)
    {
        #pragma omp task reduction(+: x)
        {
            x += 2;
        }
    }
    #pragma omp task reduction(+: x)
    {
        // Empty task to test initialization
    }

    #pragma omp taskwait
    assert(x == N*2);

    // Multiplication

    x = 10;
    for (int i = 0; i < N; ++i)
    {
        #pragma omp task reduction(*: x)
        {
            x *= 2;
        }
    }
    #pragma omp task reduction(*: x)
    {
        // Empty task to test initialization
    }

    #pragma omp taskwait
    assert(x == 10*(1 << N));

    // Substraction

    x = 100;
    for (int i = 0; i < N; ++i)
    {
        #pragma omp task reduction(-: x)
        {
            x -= 2;
        }
    }
    #pragma omp task reduction(-: x)
    {
        // Empty task to test initialization
    }

    #pragma omp taskwait
    assert(x == 100 - N*2);

    // Bitwise AND

    x = ~0;
    for (int i = 0; i < sizeof(int)*8; ++i)
    {
        #pragma omp task reduction(&: x) firstprivate(i)
        {
            if (i%2 == 0)
                x &= ~(1 << i);
        }
    }
    #pragma omp task reduction(&: x)
    {
        // Empty task to test initialization
    }

    #pragma omp taskwait
    for (int j = 0; j < sizeof(int); ++j)
    {
        assert(((unsigned char*)&x)[j] == 0xAA);
    }

    // Bitwise OR

    x = 0;
    for (int i = 0; i < sizeof(int)*8; ++i)
    {
        #pragma omp task reduction(|: x) firstprivate(i)
        {
            if (i%2 == 0)
                x |= (1 << i);
        }
    }
    #pragma omp task reduction(|: x)
    {
        // Empty task to test initialization
    }

    #pragma omp taskwait
    for (int j = 0; j < sizeof(int); ++j)
    {
        assert(((unsigned char*)&x)[j] == 0x55);
    }

    // Bitwise XOR

    x = ~0;
    for (int i = 0; i < sizeof(int)*8; ++i)
    {
        #pragma omp task reduction(^: x) firstprivate(i)
        {
            if (i%2 == 0)
                x ^= (1 << i);
        }
    }
    #pragma omp task reduction(^: x)
    {
        // Empty task to test initialization
    }

    #pragma omp taskwait
    for (int j = 0; j < sizeof(int); ++j)
    {
        assert(((unsigned char*)&x)[j] == 0xAA);
    }

    // Logical AND

    x = true;
    for (int i = 0; i < N; ++i)
    {
        #pragma omp task reduction(&&: x) firstprivate(i)
        {
            x = x && true;
        }
    }
    #pragma omp task reduction(&&: x)
    {
        // Empty task to test initialization
    }

    #pragma omp taskwait
    assert(x);

    // Logical OR

    x = false;
    for (int i = 0; i < N; ++i)
    {
        #pragma omp task reduction(||: x) firstprivate(i)
        {
            if (i%2 == 0)
                x = x || true;
            else
                x = x || false;
        }
    }
    #pragma omp task reduction(||: x)
    {
        // Empty task to test initialization
    }

    #pragma omp taskwait
    assert(x);

    // MAX

    x = 0;
    for (int i = 0; i < N; ++i)
    {
        #pragma omp task reduction(max: x) firstprivate(i)
        {
            x = MAX(x, i);
        }
    }
    #pragma omp task reduction(max: x)
    {
        // Empty task to test initialization
    }

    #pragma omp taskwait
    assert(x == N - 1);

    // MIN

    x = N;
    for (int i = 0; i < N; ++i)
    {
        #pragma omp task reduction(min: x) firstprivate(i)
        {
            x = MIN(x, i);
        }
    }
    #pragma omp task reduction(min: x)
    {
        // Empty task to test initialization
    }

    #pragma omp taskwait
    assert(x == 0);
}
