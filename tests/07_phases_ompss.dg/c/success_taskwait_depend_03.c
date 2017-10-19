/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
test_CFLAGS="--no-copy-deps"
</testinfo>
*/

#include<assert.h>

void foo()
{
    int x = 0, y = 2;

    #pragma omp task depend(inout: x) shared(x)
    x++;

    #pragma omp task depend(in: x) depend(inout: y) shared(x, y)
    y -= x;

    #pragma omp taskwait depend(inout: x)
    assert(x == 1);
    assert(x == y);
}

int main(int argc, char*argv)
{
    foo();
    return 0;
}
