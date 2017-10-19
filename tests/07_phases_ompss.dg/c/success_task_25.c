/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
</testinfo>
*/
#include<assert.h>

int x;
int *v;

#pragma omp task inout(v[x])
void f()
{
    assert(v[x] == 1);
    v[x]--;
}

int main()
{
    x = 0;
    int a = 1;
    v = &a;

    f();

    #pragma omp task inout(v[x])
    {
        assert(v[x] == 0);
        v[x]++;
    }
    #pragma omp taskwait
    assert(v[x] == 1);
    return 0;
}
