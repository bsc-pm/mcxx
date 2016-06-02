/*
<testinfo>
test_generator=config/mercurium-ompss
</testinfo>
*/
#include<assert.h>

int f(int &x) {
    x = 2;
    return 3;
}

int main(int argc, char*argv[])
{
    int res = 1;
    #pragma omp task shared(res)
    {
        int x = f(x) + x;
        res = x;
    }
    #pragma omp taskwait

    assert(res == 5);
    return 0;
}
