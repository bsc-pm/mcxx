/*
<testinfo>
test_generator="config/mercurium-ompss-2"
test_ENV=NANOS6_SCHEDULER=naive
</testinfo>
*/
#include<assert.h>

void foo(int n)
{
    int x, v1[10], v2[n];

    x = 42;
    v1[0] = 1;
    v2[0] = 2;

    #pragma oss loop shared(x) firstprivate(v1) private(v2)
    for(int i = 0; i < 1; ++i)
    {
        assert(x == 42);
        assert(v1[0] == 1);

        x = -1;
        v1[0] = -1;
        v2[0] = -1;
    }

    #pragma oss taskwait
    assert(x==-1);
    assert(v1[0]==1);
    assert(v2[0]==2);
}


int main(int argc, char*argv[])
{
    foo(5);
}
