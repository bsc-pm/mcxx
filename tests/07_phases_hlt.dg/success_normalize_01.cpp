/*
<testinfo>
test_generator="config/mercurium-hlt run"
</testinfo>
*/

#include <assert.h>

#define N 100

char c[N];
char d[N];

void init()
{
    int i;
    for (i = 0; i < N; i++)
    {
        c[i] = 0;
        d[i] = 0;
    }
}

void test1(void)
{
    init();

    int i;
    for (i = 4; i < N; i+=7)
        d[i]++;

    #pragma hlt normalize
    for (i = 4; i < N; i+=7)
        c[i]++;

    for (i = 4; i < N; i+=7)
        assert(c[i] == 1);

    for (i = 0; i < N; i++)
        assert(c[i] == d[i]);
}

void test2(void)
{

    init();

    int i;
    for (i = N-1; i >= 3; i -= 11)
        d[i]++;

    #pragma hlt normalize
    for (int k = N-1; k >= 3; k -= 11)
        c[k]++;

    for (i = N-1; i >= 3; i -= 11)
        assert(c[i] == 1);

    for (i = 0; i < N-1; i++)
        assert(c[i] == d[i]);
}

int main(int argc, char *argv[])
{
    test1();
    test2();

    return 0;
}
