/*
<testinfo>
test_generator="config/mercurium-hlt run"
</testinfo>
*/

#include <assert.h>

char c[200];
char d[200];

void init()
{
    int i;
    for (i = 0; i < 200; i++)
    {
        c[i] = 0;
        d[i] = 0;
    }
}

void test1(void)
{
    init();

    int i;
    for (i = 4; i < 100; i+=7)
    {
        d[i]++;
    }

#pragma hlt normalize
    for (i = 4; i < 100; i+=7)
    {
        c[i]++;
    }

    for (i = 4; i < 100; i+=7)
    {
        assert(c[i] == 1);
    }

    for (i = 0; i < 200; i++)
    {
        assert(c[i] == d[i]);
    }
}

void test2(void)
{

    init();

    int i;
    for (i = 100; i >= 3; i -= 11)
    {
        d[i]++;
    }

    #pragma hlt normalize
    for (int k = 100; k >= 3; k -= 11)
    {
        c[k]++;
    }

    for (i = 100; i >= 3; i -= 11)
    {
        assert(c[i] == 1);
    }

    for (i = 0; i < 200; i++)
    {
        assert(c[i] == d[i]);
    }
}

int main(int argc, char *argv[])
{
    test1();
    test2();

    return 0;
}
