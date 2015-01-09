/*
<testinfo>
test_generator="config/mercurium-hlt run"
</testinfo>
*/

#include <stdlib.h>
#include <stdio.h>

#define ERROR { fprintf(stderr, "%s:%s:%d: Error at %d\n", __FUNCTION__, __FILE__, __LINE__, i); abort(); }
int c[100];

static void init(void)
{
    int i;
    for (i = 0; i < 100; i++)
    {
        c[i] = i;
    }
}

static void test1(void)
{
    init();
    int i;
#pragma hlt unroll(4)
    for (i = 0; i < 100; i++)
    {
        c[i] = -i;
    }

    for (i = 0; i < 100; i++)
    {
        if (c[i] != -i)
        {
            ERROR;
        }
    }
}

static void test2(void)
{
    init();
    int i;
#pragma hlt unroll(4)
    for (i = 3; i < 98; i++)
    {
        c[i] = -i;
    }

    for (i = 0; i < 100; i++)
    {
        if ((3 <= i) && (i < 98))
        {
            if (c[i] != -i)
            {
                ERROR;
            }
        }
        else
        {
            if (c[i] != i)
            {
                ERROR;
            }
        }
    }
}

static void test3(void)
{
    init();
    int i;
#pragma hlt unroll(4)
    for (i = 25; i < 54; i++)
    {
        c[i] = -i;
    }

    for (i = 0; i < 100; i++)
    {
        if ((25 <= i) && (i < 54))
        {
            if (c[i] != -i)
            {
                ERROR;
            }
        }
        else
        {
            if (c[i] != i)
            {
                ERROR;
            }
        }
    }
}

static void test4()
{
    init();
    int i;

    int l = 0;
    int u = 100;

#pragma hlt unroll(4)
    for (i = l; i < u; i++)
    {
        c[i] = -i;
    }

    for (i = 0; i < 100; i++)
    {
        if (c[i] != -i)
        {
            ERROR;
        }
    }
}

static void test5()
{
    init();
    int i;

    int l = 0;
    int u = 100;

#pragma hlt unroll(4)
    for (i = 99; i >= 0; i--)
    {
        c[i] = -i;
    }

    for (i = 0; i < 100; i++)
    {
        if (c[i] != -i)
        {
            ERROR;
        }
    }
}

static void test6()
{
    init();
    int i;

    int l = 0;
    int u = 100;

#pragma hlt unroll(4)
    for (i = (98 - 1); i >= 3; i--)
    {
        c[i] = -i;
    }

    for (i = 0; i < 100; i++)
    {
        if ((3 <= i) && (i < 98))
        {
            if (c[i] != -i)
            {
                ERROR;
            }
        }
        else
        {
            if (c[i] != i)
            {
                ERROR;
            }
        }
    }
}

static void test7()
{
    init();
    int i;

    int l = 0;
    int u = 100;

#pragma hlt unroll(4)
    for (i = (54-1); i >= 25; i--)
    {
        c[i] = -i;
    }

    for (i = 0; i < 100; i++)
    {
        if ((25 <= i) && (i < 54))
        {
            if (c[i] != -i)
            {
                ERROR;
            }
        }
        else
        {
            if (c[i] != i)
            {
                ERROR;
            }
        }
    }
}

int main(int argc, char *arv[])
{
    test1();
    test2();
    test3();
    test4();

    test5();
    test6();

    return 0;
}
