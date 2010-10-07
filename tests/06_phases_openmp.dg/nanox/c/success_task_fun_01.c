/*
<testinfo>
test_generator=config/mercurium-nanox
</testinfo>
*/

#include <stdlib.h>

#pragma omp task inout(*a)
void foo1(int *a)
{
    *a = 3 + *a;
}

#pragma omp task inout([1] a)
void foo2(int *a)
{
    a[0] = 3 + a[0];
}

#pragma omp task inout(a[3:4])
void foo3(int *a)
{
    a[3] = a[3] + 10;
    a[4] = a[4] * 12;
}

#pragma omp task inout(([10] a)[3:4])
void foo4(int *a)
{
    a[3] = a[3] + 10;
    a[4] = a[4] * 12;
}

int main(int argc, char *argv[])
{
    int a[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

    foo1(a);
    if (a[0] != 3)
        abort();
    a[0] = 0;

    foo2(a);
    if (a[0] != 3)
        abort();
    a[0] = 0;

    foo3(a);
    if (a[3] != 13
            || a[4] != 48)
        abort();
    a[3] = 3;
    a[4] = 4;

    foo4(a);
    if (a[3] != 13
            || a[4] != 48)
        abort();

    return 0;
}
