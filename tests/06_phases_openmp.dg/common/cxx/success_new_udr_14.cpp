/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/

#include <stdlib.h>

struct A
{
};

#pragma omp declare reduction (myop: A : _out=_in) identity(constructor)

struct B: A
{
};

int main(int argc, char* argv[])
{
    A b;
    int r;
    #pragma omp parallel reduction(myop: b)
    r = rand();

    printf("The random result is '%d'", r);

    return 0;
}
