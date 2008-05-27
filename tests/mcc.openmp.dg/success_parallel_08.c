#include <stdio.h>

void fib(int a)
{
#pragma omp parallel if (a > 50)
    {
        printf("Lalala\n");
    }
}
