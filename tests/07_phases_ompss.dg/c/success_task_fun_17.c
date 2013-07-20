/*
<testinfo>
test_generator=config/mercurium-ompss
</testinfo>
*/

#include <stdlib.h>

enum { SIZE = 1000 };

#pragma omp task in([s]a)
void init_data_vptr(void* a, size_t s)
{
    char *q = (char*)a;

    int i;
    for (i = 0; i < s; i++)
    {
        *q = 10;
        q++;
    }
}

#pragma omp task inout([s]a)
void inc_data_vptr(void* a, size_t s)
{
    char *q = (char*)a;

    int i;
    for (i = 0; i < s; i++)
    {
        (*q)++;
        q++;
    }
}

#pragma omp task inout(a[0;s])
void inc_data_vptr_2(void* a, size_t s)
{
    char *q = (char*)a;

    int i;
    for (i = 0; i < s; i++)
    {
        (*q)++;
        q++;
    }
}

#pragma omp task inout([s]a)
void clear_data_cptr(char* a, size_t s);

char c[SIZE] = { };

int main(int argc, char* argv[])
{
    init_data_vptr(c, SIZE);
    inc_data_vptr(c, SIZE);
    inc_data_vptr_2(c, SIZE);
#pragma omp taskwait

    int i;
    for (i = 0; i < SIZE; i++)
    {
        if (c[i] != 12)
            abort();
    }

    return 0;
}
