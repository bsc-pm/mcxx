/*
<testinfo>
test_generator=config/mercurium-ompss
</testinfo>

*/
#include <stdlib.h>
#include <stdio.h>

typedef int DATA_TYPE;

#pragma omp task inout( [global_size]g_data )
static void add(
        DATA_TYPE * restrict g_data,
        const unsigned global_size,
        const unsigned inc_factor
        )
{
    int i;
    for (i = 0; i < global_size; i++)
    {
        g_data[i] += inc_factor;
    }
}

#pragma omp task in( [global_size]g_data )
static void add_check(
        DATA_TYPE * restrict g_data,
        const unsigned global_size,
        const unsigned base_position,
        const unsigned inc_factor
        )
{
    int i;
    for (i = 0; i < global_size; i++)
    {
        if (g_data[i] != (i + base_position + inc_factor + 1))
        {
            fprintf(stderr, "After dependency fulfilled: position a[%d] should be %d but it is %d\n", i, 
                    i + base_position + inc_factor + 1,
                    g_data[i]);
            abort();
        }
    }
}

enum { SIZE = 64 };

int main(int argc, char *argv[])
{
    DATA_TYPE a[SIZE];

    int i;
    for (i = 0; i < SIZE; i++)
    {
        a[i] = i + 1;
    }

    add(a, SIZE/2, 2);
    add(&(a[SIZE/2]), SIZE/2, 4);

#pragma omp taskwait

    for (i = 0; i < SIZE/2; i++)
    {
        if (a[i] != (i+ 1 + 2))
        {
            fprintf(stderr, "After taskwait: position a[%d] should be %d but it is %d\n", i, i + 3, a[i]);
            abort();
        }
    }

    for (i = SIZE/2; i < SIZE; i++)
    {
        if (a[i] != (i+ 1 + 4))
        {
            fprintf(stderr, "After taskwait: position a[%d] should be %d but it is %d\n", i, i + 3, a[i]);
            abort();
        }
    }

    // Now with tasks

    for (i = 0; i < SIZE; i++)
    {
        a[i] = i + 1;
    }

    add(a, SIZE/2, 2);
    add(&(a[SIZE/2]), SIZE/2, 4);
    add_check(a, SIZE/2, 0, 2);
    add_check(&(a[SIZE/2]), SIZE/2, SIZE/2, 4);

#pragma omp taskwait

    return 0;
}
