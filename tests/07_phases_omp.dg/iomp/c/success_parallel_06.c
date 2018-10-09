/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests omp parallel reductions*/

#include <assert.h>
#include <stdio.h>

int omp_get_max_threads(void);

int main(int argc, char *argv[]) {
    int a = 1;
    #pragma omp parallel reduction(+: a)
    {
        a = 1;
    }
    assert(a == omp_get_max_threads() + 1);
}

