/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests array shared/private variables*/

#include <assert.h>

int omp_get_num_threads(void);
int omp_get_thread_num(void);

int main(void) {
    int a[4] = { 0 };
    #pragma omp parallel num_threads(4) firstprivate(a)
    {
        a[omp_get_thread_num()] = omp_get_thread_num();
    }

    for (int i = 0; i < 4; ++i) {
        assert(a[i] == 0);
    }

    #pragma omp parallel num_threads(4)
    {
        a[omp_get_thread_num()] = omp_get_thread_num();
    }

    for (int i = 0; i < 4; ++i) {
        assert(a[i] == i);
    }

}

