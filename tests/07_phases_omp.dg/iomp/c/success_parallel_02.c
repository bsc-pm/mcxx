/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests shared/private variables*/

#include <assert.h>

#define N_THREADS 4

int omp_get_num_threads(void);
int omp_get_thread_num(void);

int main(void) {
    int a = -1;
    #pragma omp parallel num_threads(N_THREADS) firstprivate(a)
    {
        a++;
    }
    assert(a == -1);

    #pragma omp parallel num_threads(N_THREADS)
    {
        a++;
    }
    assert(a != -1);
}
