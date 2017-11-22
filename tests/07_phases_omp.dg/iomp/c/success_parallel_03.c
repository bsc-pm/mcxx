/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests struct shared/private variables*/

#include <assert.h>

#define N_THREADS 4

int omp_get_num_threads(void);
int omp_get_thread_num(void);

struct A {
    int x;
};

int main(void) {
    struct A a = { -1 };
    #pragma omp parallel num_threads(N_THREADS) firstprivate(a)
    {
        a.x++;
    }
    assert(a.x == -1);

    #pragma omp parallel num_threads(N_THREADS)
    {
        a.x++;
    }
    assert(a.x != -1);

}

