/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests shared/private variables*/

#include <assert.h>

int omp_get_num_threads(void);
int omp_get_thread_num(void);

int main(void) {
    int a = -1;
    #pragma omp parallel num_threads(4) firstprivate(a)
    {
        a++;
    }
    assert(a == -1);

    #pragma omp parallel num_threads(4)
    {
        a++;
    }
    assert(a != -1);
}
