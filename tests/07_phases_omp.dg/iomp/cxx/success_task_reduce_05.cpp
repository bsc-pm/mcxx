/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

#include <assert.h>
#include <unistd.h>
#include <omp.h>

#pragma omp declare reduction(my_add: int : omp_out = omp_in + omp_out) initializer(omp_priv = omp_orig)
int main(int argc, char *argv[]) {
    int a[10];
    for (int i = 0; i < 10; ++i) a[i] = 1;
    #pragma omp parallel num_threads(1)
    #pragma omp single
    {
        #pragma omp taskgroup task_reduction(my_add:a)
        {
            for (int i = 0; i < 10; ++i)
            #pragma omp task in_reduction(my_add:a) firstprivate(i)
            {
                a[i] += i;
            }
        }
        for (int i = 0; i < 10; ++i)
        {
            // init + i (+ private copy)
            assert(a[i] >= 1 + i);
        }
    }
}

