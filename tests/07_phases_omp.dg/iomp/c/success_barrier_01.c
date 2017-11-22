/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests barrier*/

#include <assert.h>
#include <unistd.h>

#define N_THREADS 4

int omp_get_num_threads(void);
int omp_get_thread_num(void);

int main(void) {
    int a[N_THREADS] = { 0 };
    #pragma omp parallel num_threads(N_THREADS)
    {
        a[omp_get_thread_num()] = omp_get_thread_num();

        #pragma omp barrier

        for (int i = 0; i < N_THREADS; ++i) {
            assert(a[i] == i);
        }
    }
}

