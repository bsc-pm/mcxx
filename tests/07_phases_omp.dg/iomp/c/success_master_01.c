/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests master*/

#include <assert.h>
#include <unistd.h>

int omp_get_num_threads(void);
int omp_get_thread_num(void);

int main(void) {
    int a = 0;
    #pragma omp parallel
    {
        #pragma omp master
        {
            ++a;
        }
        #pragma omp barrier

        assert(a == 1);

    }
}

