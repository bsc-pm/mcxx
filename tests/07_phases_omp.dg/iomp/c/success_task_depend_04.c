/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests array task dependencies without if/final*/

#include <assert.h>
#include <unistd.h>
#include <string.h>

int omp_get_num_threads(void);
int omp_get_thread_num(void);
int omp_in_final(void);

int main(void) {
    char a[10] = {0};
    #pragma omp parallel
    {
        #pragma omp single
        {
            #pragma omp task depend(out: a[0:5])
            {
                usleep(100);
                memset(a, -1, 5);
            }
            #pragma omp task depend(out: a[5:5])
            {
                usleep(100);
                memset(&a[5], 1, 5);
            }

            #pragma omp task depend(in: a[0:5])
            {
                for (int i = 0; i < 5; ++i) {
                    assert(a[i] == -1);
                }
            }
            #pragma omp task depend(in: a[5:5])
            {
                for (int i = 5; i < 10; ++i) {
                    assert(a[i] == 1);
                }
            }
        }

    }
}

