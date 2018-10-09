/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests taskgroup*/

#include <assert.h>
#include <unistd.h>
#include <string.h>

int omp_get_num_threads(void);
int omp_get_thread_num(void);
int omp_in_final(void);

int main(void) {
    int a = 0;
    #pragma omp parallel
    #pragma omp single
    {
        #pragma omp taskgroup
        {
            #pragma omp task
            {
                #pragma omp task
                {
                    usleep(100);
                    a = 1;
                }
            }
        }
        #pragma omp task
        a *= 10;
    }
    assert(a == 10);
}
