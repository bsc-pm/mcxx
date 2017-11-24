/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests task dependencies with if clause*/

#include <assert.h>
#include <unistd.h>

int omp_get_num_threads(void);
int omp_get_thread_num(void);

int main(void) {
    int a = 0;
    #pragma omp parallel
    {
        #pragma omp single
        {
            #pragma omp task depend(out: a)
            {
                usleep(100);
                a = 1;
            }

            #pragma omp task depend(in: a) if(1)
            {
                a *= 10;
            }
        }

    }
    assert(a == 10);
}

