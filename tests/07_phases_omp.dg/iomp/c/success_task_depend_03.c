/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests task dependencies with final clause*/

#include <assert.h>
#include <unistd.h>

int omp_get_num_threads(void);
int omp_get_thread_num(void);
int omp_in_final(void);

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

            #pragma omp task depend(in: a) final(1)
            {
                a *= omp_in_final()*10;
            }
        }

    }
    assert(a == 10);
}

