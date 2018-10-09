/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests task vla dependencies without if/final*/

#include <assert.h>
#include <unistd.h>

int omp_get_num_threads(void);
int omp_get_thread_num(void);

int main(int argc, char *argv[]) {
    int a[argc];
    #pragma omp parallel
    {
        #pragma omp single
        {
            #pragma omp task depend(out: a)
            {
                usleep(100);
                a[0] = 1;
            }

            #pragma omp task depend(in: a)
            {
                a[0] *= 10;
            }
        }

    }
    assert(a[0] == 10);
}

