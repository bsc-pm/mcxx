/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests single wait/nowait*/

#include <assert.h>
#include <unistd.h>

int omp_get_num_threads(void);
int omp_get_thread_num(void);

int main(void) {
    int a = -1;
    #pragma omp parallel num_threads(4)
    {
        #pragma omp single
        {
            ++a;
            usleep(100);
        }

        assert(a == 0);
    }

    a = -1;
    int ok = 0;
    #pragma omp parallel num_threads(4)
    {
        #pragma omp single nowait
        {
            usleep(100);
            ++a;
        }

        if (a == -1) {
            ok = 1;
        }
        assert(ok == 1);
    }
}

