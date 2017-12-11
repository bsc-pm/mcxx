/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests task with shared vlas*/

#include <assert.h>
#include <unistd.h>

int omp_get_num_threads(void);
int omp_get_thread_num(void);

int main(int argc, char *argv[]) {
    int a[argc];
    int (*b)[argc] = &a;
    a[0] = 0;
    #pragma omp parallel
    {
        #pragma omp single
        #pragma omp task
        {
            a[0]++;
            (*b[0])++;
        }
    }
    assert(a[0] == 2 && *b[0] == 2);
}

