/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests task with shared vlas mixed with constant arrays*/

#include <assert.h>
#include <unistd.h>

int omp_get_num_threads(void);
int omp_get_thread_num(void);

int main(int argc, char *argv[]) {
    int a[argc][10][argc + 1];
    int (*b)[argc][10][argc + 1] = &a;
    a[0][0][0] = 0;
    #pragma omp parallel
    {
        #pragma omp single
        #pragma omp task
        {
            a[0][0][0]++;
            (*b[0][0][0])++;
        }
    }
    assert(a[0][0][0] == 2 && (*b[0][0][0]) == 2);
}

