/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests task with firstprivate vlas*/

#include <assert.h>
#include <unistd.h>

int omp_get_num_threads(void);
int omp_get_thread_num(void);

int main(int argc, char *argv[]) {
    int a[argc];
    int b[argc][argc];
    a[0] = b[0][0] = 0;
    #pragma omp parallel
    {
        #pragma omp single
        #pragma omp task firstprivate(a, b)
        {
            a[0]++;
            b[0][0]++;
            assert(a[0] == 1 && b[0][0] == 1);
        }
    }
    assert(a[0] == 0 && b[0][0] == 0);
}

