/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/*This tests taskloop firstprivate vla alignment*/

#include <inttypes.h>
#include <assert.h>

int main(int argc, char *argv[]) {
    char a[argc + 1];
    int b[argc + 1];
    char a1[argc + 2];
    int b1[argc + 2];
    char a2[argc + 3];
    int b2[argc + 3];
    #pragma omp parallel
    #pragma omp single
    {
        #pragma omp taskloop num_tasks(50) firstprivate(a, b, a1, b1, a2, b2)
        for (int i = 0; i < 100; ++i)
        {
            a[0] = a1[0] = a2[0];
            assert(!((uintptr_t)b & (sizeof(int) - 1)));
            assert(!((uintptr_t)b1 & (sizeof(int) - 1)));
            assert(!((uintptr_t)b2 & (sizeof(int) - 1)));
        }
    }
}

