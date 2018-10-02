/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

#include <omp.h>
#include <assert.h>

struct A {
    int x;
};

#pragma omp declare reduction(my_add: struct A : omp_out.x = omp_in.x + omp_out.x) initializer(omp_priv = omp_orig)
int main(int argc, char *argv[]) {
    struct A d[argc][argc];
    for (int i = 0; i < argc; ++i)
        for (int j = 0; j < argc; ++j)
            d[i][j] = (struct A){ .x = 1 };
    #pragma omp parallel
    #pragma omp single
    {
        #pragma omp taskgroup task_reduction(my_add:d)
        {
            for (int i = 0; i < argc; ++i) {
                for (int j = 0; j < argc; ++j) {
                    #pragma omp task in_reduction(my_add:d) firstprivate(i, j)
                    {
                        d[i][j].x = d[i][j].x + i;
                    }
                }
            }
        }
    }
    for (int i = 0; i < argc; ++i)
        for (int j = 0; j < argc; ++j)
            assert(d[i][j].x >= 1 + i);
}


