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
    struct A d[argc];
    for (int i = 0; i < argc; ++i) d[i] = (struct A){ .x = 1 };
    #pragma omp parallel
    #pragma omp single
    {
        #pragma omp taskgroup task_reduction(my_add:d)
        {
            for (int i = 0; i < argc; ++i) {
                #pragma omp task in_reduction(my_add:d) firstprivate(i)
                {
                    d[i].x = d[i].x + i;
                }
            }
        }
    }
    for (int i = 0; i < argc; ++i) {
        assert(d[i].x >= 1 + i);
    }
}


