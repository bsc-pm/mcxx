/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

#include <assert.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
    int a = 0;
    #pragma omp parallel
    #pragma omp single
    {
        #pragma omp taskgroup task_reduction(+:a)
        {
            for (int i = 0; i < 10; ++i)
            #pragma omp task in_reduction(+:a) firstprivate(i)
            {
                a += i;
            }
        }
    }
    assert(a == 10/2*9);
}


