/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

/* Check induction var lowering on taskloop construct*/

#include <inttypes.h>
#include <assert.h>

int main(int argc, char *argv[]) {
    int i;
    int aux;
    int res = 0;
    #pragma omp parallel
    #pragma omp single
    {
        #pragma omp taskloop num_tasks(1)
        for (i = 1; i < 2; ++i) // i private
        {
            ++res;
            aux = i; // forward i to next taskloop
        }
        #pragma omp taskloop num_tasks(1)
        for (int j = aux; j < 2; ++j)
        {
            ++res;
        }
    }
    assert(res == 2);
}

