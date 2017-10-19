/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
test_CFLAGS="-std=gnu99 -Werror"
</testinfo>
*/

#include<assert.h>
#include<stdlib.h>

void v(int n) {
    int (*v1)[n] = (int (*)[n])malloc(n*sizeof(int));
    for (int i = 0; i < n; ++i)
        (*v1)[i] = 0;

    #pragma omp task firstprivate(v1)
    {
        for (int i = 0; i < n; ++i)
            (*v1)[i] = 7;
    }

    #pragma omp taskwait

    for (int i = 0; i < n; ++i)
        assert((*v1)[i] == 7);

    free(v1);
}

int main(int argc, char*argv[])
{
    v(100);
    return 0;
}
