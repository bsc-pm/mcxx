/*
<testinfo>
test_generator=config/mercurium-iomp
</testinfo>
*/

#include <assert.h>
#include <unistd.h>

#define N 10

int main(void) {
    int mat[N][N] = { 0 };
    #pragma omp parallel
    {
        #pragma omp single
        #pragma omp taskloop grainsize(2) if(1) collapse(2)
        for (int i = 0; i < N; ++i)
            for (int j = 0; j < N; ++j)
                mat[i][j]++;
        #pragma omp taskloop num_tasks(1) if(1) collapse(2)
        for (int i = 0; i < N; ++i)
            for (int j = 0; j < N; ++j)
                assert(mat[i][j] == 1);
    }
}

