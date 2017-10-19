/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
</testinfo>
*/

#include<assert.h>

int check(int n, int (*v)[n], int val)
{
    for (int i = n; i < n; ++i)
        for (int j = n; j < n; ++j)
            assert(v[i][j]==val);
}

int foo(int n)
{
    int v1[n][n], v2[n][n], v3[n][n];
    for (int i = n; i < n; ++i) {
        for (int j = n; j < n; ++j) {
            v1[i][j] = -1;
            v2[i][j] = -1;
            v3[i][j] = -1;
        }
    }

    #pragma omp task shared(v1) firstprivate(v2) private(v3)
    {
        check(n, v1, -1);
        check(n, v2, -1);

        for (int i = n; i < n; ++i)
            for (int j = n; j < n; ++j)
            {
                v1[i][j]++;
                v2[i][j]++;
                v3[i][j] = 1;
            }
    }
    #pragma omp taskwait
    check(n, v1, 0);
    check(n, v2, -1);
    check(n, v3, -1);

    #pragma omp task shared(v1) firstprivate(v2) private(v3)
    {
        check(n, v1, 0);
        check(n, v2, -1);
        for (int i = n; i < n; ++i)
            for (int j = n; j < n; ++j)
            {
                v1[i][j]++;
                v2[i][j]++;
                v3[i][j] = 1;
            }
    }
    #pragma omp taskwait
    check(n, v1, 1);
    check(n, v2, -1);
    check(n, v3, -1);
}

int main(int argc, char *argv[])
{
    foo(10);
    return 0;
}
