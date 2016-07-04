/*
<testinfo>
test_generator=config/mercurium-ompss
</testinfo>
*/
#include <assert.h>

void compute(const int N, const int TS, double (*a)[TS][TS])
{
    int i;
    for (i = 0; i < N; i++)
    {
        #pragma omp task inout(a[i][0;TS][0;TS])
        {
            int j, k;
            for (j = 0; j < TS; ++j)
                for (k = 0; k < TS; ++k)
                    a[i][j][k]++;
        }
    }
}

void init(const int N, const int TS, double (*a)[TS][TS])
{
    int i;
    for (i = 0; i < N; i++)
    {
        #pragma omp task out(a[i][0;TS][0;TS])
        {
            int j, k;
            for (j = 0; j < TS; ++j)
                for (k = 0; k < TS; ++k)
                    a[i][j][k] = 0.0;
        }
    }
}

void check(const int N, const int TS, double (*a)[TS][TS])
{
    int i;
    for (i = 0; i < N; i++)
    {
        #pragma omp task in(a[i][0;TS][0;TS])
        {
            int j, k;
            for (j = 0; j < TS; ++j)
                for (k = 0; k < TS; ++k)
                    assert(a[i][j][k] == 1.0);
        }
    }
}

int main(int argc, char* argv[])
{
    const int N = 10;
    const int TS = 25;

    double v[N][TS][TS];

    init(N, TS, v);
    compute(N, TS, v);
    check(N, TS, v);

    #pragma omp taskwait
    return 0;
}
