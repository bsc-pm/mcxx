/*
<testinfo>
test_generator=config/mercurium-ompss
</testinfo>
*/
#include<assert.h>

int main(int argc, char*argv[])
{
    const int BS = 10;
    int v[BS][BS];
    for (int i = 0; i < BS; ++i)
        for (int j = 0; j < BS; ++j)
            v[i][j] = 0;


    int *ptr =(int*)&v;
    #pragma omp task
    {
        int x = BS;
        typedef int (*ptrArray)[x];
        ptrArray m = reinterpret_cast<ptrArray>(ptr);

        double u = m[0][0] + m[1][1];

        for (int i = 0; i < x; ++i)
            for (int j = 0; j < x; ++j)
                m[i][j] = 1;
    }
    #pragma omp taskwait

    for (int i = 0; i < BS; ++i)
        for (int j = 0; j < BS; ++j)
            assert(v[i][j] == 1);
    return 0;
}
