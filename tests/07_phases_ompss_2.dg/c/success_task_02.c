/*
<testinfo>
test_generator="config/mercurium-ompss-2"
</testinfo>
*/

#include<assert.h>

// In OmpSs-2, the data environment of a task is destroyed once all its children tasks have been executed-
int main(int argc, char *argv[])
{
    int i, j;
    int c = -1;
    int v[4] = { -1, -1, -1, -1 };

    int N = 5;
    int vla[N];
    for (i = 0; i < N; ++i)
        vla[i] = -1;

    int vla2[N][N];
    for (i = 0; i < N; ++i)
        for (j = 0; j < N; ++j)
        vla2[i][j] = -1;

    #pragma oss task private(c, v, vla, vla2)
    {
        c = 0;
        v[0] = 12;
        vla[N-1] = 41;

        vla2[N-1][N-1] = 61;

        #pragma oss task shared(c, v, vla, vla2)
        {
            c++;
            assert(c == 1 && "'c' has an unexepected value\n");

            v[0]++;
            assert(v[0] == 13 && "'v[0]' has an unexpected value\n");

            vla[N-1]++;
            assert(vla[N-1] == 42 && "'vla[0]' has an unexpected value\n");

            vla2[N-1][N-1]++;
            assert(vla2[N-1][N-1] == 62 && "'vla2[N-1][N-1]' has an unexpected value\n");
        }
    }

    #pragma oss taskwait
    assert(c == -1 && "'c' has an unexepected value\n");

    for (i = 0; i < 4; ++i)
        assert(v[i] == -1 && "'v[i]' has an unexepected value\n");

    for (i = 0; i < N; ++i)
        assert(vla[i] == -1 && "'vla[i]' has an unexepected value\n");

    for (i = 0; i < N; ++i)
        for (j = 0; j < N; ++j)
            assert(vla2[i][j] == -1 && "vla2[i][j] has an unexepected value\n");
}
