/*
<testinfo>
test_generator=config/mercurium-ompss
test_CXXFLAGS="--no-copy-deps"
</testinfo>
*/

#include <unistd.h>
#include <stdlib.h>

struct C
{
    int x;
    int y[10];
};

int main(int argc, char *argv[])
{
    C v[10];
    int i;

    for (i = 0; i < 20; i++)
    {
        v[1].x = 3;
#pragma omp task inout(v[1].x)
        {
            v[1].x++;
            usleep(500);
        }

#pragma omp task inout(v[1].x)
        {
            if (v[1].x != 4)
                abort();
        }

        v[2].y[3] = 4;
#pragma omp task inout(v[2].y[3])
        {
            v[2].y[3]++;
            usleep(500);
        }

#pragma omp task inout(v[2].y[3])
        {
            if (v[2].y[3] != 5)
                abort();
        }

#pragma omp taskwait
    }
    return 0;
}
