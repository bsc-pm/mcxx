/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/

#include <stdio.h>

int main(int argc, char *argv[])
{
#pragma omp parallel
    {
        fprintf(stderr, "%s\n", __PRETTY_FUNCTION__);
    }
#pragma omp task
    {
        fprintf(stderr, "%s\n", __PRETTY_FUNCTION__);
    }

#pragma omp taskwait

    return 0;
}
