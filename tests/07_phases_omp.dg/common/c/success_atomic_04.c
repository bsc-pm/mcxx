/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/

#if !defined(__ICC) || (__ICC >= 1700)
#include <assert.h>
#include <omp.h>

#define epsilon 0.00025

double q = 1.0;
int num_iterations = 1000;

int main()
{
    double res = 0;

    #pragma omp parallel
    {
        for(int i = 0; i < num_iterations; ++i)
        {
            #pragma omp atomic
            res += q*(i + 1);
        }
    }

    double validate = omp_get_max_threads()*(num_iterations + 1)*num_iterations/2;
    assert(res <= validate + epsilon && res >= validate - epsilon);

    return 0;
}
#else
// ICC <17.0 seems to have precision issues with exact real representation
int main()
{
    return 0;
}
#endif
