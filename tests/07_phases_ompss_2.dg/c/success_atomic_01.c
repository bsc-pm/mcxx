/*
<testinfo>
test_generator="config/mercurium-ompss-2"
test_ENV=NANOS6_SCHEDULER=naive
test_ignore=yes
test_ignore_reason="task for is not supported anymore"
</testinfo>
*/

#include <assert.h>

#define epsilon 0.00025

double q = 1.0;
int num_iterations = 1000;

int main()
{
    double res = 0;

    #pragma oss task for shared(res)
    for(int i = 0; i < num_iterations; ++i)
    {
        #pragma oss atomic
        res += q*(i + 1);
    }

    #pragma oss taskwait

    double validate = (num_iterations + 1)*num_iterations/2;
    assert(res <= validate + epsilon && res >= validate - epsilon);

    return 0;
}
