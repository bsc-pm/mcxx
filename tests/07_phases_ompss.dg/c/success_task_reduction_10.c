/*
<testinfo>
test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
</testinfo>
*/

/*
 * This test ensures the initializers/combiners generated for a scalar type
 * aren't reused for the array type counterpart.
 *
 * See #2898
 */

#include <assert.h>

int main()
{
    int x = 0;
    int xA[10] = {};

    #pragma omp task \
        reduction(+: x) \
        reduction(+: xA)
    {
        x++;

        for (int i = 0; i < 10; ++i)
        {
            xA[i]++;
        }
    }

    #pragma omp taskwait

    assert(xA[1] == 1);
}
