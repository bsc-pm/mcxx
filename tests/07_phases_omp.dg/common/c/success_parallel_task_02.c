/*
<testinfo>
test_generator=config/mercurium-omp
</testinfo>
*/
#include<assert.h>

const char *p;
void foo()
{
    p = &__PRETTY_FUNCTION__[0];
    #pragma omp parallel
    {
        assert(p == &__PRETTY_FUNCTION__[0]);
        #pragma omp single
        #pragma omp task
        {
            assert(p == &__PRETTY_FUNCTION__[0]);
        }
    }
}

int main() {
    foo();
}
