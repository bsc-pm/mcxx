/*
<testinfo>
test_generator=config/mercurium
test_CFLAGS="-flax-vector-conversions"
</testinfo>
*/

#ifdef __x86_64__
#include <xmmintrin.h>

void f(void)
{
    (__m64)1L;
    (long)(__m64){1, 2};
}
#endif
