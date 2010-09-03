/*
<testinfo>
test_generator=config/mercurium-omp

test_compile_fail_nanos4_plain=yes
test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes
</testinfo>
*/
#include <stdlib.h>

#pragma omp declare reduction(myop: int: _out = _in * _out)

namespace A {
#pragma omp declare reduction(myop: int: _out = _in + _out)
}

int main(int argc, char* argv[])
{
    int x;
    using namespace A;
    #pragma omp parallel reduction(A::myop : x)
    x = rand();

    return 0;
}
