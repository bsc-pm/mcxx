/*
<testinfo>
test_generator=config/mercurium-omp

test_compile_fail_nanos4_plain=yes

test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes

test_compile_fail_nanox_instrument=yes
test_compile_faulty_nanox_instrument=yes
</testinfo>
*/

#include <stdlib.h>

namespace A {
#pragma omp declare reduction(myop: int: _out = _in + _out)
}

int main(int argc, char* argv[])
{
    int x;
    #pragma omp parallel reduction(myop : x)
    x = rand();

    return 0;
}
