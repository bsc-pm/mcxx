/*
<testinfo>
test_generator=config/mercurium-omp

test_compile_fail_nanox_plain=yes
test_compile_faulty_nanox_plain=yes
</testinfo>
*/
#include <stdlib.h>

namespace A {
#pragma omp declare reduction(myop: int: _out = _in + _out)
}

#pragma omp declare reduction(myop: int: _out = _in * _out)

int main(int argc, char* argv[])
{
    int x;
    using namespace A;
    #pragma omp parallel reduction(A::myop : x)
    x = rand();

    printf("The random result is '%d'", x);

    return 0;
}
