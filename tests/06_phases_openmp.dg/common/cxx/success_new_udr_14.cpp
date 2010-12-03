/*
<testinfo>
test_generator=config/mercurium-omp

# test_exec_fail_nanox_plain_1thread=yes
# test_exec_faulty_nanox_plain_1thread=yes
test_exec_fail_nanox_plain_2thread=yes
test_exec_faulty_nanox_plain_2thread=yes
test_exec_fail_nanox_plain_4thread=yes
test_exec_faulty_nanox_plain_4thread=yes

# test_exec_fail_nanox_instrument_1thread=yes
# test_exec_faulty_nanox_instrument_1thread=yes
test_exec_fail_nanox_instrument_2thread=yes
test_exec_faulty_nanox_instrument_2thread=yes
test_exec_fail_nanox_instrument_4thread=yes
test_exec_faulty_nanox_instrument_4thread=yes
</testinfo>
*/

#include <stdlib.h>

struct A
{
};

#pragma omp declare reduction (myop: A : _out=_in) identity(constructor)

struct B: A
{
};

int main(int argc, char* argv[])
{
    A b;
    int r;
    #pragma omp parallel reduction(myop: b)
    r = rand();

    printf("The random result is '%d'", r);

    return 0;
}
