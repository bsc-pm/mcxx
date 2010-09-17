/*
<testinfo>
test_generator=config/mercurium-omp

test_compile_fail_nanos4_plain=yes

test_compile_faulty_nanox_plain=yes
</testinfo>
*/

#include <stdlib.h>

struct A
{
};
#pragma omp declare reduction (foo: A : _out=_in) identity(constructor)

struct B:A {};
struct C:A {};

struct D:B,C {
};

int main (int argc, char* argv[])
{
    D d;
    int x;
    // D::B::A::udr::foo vs D::C::A::udr::foo ???
    #pragma omp parallel reduction (foo : d)
    x = rand();

    return 0;
}
