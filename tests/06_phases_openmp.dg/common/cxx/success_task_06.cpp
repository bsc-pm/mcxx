/*
<testinfo>
test_generator=config/mercurium-omp

test_compile_fail_nanos4_instrument=yes
test_compile_faulty_nanos4_instrument=yes
test_compile_fail_nanos4_plain=yes
test_compile_faulty_nanos4_plain=yes
</testinfo>
*/
#include <stdlib.h>

template <typename S>
struct A
{
template <typename T>
    void foo(int n);
};

struct C
{
    int n;
    C() : n(0) { }
};
C e;

template <typename W>
template <typename Q>
void A<W>::foo(int n)
{
#pragma omp task shared(e) firstprivate(n)
    {
        e.n = n;
    }
}

int main(int argc, char *argv[])
{
    A<int> a;

    a.foo<int>(4);

#pragma omp taskwait

    if (e.n != 4)
        abort();

    return 0;
}
