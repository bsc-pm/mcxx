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

template <typename T>
struct A
{
    void foo(int n);
};

struct C
{
    int n;
    C() : n(0) { }
};
C e;

template <typename T>
void A<T>::foo(int n)
{
#pragma omp parallel shared(e) firstprivate(n)
    {
#pragma omp critical
        {
            e.n = n;
        }
    }
}

int main(int argc, char *argv[])
{
    A<int> a;

    a.foo(4);

    if (e.n != 4)
        abort();

    return 0;
}
