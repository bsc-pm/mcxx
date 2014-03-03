/*
<testinfo>
test_generator="config/mercurium-cxx11 run"
</testinfo>
*/

#include <stdlib.h>

template <int Q>
struct C {
    enum { value = Q * 3 };
};

template <int T , int P = C<T>::value + 1>
struct A { enum { value = P }; };

static int sum()
{
    return 0;
}

template <int N, int ...W>
static int sum(A<N>& h, A<W>&...t)
{
    return h.value + sum(t...);
}

template <int ...S>
struct B
{
    int f(A<S>&... a) { return sum(a...); }
};

int main(int argc, char *argv[])
{
    A<10> a1; // A<10, 31>
    if (a1.value != 31)
    {
        abort();
    }

    A<20> a2; // A<20, 61>
    if (a2.value != 61)
    {
        abort();
    }

    B<10, 20> b;
    if (b.f(a1, a2) != 92)
    {
        abort();
    }

    return 0;
}
