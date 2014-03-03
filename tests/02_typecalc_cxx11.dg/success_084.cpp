/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

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
