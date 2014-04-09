/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
template <typename T>
struct One
{
    typedef T type;
};

template <typename T>
struct Two
{
    typedef typename T::type type;
};

template <typename T>
constexpr T f(T t)
{
    typedef One<T> A_T;
    typedef typename Two<A_T>::type D_A_T;

    return D_A_T(t) + 1;
}

void g()
{
    static_assert(f(4UL) == 5UL, "Should be 5");
}
