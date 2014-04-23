/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <int ...N1>
struct B;

template <>
struct B<>
{
    static constexpr int value = 0;
};

template <int N>
struct B<N>
{
    static constexpr int value = N;
};

template <int N, int ...R>
struct B<N, R...>
{
    static constexpr int value = N + B<R...>::value;
};

void f()
{
    constexpr int v = B<1, 2, 3, 4>::value;
    static_assert(v == 10,
             "Something is amiss");
}
