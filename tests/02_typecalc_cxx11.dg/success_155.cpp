/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
template <int N, typename ...T>
struct A;

template <int N>
struct A<N>
{
};

template <int N, typename H, typename ...T>
struct A<N, H, T...> : A<N+1, T...>
{
};

A<0> a0;
A<0, int> a1;
A<0, int, float> a2;
