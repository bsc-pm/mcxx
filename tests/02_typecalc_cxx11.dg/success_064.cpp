/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

constexpr int f(int n) { return n + 1; }

template <int N>
struct A
{
    typedef float Type[N];
};

template <typename T, T a, T b>
struct B
{
    typedef typename A<f(a) + f(b) + 2>::Type AnotherType;
};

typedef B<int, 10, 20>::AnotherType M;
typedef float M[34];
