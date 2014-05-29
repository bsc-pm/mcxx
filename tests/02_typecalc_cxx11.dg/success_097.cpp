/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct B
{
    int r;

    constexpr B(int k) : r(k) { }

    constexpr int foo() { return r; }
};

struct A0
{
    typedef int T;
    T r;

    constexpr A0(T k) : r(k) { }
    constexpr B foo() { return B(r + 1); }
};

template <typename T>
struct A
{
    T r;

    constexpr A(T k) : r(k) { }
    constexpr B foo() { return B(r + 1); }
};

void g()
{
    static_assert(A0(3).foo().foo() == 4, "should be 4");
    static_assert(A<int>(3).foo().foo() == 4, "should be 4");
}
