/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
struct A
{
    T r;

    constexpr A(T k) : r(k) { }
    constexpr T foo1() { return r + 1; }
    constexpr T foo2() { return (*this).r + 1; }
    constexpr T foo3() { return this->r + 1; }
    constexpr T foo4() { return foo3(); }
    constexpr T foo5() { return this->foo4(); }
};

void h(int (&)[4]);

void g()
{
    constexpr A<int> b(3);
    static_assert(b.foo1() == 4, "Should be 4");
    static_assert(b.foo1() == b.foo2(), "Should be the same");
    static_assert(b.foo1() == b.foo3(), "Should be the same");
    static_assert(b.foo1() == b.foo4(), "Should be the same");
    static_assert(b.foo1() == b.foo5(), "Should be the same");

    {
        int c[b.foo1()];
        h(c);
    }
    {
        int c[b.foo2()];
        h(c);
    }
    {
        int c[b.foo3()];
        h(c);
    }
    {
        int c[b.foo4()];
        h(c);
    }
    {
        int c[b.foo5()];
        h(c);
    }
}
