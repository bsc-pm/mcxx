/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct Base
{
    int x;
    constexpr Base(int x) : x(x) { }
    constexpr Base(const Base&) = default;
};

void f1()
{
    constexpr Base a(3);
    constexpr Base b(a);

    static_assert(a.x == 3, "");
    static_assert(b.x == 3, "");
}

struct Derived : Base
{
    int y;
    constexpr Derived(int x) : Base(x)
                               , y(x+1)
    { }
    constexpr Derived(const Derived&) = default;
};

void f2()
{
    constexpr Derived a(3);

    static_assert(a.y == 4, "");
    static_assert(a.x == 3, "");

    constexpr Derived b(a);

    static_assert(b.x == 3, "");
    static_assert(b.y == 4, "");
}
