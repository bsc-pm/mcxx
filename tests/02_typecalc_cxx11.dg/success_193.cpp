/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct A
{
    int x;
};

struct B
{
    A a;

    constexpr B(int x = 3) : a({1}) { }
};

void f()
{
    constexpr B b;
    static_assert(b.a.x == 1, "");

    constexpr B b1{};
    static_assert(b1.a.x == 1, "");

    constexpr B b2{4};
    static_assert(b1.a.x == 1, "");
}
