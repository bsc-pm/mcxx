/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct A
{
    int x, y;
    constexpr A() : x(1), y(2) { }
};

void f()
{
    constexpr A a[3];

    static_assert(a[0].x == 1, "");
    static_assert(a[1].x == 1, "");
    static_assert(a[2].x == 1, "");

    static_assert(a[0].y == 2, "");
    static_assert(a[1].y == 2, "");
    static_assert(a[2].y == 2, "");
}

struct B
{
    int x, y;
};

struct C
{
    B b;
    constexpr C() : b({3,4}) { }
};

void g()
{
    constexpr C c[3];

    static_assert(c[0].b.x == 3, "");
    static_assert(c[1].b.x == 3, "");
    static_assert(c[2].b.x == 3, "");

    static_assert(c[0].b.y == 4, "");
    static_assert(c[1].b.y == 4, "");
    static_assert(c[2].b.y == 4, "");
}
