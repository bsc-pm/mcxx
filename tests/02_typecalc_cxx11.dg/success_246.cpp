/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/

// This test doesn't compile with GCC 4.X and previous versions
#if defined(__GNUC__)
#if __GNUC__ > 4
struct A
{
    int x, y, z;
    constexpr A(int x, int y, int z) : x(x), y(y), z(z) { }
};

template <int ...N>
struct C { };

struct B : A
{
    constexpr B(int x) : B(x, C<1, 2, 3>()) {}

    template <int ...N>
    constexpr B(int x, C<N...>) : A{ x + N ... } { }
};

constexpr B b(3);

static_assert(b.x == 4, "");
static_assert(b.y == 5, "");
static_assert(b.z == 6, "");
#endif
#endif
