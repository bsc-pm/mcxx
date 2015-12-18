/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct A
{
    int x;
    constexpr A(int n) : x(n) { }
};

constexpr A a{1};
static_assert(a.x == 1, "");

struct B : A
{
    int y;
    constexpr B(int m) : A(m+1), y(m) { }
};

constexpr B b(1);
constexpr const A& rb = b;
static_assert(rb.x == 2, "");

constexpr B b1{1};
constexpr const A& rb1 = b1;
static_assert(rb1.x == 2, "");
