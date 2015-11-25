/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

constexpr int a[] = {0, 1, 2, 3, 4};
constexpr const int *p = a;

static_assert(a[2] == 2, "");
static_assert(p[2] == 2, "");

struct B
{
    int x, y;
};

constexpr B b{1, 2};
static_assert(b.y == 2, "");

struct C
{
    int z[2];
};

constexpr C c{{3,4}};
static_assert(c.z[1] == 4, "");

struct D
{
    int w, t;
};

constexpr D d[2] = { {1, 2}, {3, 4}};
static_assert(d[0].t == 2, "");
static_assert(d[1].t == 4, "");
