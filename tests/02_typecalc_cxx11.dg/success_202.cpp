/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

struct A
{
    int x, y;
    constexpr A(int x_, int y_) : x(x_), y(y_) { }

    constexpr int g(int k)
    {
        return x + y + k;
    }
};

static_assert(A(1,2).x == 1, "");
static_assert(A(1,2).y == 2, "");

constexpr A e(1,2);
static_assert(e.x == 1, "");
static_assert(e.y == 2, "");
static_assert(A(1,2).g(0) == 3, "");
static_assert(A(1,2).g(1) == 4, "");
