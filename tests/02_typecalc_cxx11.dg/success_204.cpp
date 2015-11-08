/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

constexpr int a = 10;
constexpr const int *pa1 = &a;
constexpr const int *pa2 = &a;

static_assert(pa1 == pa2, "");
static_assert(pa1 <= pa2, "");
static_assert(pa1 >= pa2, "");
static_assert(!(pa1 != pa2), "");

constexpr int b = 10;
constexpr const int *pb1 = &b;

static_assert(pa1 != pb1, "");

constexpr int c[] = { 1, 2, 3, 4 };

static_assert(&c[0] == &c[0], "");
static_assert(&c[0] < &c[1], "");
static_assert(&c[0] <= &c[1], "");
static_assert(&c[0] != &c[1], "");
static_assert(&c[1] > &c[0], "");
static_assert(&c[1] >= &c[0], "");
