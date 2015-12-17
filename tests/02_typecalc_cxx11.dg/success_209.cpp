/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

static_assert(static_cast< int&&>(3) == 3, "");
static_assert(static_cast<const int&&>(3) == 3, "");
static_assert(static_cast< int&&>(3) + 1 == 4, "");
static_assert(static_cast<const int&&>(3) + 1 == 4, "");

constexpr int x = 3;
static_assert(static_cast<const int&&>(x) == 3, "");
static_assert(static_cast<const int&&>(x + 1) == 4, "");
static_assert(static_cast<const int&&>(x) + 1 == 4, "");
