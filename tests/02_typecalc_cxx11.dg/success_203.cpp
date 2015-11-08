/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

constexpr int x = 10;
constexpr const int *px = &x;
static_assert(*px == 10, "");

constexpr int y[] = { 1, 2, 3 };
constexpr const int *py1 = y + 1;
static_assert(*py1 == 2, "");
