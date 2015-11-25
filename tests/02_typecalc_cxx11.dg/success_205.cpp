/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

constexpr int a = 10;
constexpr const int &b = a;
static_assert(b == 10, "");
static_assert(&b == &a, "");

int a1 = 10;
int& b1 = a1;
static_assert(&b1 == &a1, "");

int a2 = 10;
int& b2 = a2;
static_assert(&a2 != &a1, "");
static_assert(&b2 != &a1, "");
static_assert(&b2 != &b1, "");

int &b3 = b2;
static_assert(&b3 == &b2, "");
static_assert(&b3 == &a2, "");
