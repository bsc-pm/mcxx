/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

constexpr const int (&a)[3] = {1, 2, 3};
static_assert(a[1] == 2, "");

constexpr const char (&c)[5] = "hola";
static_assert(c[1] == 'o', "");
