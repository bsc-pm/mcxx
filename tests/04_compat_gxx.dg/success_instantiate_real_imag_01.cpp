/*
<testinfo>
test_generator=config/mercurium-fe-only
test_CXXFLAGS="-std=gnu++11 --pp=off"
</testinfo>
*/

template <typename T>
constexpr T foo(_Complex float a)
{
    return __real__ a + __imag__ a;
}

constexpr _Complex float b = 1.0f + 2.0fi;

void f()
{
    static_assert(foo<float>(1.0f + 2.0fi) == 3.0f, "");
    static_assert((__real__ b + __imag__ b) == 3.0f, "");
}
