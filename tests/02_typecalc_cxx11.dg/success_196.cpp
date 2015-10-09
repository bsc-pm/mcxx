/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T>
constexpr T f()
{
    return T();
}

void g()
{
    static_assert(f<int>() == 0, "");
    static_assert(f<float>() == 0.0f, "");
    static_assert(f<double>() == 0.0, "");
}
