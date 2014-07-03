/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

constexpr int h(int& t)
{
    return t + 1;
}

constexpr int g(int&& t)
{
    return h(t);
}

void m()
{
    static_assert(g(0) == 1, "Invalid value");
}
