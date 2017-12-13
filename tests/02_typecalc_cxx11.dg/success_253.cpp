/*
<testinfo>
test_generator="config/mercurium-cxx11"
test_nolink=yes
</testinfo>
*/
namespace Test1
{
    constexpr bool one(int x)
    {
        return x;
    }

    template <int N>
    void bar()
    {
        static_assert(one(N), "");
    }

    void foo()
    {
        bar<1>();
    }
}

namespace Test2
{
    template < typename T>
    constexpr bool one()
    {
        return false;
    }

    template <>
    constexpr bool one<int>()
    {
        return true;
    }

    template <typename T>
    void bar()
    {
        static_assert(one<T>(), "");
    }

    void foo()
    {
        bar<int>();
    }
}
