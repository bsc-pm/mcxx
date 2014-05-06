/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename T>
struct Foo
{
    constexpr static int foo()
    {
        return sizeof(T);
    }
};

template <typename T>
constexpr int foo()
{
    return Foo<T>::foo();
}

template <int N>
struct A
{
};

typedef A< foo<double>() > B;
typedef A< 8 > B;
