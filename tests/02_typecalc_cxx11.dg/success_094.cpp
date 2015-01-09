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
constexpr int k = foo<double>();

typedef A< k > B;
typedef A< 8 > B;
