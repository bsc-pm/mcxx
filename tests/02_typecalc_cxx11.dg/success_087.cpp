/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
template <typename T>
struct B;

template <typename T>
using Alias = typename B<T>::Foo;

template <typename T>
struct A
{
    typedef typename Alias<T>::Type Type;
};

template <typename T>
struct B
{
    struct Foo
    {
        typedef T Type;
    };
};

typedef int Test1;
typedef A<int>::Type Test1;

typedef float Test2;
typedef A<float>::Type Test2;
