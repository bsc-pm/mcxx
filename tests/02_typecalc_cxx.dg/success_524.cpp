/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace A
{
    template <typename T>
        struct Foo;
};

template <typename T>
struct A::Foo
{
    Foo(int);
};

namespace A
{
    template <typename T>
        Foo<T>::Foo(int)
        {
        }
};
