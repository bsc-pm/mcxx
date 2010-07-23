/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct A
{
    enum
    {
        Foo
    };
};

struct B : A
{
    using A::Foo;
};


template <typename _T>
struct C : _T
{
    using _T::Foo;
};

void m()
{
    B b;
    C<A> c;
}
