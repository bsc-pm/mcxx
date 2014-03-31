/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

namespace Foo
{
    template <typename T>
        struct B
        {
            void foo();
        };

}

void f(void)
{
    typedef int S;
    ::Foo::B<S> b;

    b.foo();
}

void g(void)
{
    typedef int S2;
    ::Foo::B<S2> b2;

    b2.foo();
}
