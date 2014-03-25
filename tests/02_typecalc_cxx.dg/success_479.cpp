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
    typedef int const S;
    typedef volatile S M;

    typedef Foo::B<M> T2;
    typedef Foo::B<volatile const int> T2;
}
