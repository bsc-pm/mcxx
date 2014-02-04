/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

namespace N1
{

    template <typename T>
        struct A
        {
        };

    template <typename T>
        using B = A<T>;

    template <typename S>
        struct C : B<S>
    {
    };

    template <typename T>
    void foo(C<T>&);
}

void g()
{
    N1::C<int> c;

    foo(c);
}
