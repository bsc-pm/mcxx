/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename ...T>
struct A
{
    template <T ... N>
        struct B
        {
            void foo(T... t);
        };
};

void f()
{
    A<int, long>::B<1, 2> d;

    d.foo(3, 4);
}
