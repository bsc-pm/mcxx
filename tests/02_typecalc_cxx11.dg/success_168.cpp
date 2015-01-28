/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/
struct B { B(int); };

template <typename ...T>
struct A
{
    void foo(T&&...) { }

    template <typename S>
        void foo(B&&, S&&, T&&...) { }

    template <typename S>
        void foo(S&&, T&&...) { }
};

void g()
{
    typedef A<int, float, double> A_ifd;

    A_ifd a;

    a.foo(1, 2.3f, 4.5);
    a.foo('c', 1, 2.3f, 4.5);
    a.foo(B(1), 'c', 1, 2.3f, 4.5);
}
