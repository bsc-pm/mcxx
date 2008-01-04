template <typename _T>
struct A
{
    typedef typename _T::test1 test2;

    template <typename _Q>
        struct B
        {
            typedef typename test2::test3 test;
        };

    void f();
};

struct C
{
    typedef int test1;
};

void g()
{
    A<C> c;

    c.f();
}
