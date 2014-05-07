/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A
{

    friend void foo(A a[3]);

    void bar();
    void quux()
    {
        foo(this);
    }
};

void A::bar()
{
    foo(this);
}

void foo(A x[3]) { }

namespace B
{
    void foo();
    void g()
    {
        ::foo(0);
        foo();
    }
}
