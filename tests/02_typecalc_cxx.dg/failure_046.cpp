/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/

struct A
{
    const int n_;
    A(int n) : n_(n) { }

    void f();
};

void A::f()
{
    int &m = n_;
}
