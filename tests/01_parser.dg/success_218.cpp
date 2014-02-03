/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct B;
struct A
{
    void f1(A a[]) { }
    void g1(B b[]) { }

    void f2(A a[]);
    void g2(B b[]);
};

void m(B b[])
{
}
