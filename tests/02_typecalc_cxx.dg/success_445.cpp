/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A
{
    int * f();
    template <typename T> T* g(T);
};

struct B : A
{
};

struct C : A
{
};

struct D : B, C
{
    template <typename T> void g1(T);
};

void h()
{
    D d;
    int *pi;
    pi = d.B::f();

    float *pf;
    pf = d.B::g(3.4f);
}
