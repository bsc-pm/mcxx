/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A
{
    int* f(int y) const;
};

struct B : A
{
    using A::f;
    float* f(int x);

    B();
};

void g()
{
    const B b;

    int *pi;
    pi = b.f(0);
}
