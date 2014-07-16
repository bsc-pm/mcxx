/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct A
{
    A(float);
    int n;
    template <typename T>
    A(T m1, T m2 = sizeof(T)) : n(m1 + m2) { }
};

struct B : A
{
    using A::A;
};

void f()
{
    B b(1, 2);
}
