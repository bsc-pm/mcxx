/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct A
{
    A(int);
};

struct B : A
{
    using A::A;
};

void f()
{
    B b(3);
}
