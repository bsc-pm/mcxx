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
    B(int n) : A(n) { }
    B() : B(2) { }
};
