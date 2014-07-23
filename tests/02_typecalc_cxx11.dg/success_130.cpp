/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct A
{
    int n;
    constexpr A(int m1, int m2) : n(m1 + m2) { }
};

struct B : A
{
    using A::A;
};

void f()
{
    static_assert(B(3, 4).n == 7, "Invalid value");
}
