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
    constexpr A(T m1, T m2 = sizeof(T)) : n(m1 + m2) { }
};

struct B : A
{
    using A::A;
};

void f()
{
    static_assert(B(3).n == 7, "Invalid value");
    static_assert(B(3, 4).n == 7, "Invalid value");
}
