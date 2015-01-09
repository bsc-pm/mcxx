/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct A
{
    A() noexcept(true) = default;
    A(const A&);
};

void f()
{
    A a;
    A b(a);
}

A::A(const A&) = default;

