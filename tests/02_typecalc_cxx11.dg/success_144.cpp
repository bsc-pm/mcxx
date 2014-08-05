/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct A
{
    A(const A&) = delete;
    A(A&&);
};

A f();

void g()
{
    A b = f();
}
