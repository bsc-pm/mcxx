/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

struct A
{
    A(const A&);
    A(A&&) = delete;
};

const A f();

void g()
{
    A b = f();
}
