/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/

struct A
{
    A(float);
};

struct B
{
    B(A);
    B(...);
    B(const B&);
};

B b(1.2f);
