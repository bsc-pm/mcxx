/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct A
{
    void foo(float);
};

struct B : A
{
    using A::foo;
    void foo(float);
};

struct C : A
{
    using A::foo;
};
