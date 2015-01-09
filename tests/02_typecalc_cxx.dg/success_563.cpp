/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A
{
    typedef int (*Foo)(float);

    operator Foo();
};

void g()
{
    int (*p)(float) = A();
}
