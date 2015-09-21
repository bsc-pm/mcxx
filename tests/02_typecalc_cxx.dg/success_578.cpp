/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct A {
    int memb;
};

struct B
{
    A*& operator->();
};

void f(void)
{
    B b;

    b->memb;
}
