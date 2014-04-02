/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct B { };

struct A
{
    A(const B&);
    operator bool();
};


void f(B &b)
{
    if (A a = b)
    {
    }
}
