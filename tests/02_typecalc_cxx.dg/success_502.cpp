/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A
{
    A();

    bool operator !=(const A&);
};

void f();

void g(int *n, A &s)
{
    // If we parenthesize (::A()) then g++ thinks we are casting a label addres...
    s != ::A() && n != 0 ? static_cast<void>(0) : f();
}
