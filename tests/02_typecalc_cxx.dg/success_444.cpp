/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/

struct A { template <typename T> void f(); };
struct B : A { };
struct C : A { };
struct D : B, C { };

void g()
{
    D d;

    d.B::f<int>();
    d.::B::f<int>();
}

