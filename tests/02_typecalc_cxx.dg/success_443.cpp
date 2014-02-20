/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/

struct A { void f(); };
struct B : A { };
struct C : A { };
struct D : B, C { };

void g()
{
    D d;

    d.B::f();
}

