/*
<testinfo>
test_generator="config/mercurium"
</testinfo>
*/

struct A { int x; };
struct B : A { int y1; };
struct C : A { int y2; };
struct D : B, C { int z; };

void f()
{
    D d;

    d.z;
    d.y1;
    d.y2;

    d.B::x;
    d.C::x;
}
