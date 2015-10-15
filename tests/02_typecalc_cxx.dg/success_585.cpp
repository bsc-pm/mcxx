/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/
struct A
{
    int x;
};

const A f1();
A g1();

enum E {
    V1, V2
};

const E f2();
E g2();

void f(int x)
{
    typedef const A K_A;
    typedef __typeof__(x ? f1() : g1()) K_A;

    typedef __typeof__(x ? f2() : g2()) E;
}
