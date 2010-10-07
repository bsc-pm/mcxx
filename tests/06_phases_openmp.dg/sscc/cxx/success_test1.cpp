/*
<testinfo>
test_generator=config/mercurium-ss2omp
</testinfo>
*/
struct A
{
    int a;
    A() : a(0) { }
};

#pragma css task inout(a)
void f(A* a)
{
    a->a = a->a + 1;
}

void g()
{
    A* a = new A;
    f(a);
}
