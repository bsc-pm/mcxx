/*
<testinfo>
test_generator=config/mercurium-ss2omp
</testinfo>
*/

typedef
struct A_tag
{
    int a;
} A;

#pragma css task inout(a)
void f(A* a)
{
    a->a = a->a + 1;
}

void g(A* a)
{
    f(a);
}
