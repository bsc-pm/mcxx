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

#pragma css task inout(a[10])
void f(A* a)
{
    for (int i = 0; i < 10; i++)
    {
        a[i].a = a[i].a + 1;
    }
}

void g()
{
    A a[10];
    int i;
    for (i = 0; i < 10; i++)
    {
        a[i].a = i;
    }

    f(a);
}
