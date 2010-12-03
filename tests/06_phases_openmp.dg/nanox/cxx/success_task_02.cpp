/*
<testinfo>
test_generator=config/mercurium-nanox
</testinfo>
*/

template <typename T>
struct A
{
    T e;
};

struct B
{
    int d;
};

void f(void)
{
    A<B> c;
#pragma omp task
    {
        c.e.d = 3;
    }
}
