/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    void f(T t, T = A::foo)
    {
    }
    static T foo;
};

void g()
{
    A<float*> a;
    float *pf;

    a.f(pf);
}
