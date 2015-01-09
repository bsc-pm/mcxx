/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct A
{
    enum E { V = sizeof(T) };

    void f(E);
};


void g()
{
    A<int> a;

    a.f(A<int>::V);
}

