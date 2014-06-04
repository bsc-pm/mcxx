/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T, T (*F)()>
struct A
{
    void foo()
    {
        F();
    }
};

int f();

void g()
{
    A<int, f> a;

    a.foo();
}
