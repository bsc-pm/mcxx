/*
<testinfo>
test_generator=config/mercurium-fe-only
test_compile_fail=yes
</testinfo>
*/

template <typename T>
struct A
{
    typedef typename T::err err;
    typedef typename T::ok ok;
};

struct B
{
    typedef int ok;
};

template <typename T>
A<T> f(T, float);
template <typename T>
int f(T, int);

void g()
{
    B b;
    B* m;
    f(b, 0.0f);
}
