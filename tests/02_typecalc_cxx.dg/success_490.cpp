/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
void f()
{
    double d1 = T();
    d1 += 1.0;

    double d2 = typename T::M();
    d2 += 1.0;
}

struct A
{
    typedef A M;
    operator double();
};

void g()
{
    f<A>();
}
