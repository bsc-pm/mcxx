/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

struct A
{
    typedef unsigned long int_type;
};

template <typename T>
void f(T, typename T::int_type);

void g()
{
    A a;

    f(a, 1UL);
    f(a, 1);
}
