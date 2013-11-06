/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <bool B>
struct A;

template <>
struct A<true>
{
    static int *x;
};

template <>
struct A<false>
{
    static double *x;
};

void f(void)
{
    const bool b = true;

    int *pi;
    A<b>::x = pi;

    double *pd;
    A<!b>::x = pd;
}
