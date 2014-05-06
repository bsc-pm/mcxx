/*
<testinfo>
test_generator=config/mercurium-cxx11
</testinfo>
*/

template <typename ...T1>
struct my_tuple { };

template <typename ...T2>
my_tuple<T2* ...> f(double* d, T2*... p_args)
{
};

template <typename ...T3>
struct B
{
    auto fun(double *d, T3... t) -> decltype(f<T3...>(d, (&t)...));
};

void g()
{
    B<int, float> c;
    double *d;

    typedef decltype(c.fun(d, 0, 0)) Foo;
    typedef my_tuple<int*, float*> Foo;
}
