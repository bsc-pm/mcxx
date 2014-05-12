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
    template <typename ...T4>
    auto fun(double *d, T4... t) -> decltype(f<T3...>(d, (&t)...));
};

void g()
{
    B<int, float> c;
    double *d;

    typedef my_tuple<int*, float*> Foo;
    typedef decltype(c.fun(d, int(), float())) Foo;
}
