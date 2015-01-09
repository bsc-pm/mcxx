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
struct  B
{
   auto mfun(double *d, T3... t) -> decltype(f<T3...>(d, (&t)...));
};

template <typename ...T3>
auto fun(double *d, T3... t) -> decltype(f<T3...>(d, (&t)...));

void g()
{
    typedef my_tuple<int*, float*> Foo;
    double *d;

    fun<int, float>(d, 0, 0);
    typedef decltype(fun<int, float>(d, 0, 0)) Foo;

    B<int, float> c;
    typedef decltype(c.mfun(d, 0, 0)) Foo;
}
