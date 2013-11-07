/*
<testinfo>
test_generator="config/mercurium-cxx11"
</testinfo>
*/

template <typename T, typename S>
struct A {
    void f(T*, S*);
};

template <typename ...T>
struct B
{
    A<T...> a;
};

template <typename ...T>
struct C
{
    template <typename ...S>
        struct D
        {
            A<T..., S...> a;
            A<S..., T...> b;
        };
};

void f()
{
    B<int, float> b;

    int *pi;
    float *pf;

    b.a.f(pi, pf);

    C<int>::D<float> d;

    d.a.f(pi, pf);
    d.b.f(pf, pi);
}
