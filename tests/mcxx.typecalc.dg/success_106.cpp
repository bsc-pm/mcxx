template <typename _T, typename _Q>
struct A
{
    typedef _T K1;
    template <bool _K>
        K1* f(K1)
        {
        }

    void g();
};

template <typename _T, typename _Q>
void A<_T, _Q>::g()
{
    int i;
    i = f<true>(3);
}

void h1(int*)
{
}

void h2(float*)
{
}

void m()
{
    A<int, float> c;
    A<float, float> d;

    h1(c.f<true>(3));
    h2(d.f<true>(4.3f));
}
