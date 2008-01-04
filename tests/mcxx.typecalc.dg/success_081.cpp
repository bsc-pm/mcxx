template <typename _T, typename _Q>
struct A
{
    typedef _T K1;
    template <bool _K>
        void f(K1)
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
