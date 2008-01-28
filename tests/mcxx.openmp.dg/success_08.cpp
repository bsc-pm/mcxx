template <typename _S>
struct A
{
    template <typename _T>
    void f(_T t);
};


template <typename _F>
template <typename _Q>
void A<_F>::f(_Q q)
{
#pragma omp parallel
    {
        q = 3;
    }
}
