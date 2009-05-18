struct A
{
    template <typename _T>
    void f(_T t);
};

template <typename _Q>
void A::f(_Q q)
{
#pragma hlt outline
    {
        q = 0;
    }
}
