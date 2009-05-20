template <typename _T>
struct A
{
    void f(_T t);
};

template <typename _Q>
void A<_Q>::f(_Q q)
{
#pragma hlt outline packed
    {
        q = 0;
    }
}
