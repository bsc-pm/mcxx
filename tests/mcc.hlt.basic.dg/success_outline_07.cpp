struct A
{
    template <typename _T>
    void f(_T t)
    {
#pragma hlt outline
        {
            t = 3;
        }
    }
};
