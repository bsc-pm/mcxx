struct A
{
    void f(int t)
    {
#pragma hlt outline packed
        {
            t = 3;
        }
    }
};
