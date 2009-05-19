struct A
{
    void f(int t)
    {
#pragma hlt outline
        {
            t = 3;
        }
    }
};
