void f(int i, int j)
{
#pragma hlt block factors(20)
    for (i = 0; i < 100 && j > 20; i++, j--)
    {
    }
}
