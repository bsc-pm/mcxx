void f(void)
{
    int i;
#pragma hlt unroll(11)
    for (i = 0; i < 100; i++)
    {
    }
}
