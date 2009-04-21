void f(void)
{
    int a[100];
#pragma hlt unroll(11)
    for (int i = 0; i < 100; i++)
    {
        a[i] = i;
    }
}
