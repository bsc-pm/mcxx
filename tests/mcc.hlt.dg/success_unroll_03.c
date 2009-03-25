void f(void)
{
    int i;
    int a[100];
#pragma hlt unroll(11)
    for (i = 0; i < 100; i++)
    {
        a[i] = i;
    }
}
