void f(void)
{
    const int M = 10;
    int a[100];

#pragma hlt unroll(M + 1)
    for (int i = 0; i < 100; i++)
    {
        a[i] = i;
    }
}
