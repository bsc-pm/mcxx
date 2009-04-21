void f(void)
{
    int a[100], b[100];

    int t;
#pragma hlt distribute expand(t)
    for (int i = 0; i < 100; i++)
    {
        t = a[i] + 1;
        b[i] = t * 2;
    }
}
