void f(void)
{
    int a[100], b[100];

    int i;
#pragma hlt distribute
    for (i = 0; i < 100; i++)
    {
        a[i] = i * 1;
        b[i] = 0;
    }
}
