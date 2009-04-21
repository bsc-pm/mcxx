void f(void)
{
    int a[100], b[100];

#pragma hlt distribute
    for (int i = 0; i < 100; i++)
    {
        {
            a[i] = i * 1;
            b[i] = 0;
        }
    }
}
