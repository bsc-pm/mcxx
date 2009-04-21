int g(int n);

void f(void)
{
    int a[100], b[100], c[100];

    int i;

#pragma hlt fusion
    {
        for (i = 0; i < 100; i++)
        {
            a[i] = 0;
        }
        c[3] = 4;
        for (i = 0; i < 100; i++)
        {
            b[i] = 0;
        }
    }
}
